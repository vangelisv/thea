/* -*- Mode: Prolog -*- */

:- module(memoization,[
		   table_preds/0,
                   table_pred/1,
		   persistent_table_pred/2,
                   table_call/5,
                   clear_table_pred/1
                   ]).

:- module_transparent table_pred/1.
:- module_transparent table_call/5.
:- module_transparent persistent_table_pred/2.

:- dynamic persist_to/2. % track which predspecs are persisted to which files
:- dynamic persist_to_stream/2. % track which predspecs are persisted to which streams
:- dynamic cache_file/1. % track the full set of files to be loaded when ready
:- dynamic is_tabled/2.

:- multifile memoize_hook/1.

%% table_preds
% call table_pred/1 for all predicates for which
% memoization:memoize_hook/1 is defined
table_preds :-
	forall(memoize_hook(H),
	       table_pred(H)).


%% table_pred(+PredSpec)
%  @param Pred
%  a predicate in the current module of the form Name/Arity
%
%  Tabled predicates are rewritten to store/use a cache
%  
%  A predicate in a different module can be specified by including
%  the module as a prefix - for example
%
%  =|table_pred(mymod:mypred/3)|=
table_pred(M:P):-
	!,
        table_pred(P,M).
table_pred(P):-
	!,
        context_module(M),
        table_pred(P,M).
table_pred(P,M):-
        is_tabled(M,P),
        !,
        debug(tabling,'already tabled: ~w:~w',[M,P]).
table_pred(P,M):-
        debug(tabling,'context mod ~w, tabling ~w',[M,P]),
        P = F/Arity,
        functor(T,F,Arity),     % T is term with unground args
        % fetch all clauses for this predicate
        findall(T-B,clause(M:T,B),Clauses),
        % rewrite original clause
        debug(tabling,'abolishing: ~w',[M:P]),
        abolish(M:P),
        % create implementation clauses with new functors
        % in the head of the clause - body unchanged
        forall(member(H-B,Clauses),
               create_tabled_pred_impl(H,B,M)),
        % create a single wrapper clause that mimics the original
        % predicate and calls the implementation predicate
        create_tabled_pred_wrap(T,M),
        assert(is_tabled(M,P)),
	!.
table_pred(P,M):-
	throw(error(table_pred(P,M))).

%% persistent_table_pred(+PredSpec,+File)
%
% as table_pred/1, with persistence of the results of called predicates.
%
% For example, if subclassT/2 from ontol_db is persisted via:
%  =|persistent_table_pred(ontol_db:subclassT/2,'sc.pro')|=
% then, in the same prolog session, subclassT('GO:0006915',X) is called,
% the results of this will be saved in sc.pro.
% then, if the prolog session is ended, and a new one is resumed with
% the same cache, future calls will either reuse or append to this cache.
%
% PredSpec can be Module:Pred/Arity or just Pred/Arity. In the latter
% case, the context module is assumed.
% 
% note that currently if you cache M1:P/A and M2:P/A then the results
% will be combined. This may not be desirable, may change in future
% releases.
persistent_table_pred(MPA,File) :-
	debug(tabling,'peristing ~w to ~w',[MPA,File]),
	% module can be explicit or context module
	(   MPA=_:P/A
	->  true
	;   MPA=P/A),
	assert(persist_to(P/A,File)),
	% rewrite predicates; note this also sets the relevant caching
	% predicates to be dynamic
	table_pred(MPA),
	% open the caching stream ready for writing
	initiate_persistent_stream(P/A,File).

initiate_persistent_stream(P/A,File) :-
	debug(tabling,'  caching ~w in ~w',[P/A,File]),
	% if the cache file exists, then record it for consultation later.
	% if it does not exist, create it.
	% in both cases, prepare for writing.
	(   exists_file(File)
	->  assert(cache_file(File)),
	    open(File,append,IO,[])
	;   open(File,write,IO,[])),
	debug(tabling,'persisting: ~w to stream: ~w',[P/A,IO]),
	retractall(persist_to_stream(P/A,_)),
	assert(persist_to_stream(P/A,IO)).

% this is called whenever a memoized predicate is about to be called.
% it ensures that all cache files are loaded.
% the reason for delaying this is that we want to ensure all cached
% predicates are declared dynamic before consulting them.
ensure_cache_loaded :-
	setof(File,cache_file(File),Files),
	!,
	maplist(consult,Files),
	retractall(cache_file(_)).
ensure_cache_loaded.

% internal use only.
% currently only works if one predicate is memoized per file
persistent_retractall(_:T) :-
	functor(T,P,A),
	mapped_pred_name(P_orig,P),
	persist_to(P_orig/A,File),
	exists_file(File),
	!,
	format(user_error,'not tested!!~n',[]),
	delete_file(File).
persistent_retractall(_).

% internal use only - writes fact to cache stream.
% note this does NOT call assert/1 - see wrap_assert/1
persistent_assert(M:T) :-
	debug(tabling,'persisting assert(~w) to cache',[M:T]),
	functor(T,P,A),
	mapped_pred_name(P_orig,P),
	persist_to_stream(P_orig/A,IO),
	!,
	format(IO,'~q.~n',[M:T]),
	flush_output(IO).
persistent_assert(_).

% asserts in in-memory db, and in persistent storage, if relevant
wrap_assert(T) :-
	assert(T),
	persistent_assert(T).

%% clear_table_pred(+Pred)
%  @param Pred
%  a predicate in the current module of the form Name/Arity
%
%  removes all cached solutions for a predicate
% TODO: untabling
clear_table_pred(P):-
        context_module(M),
        clear_table_pred(P,M).
clear_table_pred(M:P):-
        clear_table_pred(P,M).
clear_table_pred(P,M):-
        P = F/Arity,
        cached_pred_name(F,Fm),
        called_pred_name(F,Fs),
        pred_to_unground_term(Fm/Arity,Tm),
        pred_to_unground_term(Fs/Arity,Ts),
        retractall(M:Tm),
        retractall(M:Ts),
	persistent_retractall(M:Tm),
        persistent_retractall(M:Ts).

% create_tabled_pred_impl(+Head,+Body,+Module)
%  Head is a term representing the head of a clause. The arguments
%  may be unground, ground, or variables unified with arguments in
%  the body
%
% this will assert a new clause FOO_tabled__(ARGS):- Body
% (Body is not rewritten)
create_tabled_pred_impl(H,B,M):-
        H =.. [F|As],
        tabled_pred_name(F,Ft),
        Ht =.. [Ft|As],
        ImplClause = (Ht :- B),
        debug(tabling,'asserting new clause: ~w',[ImplClause]),
        M:assert( ImplClause ).

% --HIDDEN PREDICATE NAMES --
%  memoization is implemented using 3 hidden predicates for each
%  tabled predicate. the predicates below map a predicate functor
%  to a new functor

% real implementation which rewritten clause calls
tabled_pred_name(N1,N2):-
        concat_atom([N1,'_tabled__'],N2).
% memo table
cached_pred_name(N1,N2):-
        concat_atom([N1,'_cached__'],N2).
% subsumption: what have we called already?
called_pred_name(N1,N2):-
        concat_atom([N1,'_called__'],N2).

mapped_pred_name(N1,N2) :- cached_pred_name(N1,N2).
mapped_pred_name(N1,N2) :- called_pred_name(N1,N2).


% create_tabled_pred_wrap(+PredTerm,+Module)
%  PredTerm is a term representing a predicate. The arguments
%  are always unground
create_tabled_pred_wrap(T,M):-
        T =.. [F|As],
        length(As,Arity),

        % the implementation predicate has the same body
        % as the original tabled predicate
        tabled_pred_name(F,Ft),
        Tt =.. [Ft|As],         % impl
        dynamic((M:Ft/Arity)),

        % cached results - rather than a generic cache/1 dynamic
        % predicate, we give each tabled pred FOO(..) its own cache
        % predicate FOO_cached__(...) [this is faster]
        cached_pred_name(F,Fm),
        Tm =.. [Fm|As],         % memo
        dynamic((M:Fm/Arity)),

        % subsumption check predicate - remember the arguments
        % every time a tabled pred is called - we can then use
        % subsumption checking to see if we can give back previously
        % computed solutions
        called_pred_name(F,Fs),
        Ts =.. [Fs|As],         % subsumed
        dynamic((M:Fs/Arity)),

        % single clause mimic original predicate, calling
        % a generic table_call predicate behind the scenes
        WrapClause = (M:T :- table_call(T,Tt,Tm,Ts,M)),
        debug(tabling,'asserting rewritten clause: ~w',[WrapClause]),
        assert( WrapClause ),
        % recompile abolished clause - now a wrapper to impl
        compile_predicates([M:F/Arity]).

%% table_call(+Goal,+GoalImpl,+GoalMemo,+GoalSubsumed,+Mod)
%
%  you should not need to call this directly
% 
%  the following conventions are used:
% 
% * GoalImpl = <origpred>_tabled__(...) -- rewritten original clause
% * GoalSubsumed = <origpred>_called__(...) -- record what has been cached
% * GoalMemo = <origpred>_cached__(...) -- database of facts
%
% note that in the 4 arguments here, all should have
% identical argument lists
table_call(G,GImpl,GMemo,GSub,M):-
	ensure_cache_loaded,
        % has G been called already? [remember args(G) == args(GSub)]
        \+ goal_subsumed(M:GSub),
        !,
        % seed call - dont use cache
        debug(tabling,'table_call [seed] ~w ~w',[GImpl,GSub]),

        % findall matching solutions using the implementation
        % clauses
        %  NOTE: as a side effect will remove dupes
        %  (this is an 'unusual' side-effect but necessary coz we
        %   do not record which cached results go with which calls)
        findall(GMemo,(M:GImpl,
                       (   M:GMemo
                       ->  true % don't assert duplicate
                       ;   wrap_assert(M:GMemo))),
                GMemos),
        debug(tabling,'seeded = ~w  // ~w',[GMemos,G]),

        % record the fact that G has been called
        % (remember: GSub has identical args to G)
        wrap_assert(M:GSub),

        % succeed with each solution in cache
        member(GMemo,GMemos).

% G subsumed by previous call - get answers from cache
table_call(G,_,GMemo,_,M):-
        debug(tabling,'table_call [memo] ~w ~w',[G,GMemo]),
        % GMemo is a cache predicate; if G is foo(a,b,X)
        % then GMemo will be foo_cache__(a,b,X)
        M:GMemo.

% +Goal
%  Goal will be of form mypred_called__/X
goal_subsumed(G):-
        numbervars(G,0,_),
        G.

% coverts eg foo/3 to foo(_,_,_)
pred_to_unground_term(Pred/Arity,Term):-
        !,
        unground_list(Arity,L),
        Term =.. [Pred|L].
% passthru
pred_to_unground_term(Term,Term).

% (+N,?L) det
% makes a list of length containing vars
unground_list(N,[]):- N =<0,!.
unground_list(N,[_|L]):-
        N1 is N-1,
        unground_list(N1,L).

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

% NOTE: tests must be run separately with -t option

unittest(load(go)=
      load_bioresource(go)/[]).

unittest(test(go,
            [_=load(go)],
            (   ensure_loaded(bio(ontol_db)),
                ensure_loaded(bio(tabling)),
                table_pred(ontol_db:parentT/2),
                writeln(testing),
                parentT('GO:0005634','GO:0005575'),
                % tabling avoids recomputing
                findall(PID,parentT('GO:0005634',PID),PIDs1),
                findall(PID,parentT('GO:0005634',PID),PIDs2),
                findall(PID,parentT('GO:0005634',PID),PIDs),
                writeln(pidsA=PIDs),
                table_pred(ontol_db:subclassT/2),
                setof(X,subclassT(X,'GO:0051704'),Xs), % MOP
                length(Xs,NumXs),
                writeln(subclasses(NumXs)),
                NumXs>700,
                PIDs=PIDs1,
                PIDs=PIDs2,
                nl
            ),
            true)).

unittest(test(go_untabled,
            [_=load(go)],
            (   ensure_loaded(bio(ontol_db)),
                ensure_loaded(bio(tabling)),
                % TODO: untabling
                %clear_table_pred(ontol_db:parentT/2),
                %clear_table_pred(ontol_db:subclassT/2),
                parentT('GO:0005634','GO:0005575'),
                findall(PID,parentT('GO:0005634',PID),_),
                findall(PID,parentT('GO:0005634',PID),_),
                findall(PID,parentT('GO:0005634',PID),PIDs),
                writeln(pidsA=PIDs),
                setof(X,subclassT(X,'GO:0051704'),Xs), % MOP
                length(Xs,NumXs),
                NumXs>700,
                writeln(subclasses(NumXs)),
                nl
            ),
            true)).

unittest(load(sofa)=
      load_biofile(obo,'sofa.obo')/[]).

unittest(test(sofa,
            [_=load(sofa)],
            (   ensure_loaded(bio(ontol_db)),
                ensure_loaded(bio(tabling)),
                table_pred(ontol_db:subclassT/2),
                findall(PID,subclassT('SO:0000704',PID),PIDs),
                writeln(pidsA=PIDs),
                findall(PID,subclassT('SO:0000704',PID),PIDs),
                writeln(pidsB=PIDs),
                nl
                ),
            member('SO:0000000',PIDs))).


/** <module> simple memoization of predicates

  ---+ Synopsis

  ==
  :- use_module(bio(tabling)).

  fib(X,V):-
        (   X<2
        ->  V=X
        ;   Xm1 is X-1,
            Xm2 is X-2,
            fib(Xm1,V1),
            fib(Xm2,V2),
            V is V1+V2).

  :- table_pred(fib/2).

  demo:-
    X=30,
    fib(X,V),
    format('fib(~w)=~w~n',[X,V]).
  
  ==

  ---+ Description

  Simple memoization of predicates, including basic subsumption checking

  Predicates can be marked for memoization with table_pred/1

  The first time a tabled pred is called, all possible solutions are
calculated and cached. The second time it is called (with the second call being subsumed by the first), the solutions are retrieved from the cache

  ---++ How it works

  Calling table_pred/1 will cause the target predicate to be
rewritten, and the original clauses to be asserted but with a
different name. The rewritten clauses will call table_call/5,
which will check to see if the call is subsumed by a previous call; if
so, use a dynamic cache to present all solutions; if not, call the
clauses containing the original code.

   ---+++ Example

  The example above will be rewritten as:
  
  ==
  fib(X,V) :-
    memoization:table_call(fib(X,V), fib_tabled__(X,V), fib_cached__(X,V), fib_called__(X,V), user).

  fib_tabled__(X,V) :-
     % original clause...

  % after call fib(4,X).
  fib_cached__(1, 1).
  fib_cached__(0, 0).
  fib_cached__(2, 1).
  fib_cached__(3, 2).
  fib_cached__(4, 3).

  fib_called__(1, _).
  fib_called__(0, _).
  fib_called__(2, _).
  fib_called__(3, _).
  fib_called__(4, _).

  %then we call:
  %?- fib(5,99).
  %false.

  % adds:
  fib_called__(5, 99).
  
  ==

  ---++ Limitations

  This implementation is severly limited compared to true tabling available in prologs such as XSB

  There also may be a problem with make/0
  
  ---+++ Speed

  memoization is implemented by asserting cached solutions to the
in-memory prolog database. In general tabled predicates will run MUCH
more slowly. The benefits are only tangible if you are doing a LOT of
recompution

  For example, a naive implementation of a fibonnacci function (see
above) would require repeatedly recomputing the fib(X) for the same X
multiple times. Memoized calls start to overtake un-memoized calls
after X=8, and is signigicantly faster after that

  Of course, the fib function could be recoded so that it does not
involve recomputation avoiding the need for memoization. This strategy
is not always practical for more complex predicates, hence the need
for this module

  ---+++ Inefficient

  if you call a nondeterministic tabled pred and only use the first
call, all solutions for your call will still be calculated
  
  ---+++ Simple subsumption strategy

  No attempt is made to record which call resulted in which cached
solutions.

  ---+++ Unsafe

  If a predicate is defined in terms of dynamic predicates, and those
dynamic predicates change, the cache will NOT be recomputed. The
programmer must be aware of these situations and explicitly call
clear_table_pred/1 if the cache becomes stale

Odd things also seen to happen with make/0

  ---+++ Not a real tabling strategy

  A real tabling strategy will allow you to write code like this:

  ==
  isaT(X,Y):- isaT(X,Z),isa(Z,Y).
  isaT(X,Y):- isa(X,Y).
  ==

  with isaT tabled, the program will not enter an infinite loop

  However, this module implements a much simpler strategy, and the two
predicates above must be reordered to ensure termination

  Note that this severaly limits the usefulness compared to real
tabling - for example, XSB tabling allows us to write left-recursive
DCGs (tabling + DCGs = Earleys algorithm). This is not possible with
this implementation, left-recursive DCGs will not terninate
  
**/
