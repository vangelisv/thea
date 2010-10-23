/* -*- Mode: Prolog -*- */

:- module(owl2_lcs,
          [
           prepare_optimizations/1,
           class_pair_common_subsumer/3,
           class_pair_common_subsumer/4,
           class_pair_least_common_subsumer/3,
           class_pair_least_common_subsumer/4
          ]).

:- use_module(owl2_model).
:- use_module(owl2_reasoner).
:- use_module(owl2_graph_reasoner). % force this for now

%% prepare_optimizations(Opts)
% uses blip tabling module to cache reasoner calls
% (replace with reasoner caching?)
prepare_optimizations(_) :-
        ensure_loaded(library(thea2/util/memoization)),
        table_pred(is_subsumed_by/3),
        table_pred(is_equivalent/3),
        table_pred(reasoner_get_subsumer/3),
        graph_reasoner_memoize.

% HARCODE ALERT!!
% TODO - make this a hook
exclude('http://ontology.neuinfo.org/NIF/Backend/BIRNLex_annotation_properties.owl#_birnlex_limbo_class').
exclude('http://ontology.neuinfo.org/NIF/DigitalEntities/NIF-Investigation.owl#birnlex_2087').
exclude('http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-GrossAnatomy.owl#birnlex_6'). % anatomical entity
exclude('http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-GrossAnatomy.owl#birnlex_4'). % organ
exclude('http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-GrossAnatomy.owl#birnlex_16'). % regional part of organ
exclude('http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-GrossAnatomy.owl#birnlex_1167'). % Regional part of brain
exclude('http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-Molecule.owl#CHEBI_23367'). % molecular entity
exclude('http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-Molecule.owl#nlx_mol_20090303'). % molecular role
%%%%%%exclude(X) :- ontologyAxiom(O,class(X)),\+((subClassOf(X,Y),ontologyAxiom(O,class(Y)))). % exclude root classes DO NOT USE - multiple declarations
exclude(someValuesFrom('http://www.obofoundry.org/ro/ro.owl#has_proper_part',_)).
exclude(allValuesFrom('http://www.obofoundry.org/ro/ro.owl#has_proper_part',_)).
exclude(Class) :-
	atom(Class),
	sub_atom(Class,0,_,_,'http://www.ifomis.org').

exclude_tr(someValuesFrom(_,X)) :- exclude_tr(X).
exclude_tr(allValuesFrom(_,X)) :- exclude_tr(X).
exclude_tr(X) :- exclude(X).

%% reasoner_get_subsumer(+Class,?SuperClass)
% wrapper for reasoner_ask/2 over subClassOf/2 
reasoner_get_subsumer(C,P) :- 
        reasoner_get_subsumer(C,P,[]).
reasoner_get_subsumer(C,P,Opts) :-
        opts_reasoner(Opts,R),
        reasoner_ask(R,subClassOf(C,P)),
        \+ exclude_tr(P).

opts_reasoner(Opts,R) :- memberchk(reasoner(R),Opts),!.
opts_reasoner(_,graph_reasoner).

% TODO - consider renaming all preds class_pair ==> entity_pair (also works for individuals)

%% class_pair_common_subsumers(+ClassA,+ClassB,?CommonSubsumers:set,+Opts:list) is det
% CommonSubsumers is the set of classes and class expressions that subsume A and B
class_pair_common_subsumers(A,B,CSs,Opts) :-
        class_pair_common_subsumers(A,B,_,_,CSs,Opts).
class_pair_common_subsumers(A,B,APs,BPs,CSs,Opts) :-
        debug(owlsim,'finding cs(~w,~w) via ~w',[A,B,Opts]),
        setof(X,reasoner_get_subsumer(A,X,Opts),APs),
        setof(X,reasoner_get_subsumer(B,X,Opts),BPs),
        debug(owlsim_detail,'   finding intersection',[]),
        ord_intersection(APs,BPs,CSs).

%% class_pair_common_subsumer(+ClassA,+ClassB,?CommonSubsumer) is nondet
% see class_pair_common_subsumers/4
class_pair_common_subsumer(A,B,CS) :-
        class_pair_common_subsumer(A,B,CS,[]).

%% class_pair_common_subsumer(+ClassA,+ClassB,?CommonSubsumer,+Opts:list) is nondet
% see class_pair_common_subsumers/4
class_pair_common_subsumer(A,B,CS,Opts) :-
        class_pair_common_subsumers(A,B,CSs,Opts),
        member(CS,CSs).

%% class_pair_least_common_subsumer(+ClassA,+ClassB,?LeastCommonSubsumer) is nondet
% see class_pair_least_common_subsumer/4
class_pair_least_common_subsumer(A,B,LCS) :-
        class_pair_least_common_subsumer(A,B,LCS,[]).

%% class_pair_least_common_subsumer(+ClassA,+ClassB,?LeastCommonSubsumer,+Opts:list) is nondet
% true if LCS subsumes A and B, and there is no more specific class X that also subsumes A and B
class_pair_least_common_subsumer(A,B,LCS,Opts) :-
        member(basic(true),Opts),
        !,
        class_pair_least_common_subsumer_basic(A,B,LCS,Opts).
class_pair_least_common_subsumer(A,B,LCS,Opts) :-
        % default - use ext_combined algorithm
        class_pair_least_common_subsumer_ext_combined(A,B,LCS,Opts).

class_pair_least_common_subsumer_basic(A,B,LCS,Opts) :-
        class_pair_common_subsumers(A,B,CSs,Opts),
        member(LCS,CSs),
        opts_reasoner(Opts,R),
        % TODO - equivalence
        \+ ((member(X,CSs),
             X\=LCS,
             reasoner_ask(R,subClassOf(X,LCS)))).

% ----------------------------------------
% UNION
% ----------------------------------------

%% class_pair_common_subsumers_with_union(+ClassA,+ClassB,?CommonSubsumers:set,+Opts:list) is det
%
% generally not called directly - instead use class_pair_common_subsumer_ext/3, which uses this
%
% as class_pair_common_subsumers/4, but also includes union constructs in the set of expressions.
% this is only done in certain circumstances (we do not want "trivial" unions).
% currently, if the set of common subsumers include:
%  * X
%  * someValuesFrom(R,X)
% then the union of these will be returned in the set.
%
% the rationale is that these represent something in common.
%
% for example,
% ==
% LCS(pizza and has_part some jalapeno, habanero) = spicy_pepper or (has_part some spicy_pepper)
% ==
% if one person likes pizza with jalapeno and another person likes habanero then it seems
% reasonable to include "spicy pepper or has part some spicy pepper" in the set of things that
% these people like in common.
% (another way to handle this particular example is with a property chain, but we may not
%  have property chains defined in all situations)
class_pair_common_subsumers_with_union(A,B,CSs,Opts) :-
        setof(CS,class_pair_common_subsumer_with_union(A,B,CS,Opts),CSs).

class_pair_common_subsumer_with_union(A,B,CS,Opts) :-
        debug(owlsim_detail,'finding cs+u(~w,~w)',[A,B]),
        class_pair_common_subsumers(A,B,APs,BPs,_,Opts),
        member(AP,APs),
        member(BP,BPs),
        mk_union(AP,BP,CS).
class_pair_common_subsumer_with_union(A,B,CS,Opts) :-
        class_pair_common_subsumer(A,B,CS,Opts).

mk_union(X,someValuesFrom(R,X),unionOf([X,someValuesFrom(R,X)])).
mk_union(someValuesFrom(R,X),X,unionOf([X,someValuesFrom(R,X)])).
mk_union(someValuesFrom(R,X),someValuesFrom(R,Y),someValuesFrom(R,U)) :- mk_union(X,Y,U).

% ----------------------------------------
% EXTENDED - experimental
% ----------------------------------------
% finds class expressions

%% class_pair_common_subsumer_ext(+ClassA,+ClassB,?CommonSubsumerExpression,Opts)
%
% expression is either intersectionOf(...) | someValuesFrom(Prop,Expr) | unionOf(...)
%
% if both C1 and C2 are in the set of common subsumers for A and B,
% then C1^C2 is also a common subsumer. There may be other common subsumers
% that can be obtained by "threading" the class expressions together.
class_pair_common_subsumer_ext(A,B,CS_Out,Opts) :-
        % first enumerate standard common subsumers
        class_pair_common_subsumers_with_union(A,B,CSs,Opts),
        debug(owlsim_detail,'   union cs(~w, ~w) = ~w',[A,B,CSs]),

        % choose candidate pair of classes
        member(C1,CSs),         % eg r1 some (r2 some a)
        member(C2,CSs), % eg r1 some (r2 some b)
        C1 @< C2, % arbitrary direction
        debug(owlsim_detail,'   candidate intersection: ~w ^ ~w',[C1,C2]),

        \+ subsumes_or_subsumed_by(C1,C2,Opts),

        debug(owlsim_detail,'     NR - now try combining',[]),
        % now make r1 some (r2 some a and b)
        combine_expr_pair(C1,C2,CS,Opts),
        debug(owlsim_detail,'   candidate combined CS: ~w',[CS]),

        is_subsumed_by_chk(A,CS,Opts),
        is_subsumed_by_chk(B,CS,Opts),
        CS_Out=CS.
        %class_pair_common_subsumer_ext_chain(A,B,CSs,[C1,C2],CS,CS_Out,Opts).

/*
% 3-way; not very generic
xxxclass_pair_common_subsumer_ext(A,B,CS_Out,Opts) :-
        class_pair_common_subsumers_with_union(A,B,CSs,Opts),
        select(C1,CSs,CSs_r1),         % eg r1 some (r2 some a)
        select(C2,CSs_r1,CSs_r2),      % eg r1 some (r2 some b)
        member(C3,CSs_r2),                % eg r1 some (r2 some b)
        C1 @< C2, % arbitrary direction
        C2 @< C3,
        debug(owlsim_detail,'   candidate 3-way intersection: ~w ^ ~w ^ ~w',[C1,C2,C3]),
        \+ subsumes_or_subsumed_by(C1,C2,Opts),
        \+ subsumes_or_subsumed_by(C1,C3,Opts),
        \+ subsumes_or_subsumed_by(C2,C3,Opts),
        % now make r1 some (r2 some a and b)
        combine_expr_pair(C1,C2,CS_x,Opts),
        combine_expr_pair(CS_x,C3,CS,Opts),
        debug(owlsim_detail,'   candidate 3-way CS: ~w',[CS]),
        is_subsumed_by_chk(A,CS,Opts),
        is_subsumed_by_chk(B,CS,Opts),
        CS_Out=CS.
        %class_pair_common_subsumer_ext_chain(A,B,CSs,[C1,C2],CS,CS_Out,Opts).


% DOES NOT WORK YET
class_pair_common_subsumer_ext_chain(A,B,CSs,Used,CS_In,CS_Out,Opts) :-
        length(Used,NumUsed),
        NumUsed < 4,
        member(C3,CSs),
        \+ member(C3,Used),
        \+ subsumes_or_subsumed_by(C3,CS_In),
        (   NumUsed=3
        ->  trace
        ;   true),
        combine_expr_pair(C3,CS_In,CS_Next,Opts),
        is_subsumed_by_chk(A,CS_Next,Opts),
        is_subsumed_by_chk(B,CS_Next,Opts),
        class_pair_common_subsumer_ext_chain(A,B,CSs,[C3|Used],CS_Next,CS_Out,Opts).
class_pair_common_subsumer_ext_chain(_,_,_,_,_,CS,CS,Opts).
*/

%% class_pair_least_common_subsumer_ext(+ClassA,+ClassB,?CommonSubsumerExpression,Opts)
class_pair_least_common_subsumer_ext(A,B,CS_Simple,Opts) :-
        setof(CS,class_pair_common_subsumer_ext(A,B,CS,Opts),CS_Set), % CONSIDER MEMOIZING THIS?
        debug(owlsim_detail,'   calculated set of extended subsumers.',[]),
        member(CS,CS_Set),
        debug(owlsim_detail,'   candidate LCS: ~w',[CS]),
        % todo - include equivsets? just exclude structurally identical?
        \+ ((member(X,CS_Set),
             \+ is_equivalent(X,CS,Opts),
             is_subsumed_by_chk(X,CS,Opts),
             debug(foo,'  fail: is_subsumed_by_chk(~q,~q).',[X,CS]))),
        simplify_expr(CS,CS_Simple).


%% class_pair_least_common_subsumer_ext_combined(+ClassA,+ClassB,?CommonSubsumerExpression,Opts)
%
% as class_pair_common_subsumer_ext/4, but combines all subsumers into a single
% intersectionOf expression
class_pair_least_common_subsumer_ext_combined(A,B,CS_Combined,Opts) :-
        setof(CS,class_pair_least_common_subsumer_ext(A,B,CS,Opts),CS_Set),
        normalize_expr(intersectionOf(CS_Set),CS_Combined,Opts).

%% normalize_expr(+CE,?CE_Norm,Opts)
%
% generated CEs may have redundant or inconsistent structure
% TODO: full CNF?
normalize_expr(intersectionOf([X]),Y,Opts) :-
        !,
        normalize_expr(X,Y,Opts).
normalize_expr(intersectionOf(L1),Y,Opts) :-
        % example: (R some (A and B)) and (R some A)
        %  ==> (R some (A and B))
        select(X1,L1,L2),
        select(X2,L2,L3),
        reasoner_get_subsumer(X1,X2,Opts),
        !,
        normalize_expr(intersectionOf([X1|L3]),Y,Opts).
normalize_expr(intersectionOf(OuterL),Y,Opts) :-
        setof(X,intersection_member(X,OuterL),Xs),
        Xs\=OuterL,
        !,
        normalize_expr(intersectionOf(Xs),Y,Opts).
normalize_expr(X,X,_Opts).

% e.g. X=car L=[..., (.. and X and ..), ...]
intersection_member(X,L) :-
        member(E,L),
        E=intersectionOf(IL),
        member(X,IL).
intersection_member(E,L) :-
        member(E,L),
        E\=intersectionOf(_).

simplify_expr(C,C) :- atom(C),!.
simplify_expr(CE,C) :- equivalent_to(CE,C),atom(C),!.
simplify_expr(CE,CE2) :-
        CE =.. [F|Args],
        Args\=[],
        !,
        maplist(simplify_expr,Args,Args2),
        CE2 =.. [F|Args2].
simplify_expr(C,C).


%% combine_expr_pair(+CE1,+CE2,?CE_Subsumer,Opts)
%
% takes two linear chain expressions C1, C2 and generates class expressions
% that are the superclass of both C1 and C2.
%
% this includes not only the trivial intersectionOf expression "C1 and C2" but also
% non-trivial expressions obtained by "weaving" C1 and C2 together
%
% for example, imagine comparing people based on which movie directors they like.
% we have 3 object properties:
% * likes - fan to director
% * directs - director to film
% * about - film to subject matter
%
% e.g. likes some (directs some (about some foo)) +
%      likes some (directs some (about some bar))
%  =>  1. likes some (directs some (about some foo and bar))
%      2. likes some (directs some about some foo) and (directs some about some bar)
%      3. (likes some directs some about some foo) and (likes some directs some about some bar)
%
% The 3rd expression is the trivial expression, but 1. is obtained by intersecting the innermost
% parts of the input expressions.
%
% note this only works for input expressions that are "linear chains" - these can be obtained
% by following any class up the hierarchy (see the owl2_graph_reasoner algorithm)
combine_expr_pair(C1,C2,intersectionOf([C1,C2]),_).
combine_expr_pair(C1x,C2x,someValuesFrom(R,CE) ,Opts) :-
        C1x=someValuesFrom(R,C1),
        C2x=someValuesFrom(R,C2),
        \+ subsumes_or_subsumed_by(C1,C2,Opts),
        combine_expr_pair(C1,C2,CE,Opts).

/*
combine_expr_pair(C1x,C2x,CE,Opts) :- combine_as_union(C1x,C2x,CE,Opts).
combine_expr_pair(C1x,C2x,CE,Opts) :- combine_as_union(C2x,C1x,CE,Opts).


combine_as_union(C1x,C2,unionOf(C1x,C2),Opts) :-
        C1x = someValuesFrom(R,C2).
*/



% ----------------------------------------
% REASONING
% ----------------------------------------
% may be partially redundant with reasoner modules.
% however, we may want to use cached reasoner results,
% and we may have novel class expressions

% particular checks are made for union, intersection and
% restrictions - these are not implemented in the owl2_graph_reasoner
% TODO: stratify these

is_equivalent(C,C,_) :- !.
is_equivalent(C1,C2,Opts) :-
        is_subsumed_by_chk(C1,C2,Opts),
        is_subsumed_by_chk(C2,C1,Opts).

subsumes_or_subsumed_by(C1,C2,Opts) :-       is_subsumed_by_chk(C1,C2,Opts).
subsumes_or_subsumed_by(C1,C2,Opts) :-       is_subsumed_by_chk(C2,C1,Opts).

%% is_subsumed_by_chk(+ClassX,+ClassY,Opts) :- is semidet
% semideterministic version of is_subsumed_by/3
is_subsumed_by_chk(X,Y,Opts) :-
        !,
        is_subsumed_by(X,Y,Opts).

is_subsumed_by(X,X,_).
is_subsumed_by(X,Y,Opts) :-
        atom(X),
        equivalent_to(X,Expr),
        \+ atom(Expr), % avoid cycles - rewrite named classes as expressions only
        is_subsumed_by(Expr,Y,Opts).
is_subsumed_by(A,unionOf(L),Opts) :-
        member(X,L),
        is_subsumed_by(A,X,Opts).
is_subsumed_by(unionOf(L),B,Opts) :- % todo (A or B) < (A or B or C)
        forall(member(X,L),
               is_subsumed_by(X,B,Opts)).
is_subsumed_by(A,intersectionOf(L),Opts) :-
        forall(member(X,L),
               is_subsumed_by(A,X,Opts)).
is_subsumed_by(intersectionOf(L),B,Opts) :-
        member(X,L),
        is_subsumed_by(X,B,Opts).
is_subsumed_by(someValuesFrom(P,X),someValuesFrom(P,Y),Opts) :-
        is_subsumed_by(X,Y,Opts).
is_subsumed_by(someValuesFrom(P,X),someValuesFrom(P,Y),Opts) :-
        transitiveProperty(P),
        is_subsumed_by(X,someValuesFrom(P,Y),Opts).
is_subsumed_by(A,X,Opts) :-
        opts_reasoner(Opts,R),
        reasoner_ask(R,subClassOf(A,X1)),
        X1\=A, % non-reflexive
        X=X1.




        
