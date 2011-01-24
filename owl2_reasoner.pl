/* -*- Mode: Prolog -*- */

:- module(owl2_reasoner,
          [
	   initialize_reasoner/2,
	   initialize_reasoner/3,
	   reasoner_tell/2,
	   reasoner_tell_all/1,
	   reasoner_ask/3,
	   reasoner_ask/2,
           reasoner_ask/1,
	   reasoner_check_consistency/2,
           reasoner_cache_to_file/3
          ]).

:- use_module(owl2_model).

:- multifile owl2_reasoner:initialize_reasoner_hook/3.
:- multifile owl2_reasoner:reasoner_tell_hook/2.
:- multifile owl2_reasoner:reasoner_tell_all_hook/1.
:- multifile owl2_reasoner:reasoner_ask_hook/2. % (R,Q)
:- multifile owl2_reasoner:reasoner_ask_hook/3. % (R,Q,IsDirect)
:- multifile owl2_reasoner:reasoner_check_consistency_hook/2. % (R,IsConsistent)
:- multifile owl2_reasoner:reasoner_unsatisfiable_class_hook/2. % (R,IsConsistent)

:- multifile owl2_reasoner:cached_subClassOf/2.
:- multifile owl2_reasoner:cached_classAssertion/2.
:- multifile owl2_reasoner:cached_propertyAssertion/3.

%% initialize_reasoner(+Type,?Reasoner)
% see initialize_reasoner/2
initialize_reasoner(Type,Reasoner) :- 
	initialize_reasoner(Type,Reasoner,[]).

%% initialize_reasoner(+Type,?Reasoner,+Opts)
%
% Given a type of reasoner, generate a Reasoner object.
% This can be used in subsequent calls
%
% Example:
% ==
% initialize_reasoner(pellet,R,[])
% ==
initialize_reasoner(Type,Reasoner,Opts) :-
        load_handler(Type,Opts),
	initialize_reasoner_hook(Type,Reasoner,Opts),
        debug(reasoner,'Initialized reasoner: ~w',[Reasoner]),
        nb_setval(reasoner,Reasoner),
        !.
initialize_reasoner(null,null,_) :- !.
initialize_reasoner(cached(File),cached(File),_) :-
        !,
        load_files([File],[qcompile(large)]).
initialize_reasoner(Type,_,Opts) :- 
        throw(error(initialize_reasoner(Type,Opts))).
	
%% reasoner_tell(+Reasoner,+Axiom)
% feed an axiom to the reasoner
reasoner_tell(Reasoner,Axiom) :- 
	reasoner_tell_hook(Reasoner,Axiom).

%% reasoner_tell_all(+Reasoner)
reasoner_tell_all(Reasoner) :- 
	reasoner_tell_all_hook(Reasoner),
	!.
reasoner_tell_all(Reasoner) :- 
	forall(axiom(A),
	       reasoner_tell(Reasoner,A)).


%% reasoner_ask(+Reasoner,?Axiom,+IsDirect)
reasoner_ask(Reasoner,Axiom,IsDirect) :-  % experimental
        debug(reasoner,'Reasoner query: ~w',[Axiom]),
	reasoner_ask_hook(Reasoner,Axiom,IsDirect).

%% reasoner_ask(+Reasoner,?Axiom)
%
% in general Axiom should be a term using one of
% * subClassOf/2
% * classAssertion/2
% * propertyAssertion/3
%
% with arguments that are either ground atoms or
% variables.
% some reasoners may be able to give results for class expressions
% that contain variables.
% TODO - isConsistent
reasoner_ask(Reasoner,unsatisfiable(X)) :-
        reasoner_unsatisfiable_class(Reasoner,X).
reasoner_ask(Reasoner,reflexiveSubClassOf(X,Y)) :-
        reasoner_ask(Reasoner,subClassOf(X,Y)).
reasoner_ask(_,reflexiveSubClassOf(X,X)) :-
        class(X).
        
reasoner_ask(Reasoner,Axiom) :- 
        debug(reasoner,'Reasoner query: ~w',[Axiom]),
	reasoner_ask_hook(Reasoner,Axiom).
reasoner_ask(Reasoner,Axiom) :-
        nonvar(Reasoner),
        Reasoner=null,
        Axiom.
reasoner_ask(cached(_),subClassOf(A,B)) :- cached_subClassOf(A,B).
reasoner_ask(cached(_),classAssertion(A,B)) :- cached_classAssertion(A,B).
reasoner_ask(cached(_),propertyAssertion(A,B,C)) :- cached_propertyAssertion(A,B,C).

%% reasoner_ask(Axiom) is semidet
% as reasoner_ask/2, but uses the global variable 'reasoner' to determine
% which reasoner to use. If not set, queries using ALL available reasoners.
reasoner_ask(Axiom) :-
        nb_current(reasoner,Reasoner),
        !,
        reasoner_ask(Reasoner,Axiom).
reasoner_ask(Axiom) :-
        reasoner_ask(_,Axiom).
%reasoner_ask(_) :-
%        throw(error(reasoner_not_initialized)).



%% reasoner_check_consistency(+Reasoner)
%% reasoner_check_consistency(+Reasoner, ?IsConsistent:boolean)
reasoner_check_consistency(Reasoner) :- 
        reasoner_check_consistency(Reasoner,true).
reasoner_check_consistency(Reasoner,V) :- 
	reasoner_check_consistency_hook(Reasoner,V).

reasoner_unsatisfiable_class(Reasoner,C) :- 
	reasoner_unsatisfiable_class_hook(Reasoner,C).

%% reasoner_cache_to_file(+Reasoner,+AxiomTemplate,+File)
% caches the results of the AxiomTemplate query
reasoner_cache_to_file(Reasoner,AxiomTemplate,File) :-
        open(File,write,Out,[]),
        reasoner_cache_to_stream(Reasoner,AxiomTemplate,Out),
        close(Out).

reasoner_cache_to_stream(Reasoner,AxiomTemplate,Out) :-
        AxiomTemplate =.. [P|Args],
        atom_concat('cached_',P,P2),
        StoreTemplate =.. [P2|Args],
        forall(reasoner_ask(Reasoner,AxiomTemplate),
               write_precomputed_axiom(StoreTemplate,Out)).

write_precomputed_axiom(Ax,Out) :-
        format(Out,'~q.~n',[Ax]).



% --

load_handler(Type,_Opts) :-
        forall(reasoner_module(Type,Mod),
	       ensure_loaded(library(thea2/Mod))).

reasoner_module(pellet,owl2_java_owlapi).
reasoner_module(factpp,owl2_java_owlapi).
reasoner_module(hermit,owl2_java_owlapi).
reasoner_module(owlapi(_),owl2_java_owlapi).
reasoner_module(graph_reasoner,owl2_graph_reasoner).



/** <module> OWL Reasoning API 

  ---+ Synopsis

==
  :- use_module(bio(owl2_reasoner)).

  load_axioms('myont.owl'),
  initialize_reasoner(pellet,[],Reasoner),
  reasoner_tell(Reasoner),
  forall(reasoner_ask(Reasoner,subClassOf(X,Y)),
         writeln(X-Y)).
==

---+ Details

See Reasoning_using_Thea.txt

This module specifies which requests can be made of a reasoner. The actual implementation is in separate modules, including

   * owl2_rl_rules.pl - based on the OWL2 RL profile
   * owl2_java_owlapi.pl - use any reasoner supported by the java OWLAPI (requires JPL)
   * owl2_owllink.pl - external reasoners over HTTP

Note that you do not need to use the exported predicates of the above
modules - you can use the generic predicates defined here and plugin
your reasoner of choice.



---+ Hooks

You can provide a bridge module for your reasoner of choice. You should define the following:
  
* owl2_reasoner:initialize_reasoner_hook/3
* owl2_reasoner:reasoner_tell_hook/2
* owl2_reasoner:reasoner_tell_all_hook/1
* owl2_reasoner:reasoner_ask_hook/2
  
  
*/
