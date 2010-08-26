/* -*- Mode: Prolog -*- */

:- module(owl2_reasoner,
          [
	   initialize_reasoner/2,
	   initialize_reasoner/3,
	   reasoner_tell/2,
	   reasoner_tell_all/1,
	   reasoner_ask/2,
           reasoner_ask/1,
	   reasoner_check_consistency/1
          ]).

:- use_module(owl2_model).

:- multifile owl2_reasoner:initialize_reasoner_hook/3.
:- multifile owl2_reasoner:reasoner_tell_hook/2.
:- multifile owl2_reasoner:reasoner_tell_all_hook/1.
:- multifile owl2_reasoner:reasoner_ask_hook/2.

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
initialize_reasoner(null,null,_).
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

%% reasoner_ask(+Reasoner,?Axiom)
% in general Axiom should be one of
% * subClassOf/2
% * classAssertion/2
% * propertyAssertion/3
% with arguments that are either ground atoms or
% variables.
% some reasoners may be able to give results for class expressions
% that contain variables.
% TODO - isConsistent
reasoner_ask(Reasoner,reflexiveSubClassOf(X,Y)) :-
        reasoner_ask(Reasoner,subClassOf(X,Y)).
reasoner_ask(_,reflexiveSubClassOf(X,X)) :-
        class(X).
        
reasoner_ask(Reasoner,Axiom) :- 
        debug(reasoner,'Reasoner query: ~w',[Axiom]),
	reasoner_ask_hook(Reasoner,Axiom).
reasoner_ask(null,Axiom) :-
        Axiom.

reasoner_ask(Axiom) :-
        nb_current(reasoner,Reasoner),
        !,
        reasoner_ask(Reasoner,Axiom).
reasoner_ask(_) :-
        throw(error(reasoner_not_initialized)).



%% reasoner_check_consistency(+Reasoner)
reasoner_check_consistency(Reasoner) :- 
	reasoner_check_consistency_hook(Reasoner).


% --

load_handler(Type,_Opts) :-
        forall(reasoner_module(Type,Mod),
	       ensure_loaded(library(thea2/Mod))).

reasoner_module(pellet,owl2_java_owlapi).
reasoner_module(factpp,owl2_java_owlapi).
reasoner_module(hermit,owl2_java_owlapi).
reasoner_module(owlapi(_),owl2_java_owlapi).
reasoner_module(graph_reasoner,owl2_graph_reasoner).


/** <module> reasoner API - NOT YET IN USE

  ---+ Synopsis

  ==
  :- use_module(bio(owl2_reasoner)).

  load_axioms('myont.owl'),
  initialize_reasoner(pellet,Opts,Reasoner),
  reasoner_tell(Reasoner),
  forall(reasoner_ask(Reasoner,subClassOf(X,Y)),
         writeln(X-Y)).
==

---+ Details

See Reasoning_using_Thea.txt

  This module specifies which requests can be made of a reasoner. The actual implementation is in separate modules.

---+ Writing wrapping code for a new reasoner

You can provide a bridge module for your reasoner of choice. You should define the following:
  
* owl2_reasoner:initialize_reasoner_hook/3
* owl2_reasoner:reasoner_tell_hook/2
* owl2_reasoner:reasoner_tell_all_hook/1
* owl2_reasoner:reasoner_ask_hook/2
  
  
*/
