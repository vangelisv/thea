/* -*- Mode: Prolog -*- */

:- module(owl2_reasoner,
          [
	   initialize_reasoner/3,
	   reasoner_tell/1,
	   reasoner_tell/2,
	   reasoner_ask/2,
	   reasoner_check_consistency/1
          ]).

:- use_module(owl2_model).

%% initialize_reasoner(+Type,?Reasoner)
% see initialize_reasoner/2
initialize_reasoner(Type,Reasoner) :- 
	initialize_reasoner(Type,Reasoner,[]).

%% initialize_reasoner(+Type,?Reasoner,+Opts)
% Example:
% ==
% initialize_reasoner(pellet,R,[])
% ==
initialize_reasoner(Type,Reasoner,Opts) :- 
        load_handler(Type,Opts),
	initialize_reasoner_hook(Type,Reasoner,Opts).
	
%% reasoner_tell(+Reasoner,+Axiom)
reasoner_tell(Reasoner,Axiom) :- 
	reasoner_tell_hook(Reasoner,Axiom).

%% reasoner_tell_all(+Reasoner)
reasoner_tell_all(Reasoner,Axiom) :- 
	reasoner_tell_all_hook(Reasoner,Axiom).

%% reasoner_ask(+Reasoner,?Axiom)
reasoner_ask(Reasoner,Axiom) :- 
	reasoner_ask(Reasoner,Axiom).

%% reasoner_consistent(+Reasoner)

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




*/
