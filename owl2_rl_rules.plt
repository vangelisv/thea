/* -*- Mode: Prolog -*- */

:- use_module(owl2_rl_rules).
:- use_module(owl2_io).
:- use_module(owl2_model).

show_all_entailments :-
	debug(test,'getting entailments',[]),
	get_tbox_entailments,
	forall(is_entailed(Axiom,Expl),
	       format('Axiom: ~w  [explanation: ~w]~n',[Axiom,Expl])).

% TEST ONTOLOGY
:- begin_tests(owl2_rl_rules_basic,
   [setup(init_axioms),
    cleanup(retract_all_axioms)]).


init_axioms :-
	load_axioms('testfiles/rl_rules_test.pl').

test(query) :-
	show_all_entailments.

:- end_tests(owl2_rl_rules_basic).

% WINE
:- begin_tests(owl2_rl_rules_wine,
   [setup(init_axioms),
    cleanup(retract_all_axioms)]).

init_axioms :-
	load_axioms('testfiles/wine.owl').

test(query) :-
	show_all_entailments.

:- end_tests(owl2_rl_rules_wine).

/** <module> tests for OWL2 rl_rules

---+ Synopsis

Command line:
  
==
swipl
?- [owl2_rl_rules].
?- load_test_files([]).
?- run_tests.
==

---+ Details

This is a test module for the module owl2_rl_rules

*/
