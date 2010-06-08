/* -*- Mode: Prolog -*- */

:- use_module(owl2_tbox_reasoner).
:- use_module(owl2_model).
:- use_module(owl2_io).

:- begin_tests(owl2_tbox_reasoner,[setup(init_axioms),cleanup(retract_all_axioms)]).

init_axioms :-
        retract_all_axioms,
        load_axioms('testfiles/rtest.owlpl').


test(all) :-
        forall(subClassOfRT(A,B),
	       writeln(A-B)).

test(all) :-
        forall(expected_SubClassOf(A,B),
	       holds(subClassOfRT(A,B))).

holds(X) :- \+ \+ X, !.
holds(X) :-
	format(user_error,'FAIL_TO_PROVE: ~w',[X]),
	fail.


expected_SubClassOf(a,a).
expected_SubClassOf(a,d).
expected_SubClassOf(a,someValuesFrom(partOf,someValuesFrom(hasPart,a))).
expected_SubClassOf(d,someValuesFrom(partOf,e)).
expected_SubClassOf(a,someValuesFrom(partOf,e)).
expected_SubClassOf(a,someValuesFrom(partOf,g)).
expected_SubClassOf(a,j). % intersection
expected_SubClassOf(a,someValuesFrom(partOf,k)). % intersection
expected_SubClassOf(a,someValuesFrom(partOf,i)). % transitivity

%expected_SubClassOf(auto_b,b). % equivalence and intersection - TODO


%test(lca) :-
%test(lca) :-
%        forall(property_assertion_least_common_ancestor(P,XI,YI,XC,YC),
%               writeln(lca(P,XI,YI,XC,YC))),
%        nl.



:- end_tests(owl2_tbox_reasoner).

/** <module> tests for OWL2 tbox_reasoner

---+ Synopsis

Command line:
  
==
swipl
?- [owl2_tbox_reasoner].
?- load_test_files([]).
?- run_tests.
==

---+ Details

This is a test module for the module owl2_tbox_reasoner

*/
