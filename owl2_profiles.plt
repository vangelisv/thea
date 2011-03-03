/* -*- Mode: Prolog -*- */

:- use_module(owl2_profiles).

:- begin_tests(owl2_profiles,[]).

%test(t1,[true(\+axiom_profile(Ax,Pr,true))]) :- violation(N,Ax,Pr).

test(x1, true( \+ axiom_profile( subClassOf(a,unionOf([b,c])), owl2_EL, true) )) :- true.
test(x2, true( \+ axiom_profile( equivalentClasses([a,unionOf([b,c])]), owl2_EL, true) )) :- true.
test(x3, true(    axiom_profile( equivalentClasses([a,intersectionOf([b,c])]), owl2_EL, true) )) :- true.








:- end_tests(owl2_profiles).

/** <module> tests for OWL2 profiles

---+ Synopsis

Command line:
  
==
swipl
?- [owl2_profiles].
?- load_test_files([]).
?- run_tests.
==

---+ Details

This is a test module for the module owl2_profiles

*/
