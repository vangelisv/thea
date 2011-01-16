/* -*- Mode: Prolog -*- */

%:- use_module(owl2_visitor).
:- [owl2_visitor].

:- begin_tests(owl2_visitor,[setup(init_axioms),cleanup(retract_all_axioms)]).

init_axioms :-
        load_axioms('testfiles/caro.owl').

test(tr_axiom, [true(Axiom = subClassOf(b,someValuesFrom(p2,a)))]) :-
        rewrite_axiom(subClassOf(a,someValuesFrom(p,b)),
                      tr(axiom,
                         subClassOf(A,someValuesFrom(p,B)),
                         subClassOf(B,someValuesFrom(p2,A)),
                         true,
                         []),
                      Axiom).
test(tr_axiom_dupe, [true(Axioms = [_,_])]) :-
        findall(Axiom,
                rewrite_axiom(subClassOf(a,someValuesFrom(p,b)),
                              tr(axiom,
                                 subClassOf(A,someValuesFrom(p,B)),
                                 (   subClassOf(B,someValuesFrom(p2,A)),
                                     subClassOf(B,someValuesFrom(p3,A))
                                 ),
                                 true,
                                 []),
                              Axiom),
                Axioms).


test(tr_expr, [true(Axiom = subClassOf(a,allValuesFrom(p,b)))]) :-
        rewrite_axiom(subClassOf(a,someValuesFrom(p,b)),
                      tr(expression,
                         someValuesFrom(p,B),
                         allValuesFrom(p,B),
                         true,
                         []),
                      Axiom),
        writeln(Axiom).

test(tr_expr_dupe, [true((member(subClassOf(a,allValuesFrom(p,b)),Axioms),
                          member(subClassOf(a,allValuesFrom(p2,b)),Axioms),
                          Axioms=[_,_]))]) :-
        findall(Axiom,
                rewrite_axiom(subClassOf(a,someValuesFrom(p,b)),
                              tr(expression,
                                 someValuesFrom(p,B),
                                 (   allValuesFrom(p,B),
                                     allValuesFrom(p2,B)
                                 ),
                                 true,
                                 []),
                              Axiom),
                Axioms),
        writeln(Axioms).

/*
  test(tr_axiom, [true(Axiom = subClassOf(b,someValuesFrom(p2,a)))]) :-
        rewrite_axiom(subClassOf(a,someValuesFrom(p,b)),
                      tr(subClassOf(A,someValuesFrom(p,B)),
                         subClassOf(B,someValuesFrom(p2,A))),
                      Axiom).
test(tr_axiom_dupe, [true(Axioms = [_,_])]) :-
        findall(Axiom,
                rewrite_axiom(subClassOf(a,someValuesFrom(p,b)),
                              tr(subClassOf(A,someValuesFrom(p,B)),
                                 (   subClassOf(B,someValuesFrom(p2,A)),
                                     subClassOf(B,someValuesFrom(p3,A))
                                 )),
                              Axiom),
                Axioms).


test(tr_expr, [true(Axiom = subClassOf(a,allValuesFrom(p,b)))]) :-
        rewrite_axiom(subClassOf(a,someValuesFrom(p,b)),
                      tr(someValuesFrom(p,B),
                         allValuesFrom(p,B)),
                      Axiom),
        writeln(Axiom).

test(tr_expr_dupe, [true((member(subClassOf(a,allValuesFrom(p,b)),Axioms),
                          member(subClassOf(a,allValuesFrom(p2,b)),Axioms),
                          Axioms=[_,_]))]) :-
        findall(Axiom,
                rewrite_axiom(subClassOf(a,someValuesFrom(p,b)),
                              tr(someValuesFrom(p,B),
                                 (   allValuesFrom(p,B),
                                     allValuesFrom(p2,B)
                                 )),
                              Axiom),
                Axioms),
        writeln(Axioms).
*/


        

:- end_tests(owl2_visitor).

/** <module> tests for OWL2 visitor

---+ Synopsis

Command line:
  
==
swipl
?- [owl2_visitor].
?- load_test_files([]).
?- run_tests.
==

---+ Details

This is a test module for the module owl2_visitor

*/
