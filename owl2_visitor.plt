/* -*- Mode: Prolog -*- */

:- use_module(owl2_visitor).

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

test(tr_expr_deep, [true(Axiom = subClassOf(a,intersectionOf([z,someValuesFrom(q,allValuesFrom(p,b))])))]) :-
        rewrite_axiom(subClassOf(a,intersectionOf([z,someValuesFrom(q,someValuesFrom(p,b))])),
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

test(tr_expr_ec, [true(Axiom = equivalentClasses([z,someValuesFrom(p,intersectionOf([a,b]))]))]) :-
        rewrite_axiom(equivalentClasses([z,intersectionOf([a,someValuesFrom(p,b)])]),
                      tr(expression,
                         intersectionOf([A,someValuesFrom(P,B)]),
                         someValuesFrom(P,intersectionOf([A,B])),
                         true,
                         []),
                      Axiom),
        writeln(a1=Axiom).
test(tr_expr_ec2, [true(Axiom = equivalentClasses([z,someValuesFrom(p,intersectionOf([a,b]))]))]) :-
        rewrite_axiom(equivalentClasses([z,intersectionOf([a,someValuesFrom(p,b)])]),
                      tr(expression,
                         intersectionOf([A,someValuesFrom(P,B)]),
                         someValuesFrom(P,intersectionOf([A,B])),
                         atomic(A),
                         []),
                      Axiom),
        writeln(a2=Axiom).

test(tr_expr_open, [true(Axiom = equivalentClasses([z,someValuesFrom(p,intersectionOf([a,b]))]))]) :-
        rewrite_axiom(equivalentClasses([z,intersectionOf([a,someValuesFrom(p,b)])]),
                      tr(_,
                         intersectionOf([A,someValuesFrom(P,B) | _]),
                         someValuesFrom(P,intersectionOf([A,B])),
                         true,
                         []),
                      Axiom),
        writeln(a3=Axiom).
test(tr_expr_open2, [true(Axiom = equivalentClasses([z,someValuesFrom(p,intersectionOf([a,b]))]))]) :-
        rewrite_axiom(equivalentClasses([z,intersectionOf([a,someValuesFrom(p,b),someValuesFrom(q,c)])]),
                      tr(expression,
                         intersectionOf([A,someValuesFrom(P,B) | _]),
                         someValuesFrom(P,intersectionOf([A,B])),
                         true,
                         []),
                      Axiom),
        writeln(a4=Axiom).
test(tr_expr_open3, [true(Axiom = equivalentClasses([z,someValuesFrom(p,intersectionOf([a,b,someValuesFrom(q,c)]))]))]) :-
        rewrite_axiom(equivalentClasses([z,intersectionOf([a,someValuesFrom(p,b),someValuesFrom(q,c)])]),
                      tr(expression,
                         intersectionOf([A,someValuesFrom(P,B) | Rest]),
                         someValuesFrom(P,intersectionOf([A,B | Rest])),
                         P==p,
                         []),
                      Axiom),
        writeln(a5=Axiom).



test(smatch1, [true(smatch(a,a))]) :- true.
test(smatch2, [true(smatch(someValuesFrom(a,b),someValuesFrom(a,b)))]) :- true.
test(smatch3, [true(\+smatch(someValuesFrom(a,b),someValuesFrom(b,a)))]) :- true.
test(smatch4, [true(smatch(intersectionOf([a,b,c]),
                           intersectionOf([b,a,c])))]) :- true.
test(smatch5, [true(\+smatch(intersectionOf([a,b,c,d]),
                             intersectionOf([b,a,c])))]) :- true.
test(smatch6, [true(smatch(intersectionOf([a,b,c]),
                           intersectionOf([b,a|_])))]) :- true.
test(smatch7, [true(\+smatch(intersectionOf([a,b,c]),
                             intersectionOf([d|_])))]) :- true.

test(smatch8, [true(A/P/B=a/p/b)]) :-
        smatch(intersectionOf([a,someValuesFrom(p,b)]),
               intersectionOf([someValuesFrom(P,B),A])),
        writeln(sm(A/P/B)).
test(smatch9, [true(A/P/B=a/p/b)]) :-
        smatch(intersectionOf([a,someValuesFrom(p,b)]),
               intersectionOf([A,someValuesFrom(P,B)|_])),
        writeln(sm(A/P/B)).
test(smatch10, [true(A/P/B=a/p/b)]) :-
        smatch(intersectionOf([a,someValuesFrom(p,b),b,c]),
               intersectionOf([A,someValuesFrom(P,B)|_])),
        writeln(sm(A/P/B)).
test(smatch11, [true(P/B/Rest=p/b/[c,someValuesFrom(q,d)])]) :-
        smatch(intersectionOf([a,someValuesFrom(p,b),c,someValuesFrom(q,d)]),
               intersectionOf([a,someValuesFrom(P,B)|Rest])),
        writeln(sm(P/B/Rest)).




        

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
