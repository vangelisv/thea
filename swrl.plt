/* -*- Mode: Prolog -*- */

:- use_module(swrl).
:- use_module(owl2_model).
:- use_module(owl2_from_rdf).
:- use_module(swrl_rdf_hooks).

:- begin_tests(family,[setup(load_swrlfile),cleanup(retract_all_axioms)]).

load_swrlfile :-
        owl_parse_rdf('testfiles/family.owl').

test(expected) :-
        forall(expected(Ax),
               (   writeln(Ax),
                   Ax)).

test(loaded_implies) :-
        \+ \+ implies(_,_).

expected(implies(['http://www.owl-ontologies.com/Ontology1172270693.owl#has_foster_grandfather'(i('http://www.owl-ontologies.com/Ontology1172270693.owl#a'), i('http://www.owl-ontologies.com/Ontology1172270693.owl#b')), 'http://www.owl-ontologies.com/Ontology1172270693.owl#has_health_state'(i('http://www.owl-ontologies.com/Ontology1172270693.owl#b'), i('http://www.owl-ontologies.com/Ontology1172270693.owl#c'))], ['http://www.owl-ontologies.com/Ontology1172270693.owl#has_fam_hx_in_foster_grandfather'(i('http://www.owl-ontologies.com/Ontology1172270693.owl#a'), i('http://www.owl-ontologies.com/Ontology1172270693.owl#c'))])).

:- end_tests(family).

:- begin_tests(ancestor,[setup(load_swrlfile),cleanup(retract_all_axioms)]).

load_swrlfile :-
        owl_parse_rdf('testfiles/dl-safe-ancestor.owl').

test(expected) :-
        forall(expected(Ax),
               (   writeln(Ax),
                   Ax)).

test(loaded_implies) :-
        \+ \+ implies(_,_).

expected(implies(['http://www.cs.man.ac.uk/~bparsia/2007/examples/dl-safe-ancestor.owl#ancestorOf'(i('http://www.cs.man.ac.uk/~bparsia/2007/examples/dl-safe-ancestor.owl#x'), i('http://www.cs.man.ac.uk/~bparsia/2007/examples/dl-safe-ancestor.owl#y')),
                'http://www.cs.man.ac.uk/~bparsia/2007/examples/dl-safe-ancestor.owl#ancestorOf'(i('http://www.cs.man.ac.uk/~bparsia/2007/examples/dl-safe-ancestor.owl#y'), i('http://www.cs.man.ac.uk/~bparsia/2007/examples/dl-safe-ancestor.owl#z'))],
               ['http://www.cs.man.ac.uk/~bparsia/2007/examples/dl-safe-ancestor.owl#ancestorOf'(i('http://www.cs.man.ac.uk/~bparsia/2007/examples/dl-safe-ancestor.owl#x'), i('http://www.cs.man.ac.uk/~bparsia/2007/examples/dl-safe-ancestor.owl#z'))])).


:- end_tests(ancestor).

:- begin_tests(pl2swrl,[]).

:- op(1100,xfy,<==>).


subClassOf(ab, unionOf([a, b])) <==>
(   (   a(X) ;   b(X)) :- ab(X)).

subClassOf(x, y) <==>  (   y(X):-  x(X)).

subClassOf(unionOf([a, b]), ab) <==>
(   ab(X):- (   a(X) ;   b(X))).

subPropertyOf(r, s) <==>
(   r(X,Y):- s(X,Y)).

subPropertyOf(r, inverseOf(t)) <==>
(   r(X,Y):- t(Y,X)).

subPropertyOf(r, propertyChain([s, t])) <==>
(   r(X,Y):- s(X,V1),t(V1,Y)).

subPropertyOf(r, propertyChain([s, t, u, v, inverseOf(w), x])) <==>
(   r(X,Y):-      s(X,V1),t(V1,V2),u(V2,V3),v(V3,V4),w(V5,V4),x(V5,Y)).

subPropertyOf(r, propertyChain([s, u, w])) <==>
(   r(X,Y):-     s(X,V1),u(V1,V2),w(V2,Y)).

subPropertyOf(r, propertyChain([s, inverseOf(t)])) <==>
(   r(X,Y):-   s(X,V1),t(Y,V1)).

% TODO: equivalent classes involves checking combinations of rules
(equivalentClasses([ab,unionOf([a,b])]) <==>
(   ab(X) :- (a(X);b(X)))).

equivalentClasses([efg, intersectionOf([e, f, g])])
<==>
[(   e(X):- efg(X)),
 (   f(X):- efg(X)),
 (   g(X):- efg(X)),
 (   efg(X):- e(X),f(X),g(X))].


test(pl2owl) :-
        forall( (Axiom <==> Pl),
                pl2owl(Pl,Axiom)).

pl2owl(Pl,Axiom) :-
        prolog_clause_to_swrl_rule(Pl,Rule),
        swrl_to_owl_axioms(Rule,[Axiom]).

check(Axiom,Pl) :-
        prolog_clause_to_swrl_rule(Pl,Rule),
        swrl_to_owl_axioms(Rule,[Axiom]).

:- end_tests(pl2swrl,[]).



/** <module> tests for SWRL

  ---+ Synopsis

  Command line:
  
==
swipl
?- [swrl].
?- load_test_files([]).
?- run_tests.
==

---+ Details

This is a test module for the module swrl.pl 

*/
