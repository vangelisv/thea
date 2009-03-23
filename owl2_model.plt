/* -*- Mode: Prolog -*- */

:- use_module(owl2_model).

:- begin_tests(owl2_model,[setup(init_axioms)]).

init_axioms :-
        Axioms=[
                ontology(animals),
                class(organism),
                class(animal),
                class(carnivore),
                class(herbivore),
                objectProperty(eats),
                subClassOf(animal,organism),
                equivalentClasses([carnivore,intersectionOf([animal,someValuesFrom(eats,animal)])]),
                equivalentClasses([herbivore,
                                   intersectionOf([animal,
                                                   allValuesFrom(eats,complementOf(animal))])]),
                equivalentClasses([auto_carnivore,hasSelf(eats)]),
                disjointClasses([herbivore,carnivore])
               ],
        maplist(assert_axiom,Axioms).

test(loaded) :-
        \+ \+ ontology(_).

test(subclasses) :-
        findall(A-B,subClassOf(A,B),Axs),
        Axs\=[].

:- end_tests(owl2_model).

/** <module> tests for OWL2 model

---+ Synopsis

Command line:
  
==
swipl
?- [owl2_model].
?- load_test_files([]).
?- run_tests.
==

---+ Details

This is a test module for the module owl2_from_rdf.pl 

*/
