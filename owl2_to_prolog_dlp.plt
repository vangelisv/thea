/* -*- Mode: Prolog -*- */

:- use_module(owl2_to_prolog_dlp).

:- begin_tests(owl2_to_prolog_dlp,[setup(init_axioms),cleanup(retract_all_axioms)]).

init_axioms :-
        Axioms=[
                ontology(animals),
                class(organism),
                class(animal),
                class(carnivore),
                class(herbivore),
                objectProperty(eatsChain),
                objectProperty(eats),
                objectProperty(eatenBy),
                subPropertyOf(eats,eatsChain),
                transitiveProperty(eatsChain),
                inverseProperties(eats,eatenBy),
                subClassOf(animal,organism),
                equivalentClasses([carnivore,intersectionOf([animal,someValuesFrom(eats,animal)])]),
                equivalentClasses([herbivore,
                                   intersectionOf([animal,
                                                   allValuesFrom(eats,complementOf(animal))])]),
                %equivalentClasses([auto_carnivore,hasSelf(eats)]),
                disjointClasses([herbivore,carnivore])
               ],
        retract_all_axioms,
        maplist(assert_axiom,Axioms).

test(all) :-
        axiom(Ax),
        writeln(axiom=Ax),
        owl_dlpterm(Ax,Pl),
        writeln(prolog=Pl),
        owl_write_prolog_code(Pl,[]),
        fail.


:- end_tests(owl2_to_prolog_dlp).

/** <module> tests for OWL2 model

---+ Synopsis

Command line:
  
==
swipl
?- [owl2_to_prolog_dlp].
?- load_test_files([]).
?- run_tests.
==

---+ Details

This is a test module for the module owl2_from_rdf.pl 

*/
