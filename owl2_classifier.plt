/* -*- Mode: Prolog -*- */

:- use_module(owl2_classifier).
:- use_module(owl2_model).

:- begin_tests(owl2_classifier,[setup(init_axioms),cleanup(retract_all_axioms)]).

init_axioms :-
        Axioms=[
                ontology(trophy),
                subClassOf(animal,organism),
                subClassOf(fungi,organism),
                subClassOf(yeast,fungi),
                subClassOf(vertebrate,animal),
                subClassOf(tetrapod,vertebrate),
                subClassOf(mammal,tetrapod),
                subClassOf(snake,tetrapod),
                subClassOf(bird,tetrapod),
                subClassOf(fish,vertebrate),
                subClassOf(arthropod,animal),
                subClassOf(crustacean,arthropod),
                subClassOf(insect,arthropod),
                subClassOf(drosophila,insect),
                subClassOf(dmel,drosophila),
                subClassOf(cat,mammal),
                subClassOf(mouse,mammal),
                subClassOf(dog,mammal),
                subClassOf(human,mammal),
                subClassOf(raptor,bird),
                subClassOf(eagle,raptor),
                subClassOf(kestrel,raptor),
                subClassOf(goldfish,fish),
                subClassOf(pike,fish),
                subClassOf(perch,fish),
                subClassOf(shrimp,crustacean),
                subClassOf(man_eating_shrimp,crustacean),

                equivalentClasses([man_eating_shrimp,intersectionOf([shrimp,someValuesFrom(eats,human)])]),
                equivalentClasses([dangerous_animal,intersectionOf([animal,someValuesFrom(eats,human)])]),
                equivalentClasses([carnivore,intersectionOf([organism,someValuesFrom(eats,animal)])]),
                
                classAssertion(cat,tom),
                classAssertion(mouse,jerry),
                classAssertion(goldfish,goldie),
                classAssertion(kestrel,kes),
                classAssertion(pike,pike1),
                classAssertion(perch,perch2),
                classAssertion(shrimp,shrimp3),
                classAssertion(snake,mr_slither),
                classAssertion(human,human1),
                classAssertion(man_eating_shrimp,shrimpzilla),
                classAssertion(shrimp,shrimpzuki),
                classAssertion(yeast,yeastcell1),
                classAssertion(dmel,fly1),

                propertyAssertion(eats,tom,goldie),
                propertyAssertion(eats,tom,jerry),
                propertyAssertion(eats,kes,tom),
                propertyAssertion(eats,kes,pike1),
                propertyAssertion(eats,pike1,perch2),
                propertyAssertion(eats,perch2,shrimp3),
                propertyAssertion(eats,perch2,shrimp3),
                propertyAssertion(eats,shrimpzilla,human1),
                propertyAssertion(eats,shrimpzuki,human1),
                propertyAssertion(eats,fly1,yeastcell1),
                propertyAssertion(eats,mr_slither,fly1),
                
                disjointClasses([mammal,fish])
               ],
        retract_all_axioms,
        maplist(assert_axiom,Axioms).

test(loaded) :-
        \+ \+ ontology(_).

test(classmaker) :-
        forall(optimal_description(D),
               writeln(D)).


:- end_tests(owl2_classifier).

/** <module> tests for OWL2 classifier

---+ Synopsis

Command line:
  
==
swipl
?- [owl2_classifier].
?- load_test_files([]).
?- run_tests.
==

---+ Details

This is a test module for the module owl2_classifier

*/
