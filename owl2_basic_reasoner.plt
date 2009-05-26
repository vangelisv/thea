/* -*- Mode: Prolog -*- */

:- use_module(owl2_basic_reasoner).
:- use_module(owl2_model).

:- begin_tests(owl2_basic_reasoner,[setup(init_axioms),cleanup(retract_all_axioms)]).

init_axioms :-
        Axioms=[
                ontology(trophy),
                subClassOf(animal,organism),
                subClassOf(mammal,animal),
                subClassOf(bird,animal),
                subClassOf(fish,animal),
                subClassOf(arthropod,animal),
                subClassOf(crustacean,arthropod),
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
                classAssertion(human,human1),
                classAssertion(man_eating_shrimp,shrimpzilla),
                classAssertion(shrimp,shrimpzuki),

                propertyAssertion(eats,tom,goldie),
                propertyAssertion(eats,tom,jerry),
                propertyAssertion(eats,kes,tom),
                propertyAssertion(eats,kes,pike1),
                propertyAssertion(eats,pike1,perch2),
                propertyAssertion(eats,perch2,shrimp3),
                propertyAssertion(eats,perch2,shrimp3),
                propertyAssertion(eats,shrimpzilla,human1),
                propertyAssertion(eats,shrimpzuki,human1),
                
                disjointClasses([mammal,fish])
               ],
        retract_all_axioms,
        maplist(assert_axiom,Axioms).

test(loaded) :-
        \+ \+ ontology(_).

test(query) :-
        forall((query(Q,T,ExpectedL),debug(test,'Query: ~w',[Q])),
               (   setof(T,Q^entailed(Q),RL),
                   writeln(results=RL),
                   RL=ExpectedL)).

query(subClassOf(cat,X),X,[animal,mammal,organism|_]).


expected(subClassOf(man_eating_shrimp,someValuesFrom(eats,human))).
expected(subClassOf(man_eating_shrimp,dangerous_animal)).
expected(subClassOf(human,organism)).
expected(subClassOf(intersectionOf([mammal, someValuesFrom(eats, animal)]),
                    intersectionOf([mammal, someValuesFrom(eats, organism)]))).
expected(classAssertion(organism,human1)).
expected(classAssertion(man_eating_shrimp,shrimpzuki)).
expected(classAssertion(intersectionOf([mammal, someValuesFrom(eats, fish)]),tom)).


test(expected) :-
        forall(expected(A),
               test_for(A)).

test_for(A) :-
        debug(test,'testing for ~w',[A]),
        (   entailed(A)
        ->  debug(test,'  **OK** ~w',[A])
        ;   debug(test,'  **FAIL** ~w',[A]),
            fail).



:- end_tests(owl2_basic_reasoner).

/** <module> tests for OWL2 basic_reasoner

---+ Synopsis

Command line:
  
==
swipl
?- [owl2_basic_reasoner].
?- load_test_files([]).
?- run_tests.
==

---+ Details

This is a test module for the module owl2_basic_reasoner

*/
