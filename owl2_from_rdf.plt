/* -*- Mode: Prolog -*- */

:- use_module(owl2_model).
:- use_module(owl2_from_rdf).

:- begin_tests(wine,[setup(load_rdffile)]).

load_rdffile :-
        owl_parse_rdf('testfiles/wine.owl').

test(loaded) :-
        \+ \+ ontology(_).

test(subclasses) :-
        findall(A-B,subClassOf(A,B),Axs),
        %maplist(writeln,Axs),
        Axs\=[].

test(expected) :-
        forall(expected(Ax),
               Ax).

expected(objectProperty('http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#locatedIn')).
expected(subClassOf('http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#Wine', 'http://www.w3.org/TR/2003/PR-owl-guide-20031209/food#PotableLiquid')).
expected(subClassOf(intersectionOf(['http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#Loire',
                                    hasValue('http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#locatedIn',
                                             'http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#ToursRegion')]),
                    hasValue('http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#madeFromGrape',
                             'http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#CheninBlancGrape'))).


:- end_tests(wine).

:- begin_tests(wine_and_food,[setup(load_and_import_rdffile)]).

load_and_import_rdffile :-
        owl_parse_rdf('testfiles/wine.owl',[imports(true)]).

test(loaded) :-
        \+ \+ ontology(_).

test(expected) :-
        forall(expected(Ax),
               Ax).

expected(objectProperty('http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#locatedIn')).
expected(subClassOf('http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#Wine', 'http://www.w3.org/TR/2003/PR-owl-guide-20031209/food#PotableLiquid')).
expected(subClassOf(intersectionOf(['http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#Loire',
                                    hasValue('http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#locatedIn',
                                             'http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#ToursRegion')]),
                    hasValue('http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#madeFromGrape',
                             'http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#CheninBlancGrape'))).


:- end_tests(wine_and_food).

:- begin_tests(hydrology,[setup(load_rdffile)]).

load_rdffile :-
        owl_parse_rdf('testfiles/Hydrology.owl').

test(loaded) :-
        \+ \+ ontology(_).


test(expected) :-
        forall(expected(Ax),
               Ax).

expected(class('http://www.ordnancesurvey.co.uk/ontology/Hydrology/v2.0/Hydrology.owl#Canal')).

% TODO: un-functionify literal?
expected(annotationAssertion('http://www.ordnancesurvey.co.uk/ontology/Rabbit/v1.0/Rabbit.owl#Rabbit',
  'http://www.ordnancesurvey.co.uk/ontology/Hydrology/v2.0/Hydrology.owl#Burn',
  literal('Every Burn is a kind of Stream.\nEvery Burn is only located in exactly 1 of Scotland or Northern England.'))).

:- end_tests(hydrology).

/** <module> tests for OWL2 RDF parser

---+ Synopsis

Command line:
  
==
swipl
?- [owl2_from_rdf].
?- load_test_files([]).
?- run_tests.
==

Or run from makefile

==
make test-owl2_from_rdf
==

---+ Details

This is a test module for the module owl2_from_rdf.pl 

*/
