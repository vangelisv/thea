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
        findall(Ax,
                (   expected(Ax),
                    debug(test,'Testing for: ~w',[Ax]),
                    \+ Ax,
                    debug(test,'** FAILED: ~w',[Ax])),
                FailedAxs),
        length(FailedAxs,NumFailed),
        debug(test,'*** TOTAL FAILED: ~d',[NumFailed]),
        FailedAxs=[].

expected(objectProperty('http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#locatedIn')).
expected(subClassOf('http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#Wine', 'http://www.w3.org/TR/2003/PR-owl-guide-20031209/food#PotableLiquid')).
expected(subClassOf(intersectionOf(['http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#Loire',
                                    hasValue('http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#locatedIn',
                                             'http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#ToursRegion')]),
                    hasValue('http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#madeFromGrape',
                             'http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#CheninBlancGrape'))).
expected(ontologyAxiom('http://www.w3.org/TR/2003/CR-owl-guide-20030818/wine',
                       propertyAssertion('http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#hasBody',
                                         'http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#SelaksIceWine',
                                         'http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#Medium'))).


:- end_tests(wine).

:- begin_tests(wine_and_food,[setup(load_and_import_rdffile)]).

load_and_import_rdffile :-
        owl_parse_rdf('testfiles/wine.owl',[imports(true)]).

test(loaded) :-
        \+ \+ ontology(_).

test(expected) :-
        findall(Ax,
                (   expected(Ax),
                    debug(test,'Testing for: ~w',[Ax]),
                    \+ Ax,
                    debug(test,'** FAILED: ~w',[Ax])),
                FailedAxs),
        length(FailedAxs,NumFailed),
        debug(test,'*** TOTAL FAILED: ~d',[NumFailed]),
        FailedAxs=[].

% these are replicated from the wine test
expected(objectProperty('http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#locatedIn')).
expected(subClassOf('http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#Wine', 'http://www.w3.org/TR/2003/PR-owl-guide-20031209/food#PotableLiquid')).
expected(subClassOf(intersectionOf(['http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#Loire',
                                    hasValue('http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#locatedIn',
                                             'http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#ToursRegion')]),
                    hasValue('http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#madeFromGrape',
                             'http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#CheninBlancGrape'))).
expected(ontologyAxiom('http://www.w3.org/TR/2003/CR-owl-guide-20030818/wine',
                       propertyAssertion('http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#hasBody',
                                         'http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#SelaksIceWine',
                                         'http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#Medium'))).


% these will only succeed if food.owl is imported
expected(objectProperty('http://www.w3.org/TR/2003/PR-owl-guide-20031209/food#madeFromFruit')).
expected(disjointClasses(['http://www.w3.org/TR/2003/PR-owl-guide-20031209/food#PastaWithWhiteSauce', 'http://www.w3.org/TR/2003/PR-owl-guide-20031209/food#PastaWithRedSauce'])).
expected(subClassOf('http://www.w3.org/TR/2003/PR-owl-guide-20031209/food#PastaWithWhiteSauce', 'http://www.w3.org/TR/2003/PR-owl-guide-20031209/food#Pasta')).

% VV - this one currently fails -- why?
expected(ontologyAxiom('http://www.w3.org/TR/2003/PR-owl-guide-20031209/food',
                       subClassOf('http://www.w3.org/TR/2003/PR-owl-guide-20031209/food#PastaWithWhiteSauce', 'http://www.w3.org/TR/2003/PR-owl-guide-20031209/food#Pasta'))).


:- end_tests(wine_and_food).

:- begin_tests(hydrology,[setup(load_rdffile)]).

load_rdffile :-
        owl_parse_rdf('testfiles/Hydrology.owl',[clear(complete)]).

test(loaded) :-
        \+ \+ ontology(_).

test(expected_count) :-
        debug(test,'Testing counts',[]),
        findall(Goal,
                (   expected_count(Goal,Number),
                    debug(test,'Testing count(~w) == ~w',[Goal,Number]),
                    aggregate(count,Goal,Goal,ActualNumber),
                    Number \= ActualNumber,
                    debug(test,'** FAILED count(~w) = ~w, expected: ~w',[Goal,ActualNumber,Number])),
                FailedGoals),
        length(FailedGoals,NumFailed),
        debug(test,'*** TOTAL FAILED: ~d',[NumFailed]),
        FailedGoals=[].

test(expected) :-
        findall(Ax,
                (   expected(Ax),
                    debug(test,'Testing for: ~w',[Ax]),
                    \+ Ax,
                    debug(test,'** FAILED: ~w',[Ax])),
                FailedAxs),
        length(FailedAxs,NumFailed),
        debug(test,'*** TOTAL FAILED: ~d',[NumFailed]),
        FailedAxs=[].

        

expected_count(class(_),186).
expected_count(symmetricProperty(_),186).



expected(class('http://www.ordnancesurvey.co.uk/ontology/Hydrology/v2.0/Hydrology.owl#Canal')).

% TODO: un-functionify literal?
expected(annotationAssertion('http://www.ordnancesurvey.co.uk/ontology/Rabbit/v1.0/Rabbit.owl#Rabbit',
  'http://www.ordnancesurvey.co.uk/ontology/Hydrology/v2.0/Hydrology.owl#Burn',
  literal('Every Burn is a kind of Stream.\nEvery Burn is only located in exactly 1 of Scotland or Northern England.'))).

expected(symmetricProperty('http://www.ordnancesurvey.co.uk/ontology/SpatialRelations/v0.2/SpatialRelations.owl#isAdjacentTo')).
expected(objectProperty('http://www.ordnancesurvey.co.uk/ontology/SpatialRelations/v0.2/SpatialRelations.owl#isAdjacentTo')).

% check to make sure annotationAssertions and propertyAssertions are handled correctly
% AP:
expected(annotationAssertion('http://www.ordnancesurvey.co.uk/ontology/Rabbit/v1.0/Rabbit.owl#Rabbit', 'http://www.ordnancesurvey.co.uk/ontology/Hydrology/v2.0/Hydrology.owl#Irrigation', literal('Irrigation is a secondary concept.'))).
expected(annotationAssertion('http://www.w3.org/2000/01/rdf-schema#comment', 'http://www.ordnancesurvey.co.uk/ontology/Hydrology/v2.0/Hydrology.owl#MineralWater', literal('Mineral Water is a secondary concept.\nEvery Mineral Water contains Mineral Salts.'))).
expected(annotationAssertion('http://purl.org/dc/elements/1.1/publisher', 'http://www.ordnancesurvey.co.uk/ontology/Hydrology/v2.0/Hydrology.owl', literal('Ordnance Survey'))).
expected(annotationAssertion('http://www.w3.org/2000/01/rdf-schema#label', 'http://www.ordnancesurvey.co.uk/ontology/Hydrology/v2.0/Hydrology.owl#Transport', literal('Transport'))).
expected(classAssertion('http://www.ordnancesurvey.co.uk/ontology/Hydrology/v2.0/Hydrology.owl#Surface', 'http://www.ordnancesurvey.co.uk/ontology/Hydrology/v2.0/Hydrology.owl#EarthsSurface')).
expected(propertyAssertion('http://www.ordnancesurvey.co.uk/ontology/Topography/v0.1/Topography.owl#isPartOf', 'http://www.ordnancesurvey.co.uk/ontology/Hydrology/v2.0/Hydrology.owl#Northern_England', 'http://www.ordnancesurvey.co.uk/ontology/Topography/v0.1/Topography.owl#England')).


expected(ontology('http://www.ordnancesurvey.co.uk/ontology/Hydrology/v2.0/Hydrology.owl')).


% class assertions
expected(classAssertion('http://www.ordnancesurvey.co.uk/ontology/Hydrology/v2.0/Hydrology.owl#UKCountry', 'http://www.ordnancesurvey.co.uk/ontology/Hydrology/v2.0/Hydrology.owl#scotland')).

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
