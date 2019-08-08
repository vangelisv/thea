% /* -*- Mode: Prolog -*- */

:- use_module(owl2_model).
:- use_module(owl2_from_rdf).


:- begin_tests(wine,[setup(load_rdffile)]).

load_rdffile :-
        owl_parse_rdf('testfiles/wine.owl').

test(loaded) :-
        \+ \+ ontology(_).

test(expected) :-        test_expected.
test(expected_count) :-        test_expected_count.

expected_count(class(_),74).
expected_count(transitiveProperty(_),1).

expected(objectProperty('http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#locatedIn')).
expected(subClassOf('http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#Wine', 'http://www.w3.org/TR/2003/PR-owl-guide-20031209/food#PotableLiquid')).
%expected(subClassOf(intersectionOf(['http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#Loire',
%                                    hasValue('http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#locatedIn',
%                                             'http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#ToursRegion')]),
%                    hasValue('http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#madeFromGrape',
%                             'http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#CheninBlancGrape'))).
expected(ontologyAxiom('http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine',
                       propertyAssertion('http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#hasBody',
                                         'http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#SelaksIceWine',
                                         'http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#Medium'))).

expected(axiom(equivalentClasses(['http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#WhiteWine',
				  intersectionOf(['http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#Wine',
						  hasValue('http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#hasColor',
							   'http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#White')])
				 ]))).

:- end_tests(wine).

:- begin_tests(wine_and_food,[setup(load_and_import_rdffile)]).

load_and_import_rdffile :-
        owl_parse_rdf('testfiles/wine.owl',[imports(true),clear(complete)]).

test(loaded) :-
        \+ \+ ontology(_).

test(expected) :-        test_expected.
test(expected_count) :-        test_expected_count.

expected_count(class(_),137).

% these are replicated from the wine test
expected(objectProperty('http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#locatedIn')).
expected(subClassOf('http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#Wine', 'http://www.w3.org/TR/2003/PR-owl-guide-20031209/food#PotableLiquid')).
%expected(subClassOf(intersectionOf(['http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#Loire',
%                                    hasValue('http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#locatedIn',
%                                             'http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#ToursRegion')]),
%                    hasValue('http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#madeFromGrape',
%                             'http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#CheninBlancGrape'))).
expected(ontologyAxiom('http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine',
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

%expected(ontologyAxiom('http://www.w3.org/TR/2003/PR-owl-guide-20031209/food',
%		       subClassOf('http://www.w3.org/TR/2003/PR-owl-guide-20031209/food#PastaWithWhiteSauce',
%				  'http://www.w3.org/TR/2003/PR-owl-guide-20031209/food#Pasta'))).


:- end_tests(wine_and_food).

:- begin_tests(hydrology,[setup(load_rdffile)]).

load_rdffile :-
        owl_parse_rdf('testfiles/Hydrology.owl',[clear(complete)]).

test(loaded) :-
        \+ \+ ontology(_).

test(expected_count) :- test_expected_count.
test(expected) :-        test_expected.

% TODO: parser still needs to do extra cleaning up
test(clean) :-
        \+ ((classAssertion(_,BNode),
             sub_atom(BNode,_,_,_,'#__Description'))).


expected_count(class(_),186).
expected_count(symmetricProperty(_),3). % isPartOf is declared symmetric - weird..



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

expected(subClassOf('http://www.ordnancesurvey.co.uk/ontology/Hydrology/v2.0/Hydrology.owl#LockGate',
                    someValuesFrom('http://www.ordnancesurvey.co.uk/ontology/MereologicalRelations/v0.2/MereologicalRelations.owl#isPartOf',
                                   'http://www.ordnancesurvey.co.uk/ontology/Hydrology/v2.0/Hydrology.owl#Lock'))).


:- end_tests(hydrology).

:- begin_tests(edge_cases,[setup(load_rdffile)]).

load_rdffile :-
        owl_parse_rdf('testfiles/rdfowl_test.owl',[clear(complete)]).

test(loaded) :-
        \+ \+ ontology(_).

test(expected_count) :- test_expected_count.

expected_count(class(_),4).

:- end_tests(edge_cases).

:- begin_tests(repository_test,[]).

test(no_repos) :-
        retractall(owl2_from_rdf:owl_repository(_,_)),
        \+ catch(owl_parse_rdf('testfiles/import_test.owl',[imports(true)]),
                 Message,
                 (   format('Got error as expected: ~w~n',[Message]),
                     fail)).

test(with_repos) :-
        retractall(owl2_from_rdf:owl_repository(_,_)),
        assert(owl2_from_rdf:owl_repository('http://hopefully.non.existent.swi-prolog.org','http://www.w3.org/TR/2003/CR-owl-guide-20030818/wine')),
        owl_parse_rdf('testfiles/import_test.owl',[imports(true)]).


:- end_tests(repository_test).


:- begin_tests(owl2_test_cases,[setup(setup_owl2_test_cases)]).

test(all) :-
	expand_file_name('c:/sw/supportmaterial/owl/owl2_test_cases/all/*.rdf',Files),
	forall(member(File,Files),
	       (owl_parse(File,complete,complete,false),
		aggregate_all(count,owl2_from_rdf:owl(not_used,_,_,_),S),
		( S = 0, print('...success') ; print('...fail'-S-unused-'triples')),
		nl)).

:- end_tests(owl2_test_cases).

% TEST UTILITY PREDICATES

setup_owl2_test_cases :-
      owl_parse('c:/sw/supportmaterial/owl/owl2_test_cases/all.rdf',complete,complete,true),
      print('parsin all tests --ok'),
      forall((axiom(classAssertion('http://www.w3.org/2007/OWL/testOntology#TestCase',X)),
	      propertyAssertion('http://www.w3.org/2007/OWL/testOntology#rdfXmlPremiseOntology',X,literal(type(_,V))),
	      propertyAssertion('http://www.w3.org/2007/OWL/testOntology#identifier',X,literal(type(_,ID)))
	     ),
	     (print(ID),nl,
	      atomic_list_concat(['c:/sw/supportmaterial/owl/owl2_test_cases/all/',ID,'.rdf'],Filename),print(Filename),nl,
	      open(Filename,write,S),
	      write(S,'<?xml version="1.0" encoding="UTF-8"?>'),nl(S),
	      write(S,V),
	      close(S)
	     )
	    ).



:- module_transparent test_expected_count/0.
test_expected_count :-
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

:- module_transparent test_expected/0.
test_expected :-
        findall(Ax,
                (   expected(Ax),
                    debug(test,'Testing for: ~w',[Ax]),
                    \+ Ax,
                    debug(test,'** FAILED: ~w',[Ax])),
                FailedAxs),
        length(FailedAxs,NumFailed),
        debug(test,'*** TOTAL FAILED: ~d',[NumFailed]),
        FailedAxs=[].


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
