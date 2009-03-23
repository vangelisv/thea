/* -*- Mode: Prolog -*- */

:- use_module(owl2_model).
:- use_module(owl2_from_rdf).

:- begin_tests(owl2_from_rdf,[setup(load_rdffile)]).

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


:- end_tests(owl2_from_rdf).

/** <module> tests for OWL2 RDF parser

---+ Synopsis

Command line:
  
==
swipl
?- [owl2_from_rdf].
?- load_test_files([]).
?- run_tests.
==

---+ Details

This is a test module for the module owl2_from_rdf.pl 

*/
