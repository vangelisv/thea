/* -*- Mode: Prolog -*- */

:- use_module(owl2_io).
:- use_module(owl2_model).
:- use_module(owl2_export_rdf).

:- begin_tests(wine_trip,[setup(roundtrip)]).

roundtrip :-
        load_axioms('testfiles/wine.owl'),
        save_axioms('testfiles/temp.owl',owl),
        retract_all_axioms/0,
        save_axioms('testfiles/temp.owl',owl).

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


:- end_tests(wine_trip).

