/* -*- Mode: Prolog -*- */

:- use_module(owl2_model).
:- use_module(owl2_xml).
:- use_module(owl2_io).

:- begin_tests(rna,[setup(load_owlxfile)]).

load_owlxfile :-
        load_axioms('testfiles/rnao.owlx',owlx).

test(loaded) :-
        \+ \+ ontology(_).

%test(showall) :-
%        forall(axiom(A),
%               format('~q.~n',[A])).

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

expected(subPropertyOf('http://purl.obofoundry.org/pairs_with_CHH', 'http://purl.obofoundry.org/pairs_with_HH')).
expected(equivalentClasses(['http://purl.obofoundry.org/RNAO_0000003',
                            objectIntersectionOf(objectComplementOf(objectSomeValuesFrom('http://purl.obofoundry.org/has_part',
                                                                                         objectIntersectionOf('http://purl.obofoundry.org/RNAO_0000028',
                                                                                                              objectSomeValuesFrom('http://purl.obofoundry.org/pairs_with_CWW',
                                                                                                                                   'http://purl.obofoundry.org/RNAO_0000028')))),
                                                 objectSomeValuesFrom('http://purl.obofoundry.org/has_part',
                                                                      objectIntersectionOf('http://purl.obofoundry.org/RNAO_0000017',
                                                                                           objectSomeValuesFrom('http://purl.obofoundry.org/pairs_with_CWW',
                                                                                                                'http://purl.obofoundry.org/RNAO_0000017'))),
                                                 objectExactCardinality('2', 'http://purl.obofoundry.org/has_part', 'http://purl.obofoundry.org/RNAO_0000019'))])).
        
:- end_tests(rna).

:- begin_tests(write,[setup(roundtrip)]).

roundtrip :-
        retract_all_axioms,
        load_axioms('testfiles/wine.owl'),
        save_axioms('testfiles/wine.owlx',owlx),
        retract_all_axioms,
        load_axioms('testfiles/wine.owlx',owlx).

        

test(loaded) :-
        \+ \+ ontology(_).
        
:- end_tests(write).


/** <module> tests for OWL2-XML parser

---+ Synopsis

Command line:
  
==
swipl
?- [owl2_xml].
?- load_test_files([]).
?- run_tests.
==

Or run from makefile

==
make test-owl2_xml
==

---+ Details

This is a test module for the module owl2_xml.pl 

*/
