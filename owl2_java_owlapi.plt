/* -*- Mode: Prolog -*- */

:- use_module(owl2_model).
:- use_module(owl2_java_owlapi).

:- begin_tests(pellet,[setup(load_rdffile)]).

:- use_module(owl2_from_rdf).

load_rdffile :-
        owl_parse_rdf('testfiles/music_ontology.owl').

test(loaded) :-
        \+ \+ ontology(_).

test(reasoner) :-
        create_factory(Man,Fac),
        build_ontology(Man,Fac,Ont),
        writeln(classifying),
        create_reasoner(Man,pellet,Reasoner),
        reasoner_classify(Reasoner,Man,Ont),
        writeln(classified),
        forall(class(C),
               (   writeln(class=C),
                   forall(reasoner_subClassOf(Reasoner,Fac,C,P),
                         writeln(superclass=P)))).


:- end_tests(pellet).


/** <module> tests for OWL2 RDF parser

---+ Synopsis

Command line:
  
==
swipl
?- [owl2_java_owlapi].
?- load_test_files([]).
?- run_tests.
==

JPL Required

Set your CLASSPATH to include owlapi-bin.jar, pellet.jar, ...

Or run from command line

==
bin/thea-run-tests-java owl2_java_owlapi
==

---+ Details

This is a test module for the module owl2_java_owlapi.pl 

*/
