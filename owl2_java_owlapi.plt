/* -*- Mode: Prolog -*- */

:- use_module(owl2_model).
:- use_module(owl2_io).
:- use_module(owl2_java_owlapi).


:- begin_tests(pellet,[setup(load)]).

load :-
        load_axioms('testfiles/music_ontology.owl').

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

:- begin_tests(teams,[setup(load)]).

% see http://owl.cs.manchester.ac.uk/2009/07/sssw/teams.html

load :-
        load_axioms('testfiles/teams.owl').

test(loaded) :-
        \+ \+ ontology(_).

test(reasoner) :-
        create_factory(Man,Fac),
        build_ontology(Man,Fac,Ont),
        writeln(classifying),
        create_reasoner(Man,pellet,Reasoner),
        reasoner_classify(Reasoner,Man,Ont),
        reasoner_subClassOf(Reasoner,Fac,'http://owl.cs.manchester.ac.uk/2009/07/sssw/teams#OntologyFC','http://owl.cs.manchester.ac.uk/2009/07/sssw/teams#MixedTeam'),
        \+ reasoner_subClassOf(Reasoner,Fac,'http://owl.cs.manchester.ac.uk/2009/07/sssw/teams#OntologyFC','http://owl.cs.manchester.ac.uk/2009/07/sssw/teams#NonSingletonTeam').

        

:- end_tests(teams).


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
