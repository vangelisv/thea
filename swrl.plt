/* -*- Mode: Prolog -*- */

:- use_module(swrl).
:- use_module(owl2_model).
:- use_module(owl2_from_rdf).
:- use_module(swrl_rdf_hooks).

:- begin_tests(family,[setup(load_swrlfile),cleanup(retract_all_axioms)]).

load_swrlfile :-
        owl_parse_rdf('testfiles/family.owl').

test(expected) :-
        forall(expected(Ax),
               (   writeln(Ax),
                   Ax)).

test(loaded_implies) :-
        \+ \+ implies(_,_).

expected(implies(['http://www.owl-ontologies.com/Ontology1172270693.owl#has_foster_grandfather'(i('http://www.owl-ontologies.com/Ontology1172270693.owl#a'), i('http://www.owl-ontologies.com/Ontology1172270693.owl#b')), 'http://www.owl-ontologies.com/Ontology1172270693.owl#has_health_state'(i('http://www.owl-ontologies.com/Ontology1172270693.owl#b'), i('http://www.owl-ontologies.com/Ontology1172270693.owl#c'))], ['http://www.owl-ontologies.com/Ontology1172270693.owl#has_fam_hx_in_foster_grandfather'(i('http://www.owl-ontologies.com/Ontology1172270693.owl#a'), i('http://www.owl-ontologies.com/Ontology1172270693.owl#c'))])).

:- end_tests(family).

:- begin_tests(ancestor,[setup(load_swrlfile),cleanup(retract_all_axioms)]).

load_swrlfile :-
        owl_parse_rdf('testfiles/dl-safe-ancestor.owl').

test(expected) :-
        forall(expected(Ax),
               (   writeln(Ax),
                   Ax)).

test(loaded_implies) :-
        \+ \+ implies(_,_).

expected(implies(['http://www.cs.man.ac.uk/~bparsia/2007/examples/dl-safe-ancestor.owl#ancestorOf'(i('http://www.cs.man.ac.uk/~bparsia/2007/examples/dl-safe-ancestor.owl#x'), i('http://www.cs.man.ac.uk/~bparsia/2007/examples/dl-safe-ancestor.owl#y')),
                'http://www.cs.man.ac.uk/~bparsia/2007/examples/dl-safe-ancestor.owl#ancestorOf'(i('http://www.cs.man.ac.uk/~bparsia/2007/examples/dl-safe-ancestor.owl#y'), i('http://www.cs.man.ac.uk/~bparsia/2007/examples/dl-safe-ancestor.owl#z'))],
               ['http://www.cs.man.ac.uk/~bparsia/2007/examples/dl-safe-ancestor.owl#ancestorOf'(i('http://www.cs.man.ac.uk/~bparsia/2007/examples/dl-safe-ancestor.owl#x'), i('http://www.cs.man.ac.uk/~bparsia/2007/examples/dl-safe-ancestor.owl#z'))])).


:- end_tests(ancestor).

/** <module> tests for SWRL

  ---+ Synopsis

  Command line:
  
==
swipl
?- [swrl].
?- load_test_files([]).
?- run_tests.
==

---+ Details

This is a test module for the module swrl.pl 

*/
