/* -*- Mode: Prolog -*- */

:- use_module(owl2_model).
:- use_module(owl2_manchester_parser).

:- begin_tests(frame,[]).

:- style_check(-atom).

tf('Class: foo').
tf('
  Class: foo
  SubClassOf: bar').



test(frame) :-
	forall(tf(A),
	       (   owl_parse_manchester_frame(A,X),
		   writeln(X))).




:- end_tests(frame).

:- begin_tests(expr,[]).

:- style_check(-atom).

t('r some (
   ( a and (r some (b and r some c))) or (z and (r some (y and r some z))))').
t('z and x').
t('foo and bar').
t('foo or bar').
t('foo and (r some bar)').
t('
 BFO_0000051 some (
GO_0030425 and BFO_0000051 some (
   GO_0042734 and BFO_0000050 some (
      GO_0045202 and BFO_0000050 some ?Y)))
 ').
t('RO_0002132 some (
   CARO_0001001 and (
      BFO_0000051 some (
         GO_0045211 and BFO_0000050 some (
            GO_0045202 and BFO_0000050 some ?Y
         ))) or (
      BFO_0000051 some (
         GO_0045211 and BFO_0000050 some (
            GO_0045202 and BFO_0000050 some ?Y))))').




test(expr) :-
	forall(t(X),
	       (   owl_parse_manchester_expression(X,Y),
		   writeln(Y))).




:- end_tests(expr).


:- begin_tests(owl2_manchester_parser,[setup(load_msfile)]).

load_msfile :-
        owl_parse_manchester_syntax_file('testfiles/rnao.owlms').

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


:- end_tests(owl2_manchester_parser).

/** <module> tests for OWL2 RDF parser

---+ Synopsis

Command line:
  
==
swipl
?- [owl2_manchester_parser].
?- load_test_files([]).
?- run_tests.
==

---+ Details

This is a test module for the module owl2_manchester_parser.pl 

*/
