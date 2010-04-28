/* -*- Mode: Prolog -*- */

:-module(owl2_rl_rules2,
	 [
	  clear_entailments/0,
	  is_entailed/2,
	  are_entailed/4

	 ]).


/** <module> OWL2 RL Rules Implementation

---+ Synopsis

This module implements the OWL2 RL rule-based reasoninng


---+ Details

See http://www.w3.org/TR/2008/WD-owl2-profiles-20081202/#Reasoning_in_OWL_2_RL_and_RDF_Graphs_using_Rules

---+ Additional Information


@author Vangelis Vassiliadis
@license GPL
@tbd
 Ontology - specific reasoning: Execute entailments only for axioms of
 specific Ontology based on axiom/2 facts.

*/


:- use_module('owl2_from_rdf').
:- use_module('owl2_model').

:- dynamic entails/3.
:- dynamic entailed/3.
:- discontiguous entails/3.


% ----------------------------------------
% Rule Engine
% ----------------------------------------

%% entails(+RuleID,+Antecedents:list, +Consequents:list) is nondet
%
%  Database of entails facts representing the rule database%  it is used by the rule engine to infer new axioms based on existing
%  axioms
%
/*
% Semantics of Equality (Table 4)
entails(eq-sym, [sameAs(X,Y)],[sameAs(Y,X)]).
entails(eq-trans, [sameAs(X,Z),sameAs(Z,Y)],[sameAs(X,Y)]).

entails(eq-rep-s,[sameAs(X,Y),propertyAssertion(P,X,V)],[propertyAssertion(P,Y,V)]).
entails(eq-rep-p,[sameAs(P1,P2),propertyAssertion(P1,X,V)],[propertyAssertion(P2,X,V)]).
entails(eq-rep-o,[sameAs(V1,V2),propertyAssertion(P,X,V1)],[propertyAssertion(P,X,V2)]).
entails(eq-diff1,[sameAs(X,Y),differentFrom(X,Y)],[false]).
% entails(eq-diff2, Todo...

% Table 5. The Semantics of Axioms about Properties
entails(prp-dom, [propertyDomain(P,C),propertyAssertion(P,X,_)],[classAssertion(C,X)]).
entails(prp-rng, [propertyRange(P,C),propertyAssertion(P,_,Y)],[classAssertion(C,Y)]).
entails(prp-fp,  [functionalProperty(P),propertyAssertion(P,X,Y1),propertyAssertion(P,X,Y2),pl(Y1 \= Y2)],[sameAs(Y1,Y2)]).
entails(prp-ifp, [inverseFunctionalProperty(P),propertyAssertion(P,X1,Y),propertyAssertion(P,X2,Y)],[sameAs(X1,X2)]).
entails(prp-irp, [irreflexiveProperty(P),propertyAssertion(P,X,X)],[false]).
entails(prp-symp,[symmetricProperty(P),propertyAssertion(P,X,Y)],[propertyAssertion(P,Y,X)]).
entails(prp-asyp,[asymmetricProperty(P),propertyAssertion(P,X,Y),propertyAssertion(P,Y,X)],[false]).
entails(prp-trp, [transitiveProperty(P),propertyAssertion(P,X,Y),propertyAssertion(P,Y,Z)],[propertyAssertion(P,X,Z)]).
entails(prp-spo1,[subPropertyOf(P1,P2),propertyAssertion(P1,X,Y)],[propertyAssertion(P2,X,Y)]).
% Todo entails(prp-spo2... Propertychain
entails(prp-eqp1,[equivalentProperties([P1,P2]),propertyAssertion(P1,X,Y)],[propertyAssertion(P2,X,Y)]).
entails(prp-eqp2,[equivalentProperties([P1,P2]),propertyAssertion(P2,X,Y)],[propertyAssertion(P1,X,Y)]).
entails(prp-pdw, [disjointProperties(P1,P2),propertyAssertion(P1,X,Y),propertyAssertion(P2,X,Y)],[false]).
entails(prp-adp, [disjointProperties(P),pl(select(P1,P,Rest)),pl(member(P2,Rest)),
		  propertyAssertion(P1,X,Y),
		  propertyAssertion(P2,X,Y)],[false]).
entails(prp-inv1,[inverseOf(P1,P2),propertyAssertion(P1,X,Y)],[propertyAssertion(P2,Y,X)]).
entails(prp-inv2,[inverseOf(P1,P2),propertyAssertion(P2,X,Y)],[propertyAssertion(P1,Y,X)]).
entails(prp-key, [hasKey(C,[P]),classAssertion(C,X), % Todo for Key lists with more than 1 properties
		  propertyAssertion(P,X,Z),propertyAssertion(P,Y,Z)],[sameAs(X,Y)]).

% Table 6. The Semantics of Classes
% Todo what about
% cls-thing 	 true 	 T(owl:Thing, rdf:type, owl:Class)
% cls-nothing1   true    T(owl:Nothing, rdf:type, owl:Class)
entails(cls-nothing2,[classAssertion('owl:Nothing',_X)],[false]).
entails(cls-int1, [equivalentClasses([C,intersectionOf(L)]),
		   pl(classAssertionList(L,X))],[classAssertion(C,X)]).
entails(cls-int2, [equivalentClasses([C,intersectionOf(L)]),
		   classAssertion(C,X),pl(member(C1,L))],[classAssertion(C1,X)]).
entails(cls-int2s, [subClassOf(C,intersectionOf(L)),
		   classAssertion(C,X),pl(member(C1,L))],[classAssertion(C1,X)]).
entails(cls-uni,  [equivalentClasses([C,unionOf(L)]),classAssertion(C1,X),
		   pl(member(C1,L))],[classAssertion(C,X)]).
entails(cls-unis,  [subClassOf([unionOf(L),C]),classAssertion(C1,X),
		   pl(member(C1,L))],[classAssertion(C,X)]).
entails(ls-svf1, [subClassOf(C,someValuesFrom(P,Y)),propertyAssertion(P,X,V),
		  classAssertion(Y,V)],[classAssertyion(C,X)]).
%entails(ls-svf2 ToDo...
entails(cls-avf,[classAssertion(allValuesFrom(P,Y),U),propertyAssertion(P,U,V)],
	[classAssertion(Y,V)]).
entails(cls-hv1, [classAssertion(hasValue(P,V),I)],[propertyAssertion(P,I,V)]).
entails(cls-hv2, [classAssertion(hasValue(P,V),I),propertyAssertion(P,X,I)],[classAssertion(hasValue(P,V),X)]).
entails(cls-maxc1, [classAssertion(maxCardinality(0,P),U),propertyAssertion(P,U,_)],[false]).
entails(cls-maxc2, [classAssertion(maxCardinality(1,P),U),
		    propertyAssertion(P,U,Y1),
		    propertyAssertion(P,U,Y2), pl(Y1 \= Y2)],[sameAs(Y1,Y2)]).
% ToDo entails maxqc1-4
entails(cls-oo,  [subClassOf(_,oneOf(L)),pl(member(X,L))],[classAssertion(oneOf(L),X)]).
entails(cls-oo2,  [subClassOf(_,allValuesFrom(_,oneOf(L))),pl(member(X,L))],[classAssertion(oneOf(L),X)]).
entails(cls-oo3,  [subClassOf(_,someValuesFrom(_,oneOf(L))),pl(member(X,L))],[classAssertion(oneOf(L),X)]).


% Table 7. The Semantics of Class Axioms
entails(cax-sco, [subClassOf(C1,C2),classAssertion(C1,I)],[classAssertion(C2,I)]).
entails(cax-eqc1, [equivalentClasses(L),pl(member(C1,L)),pl(member(C2,L)),pl(C1 \= C2),
		   classAssertion(C1,X)],[classAssertion(C2,X)]).
% rule cax-eqc2 is already satisfied from the member permutations
% of the above.
entails(cax-dw,	[disjointWith(L),pl(member(C1,L)),pl(member(C2,L)),pl(C1 \= C2),
		   classAssertion(C1,X),classAssertion(C2,X)],[false]).
entails(cax-adc, [allDisjointClasses(L),pl(member(C1,L)),pl(member(C2,L)),pl(C1 \= C2),
		   classAssertion(C1,X),classAssertion(C2,X)],[false]).

% Table 8. The Semantics of Datatypes - Nothing to be implemented here.

% Table 9. The Semantics of Schema Vocabulary
entails(scm-cls, [class(C)],[subClassOf(C,C)]). % ToDo also Thing and Nothing
entails(scm-sco, [subClassOf(X,Z),subClassOf(Z,Y)],[subClassOf(X,Y)]).
entails(scm-eqc, [equivalentClasses([C1,C2])],[subClassOf(C1,C2),subClassOf(C2,C1)]).
entails(scm-op,  [objectProperty(P)],[subPropertyOf(P,P)]).
entails(scm-dp,  [datatypePropert(P)],[subPropertyOf(P,P)]).
entails(scm-spo, [subPropertyOf(X,Z),subPropertyOf(Z,Y)],[subPropertyOf(X,Y)]).
entails(scm-eqp, [equivalentProperties([P1,P2])],[subPropertyOf(P1,P2),subPropertyOf(P2,P1)]).
entails(scm-dom1,[propertyDomain(P,C1),subClassOf(C1,C2)],[propertyDomain(P,C2)]).
entails(scm-dom2,[propertyDomain(P2,C),subPropertyOf(P1,P2)],[propertyDomain(P1,C)]).
entails(scm-rng1,[propertyRange(P,C1),subClassOf(C1,C2)],[propertyRange(P,C2)]).
entails(scm-rng2,[propertyRange(P2,C),subPropertyOf(P1,P2)],[propertyRange(P1,C)]).
entails(scm-hv,[subClassOf(_,hasValue(P1,I)),subClassOf(_,hasValue(P2,I),_),subPropertyOf(P1,P2)],
	[subClassOf(hasValue(P1,I),hasValue(P2,I))]).

entails(scm-svf1,[subClassOf(_,someValuesFrom(P,Y1)),subClassOf(_,someValuesFrom(P,Y2)), pl( Y1 \= Y2),
		  subClassOf(Y1,Y2)],[subClassOf(someValuesFrom(P,Y1),someValuesFrom(P,Y2))]).
entails(scm-svf2,[subClassOf(_,someValuesFrom(P1,Y)),subClassOf(_,someValuesFrom(P2,Y)), pl( P1 \= P2),
		  subPropertyOf(P1,P2)],[subPropertyOf(P1,P2)]).
entails(scm-avf1,[subClassOf(_,allValuesFrom(P,Y1)),subClassOf(_,allValuesFrom(P,Y2)), pl(Y1 \= Y2),
		  subClassOf(Y1,Y2)],[subClassOf(allValuesFrom(P,Y1),allValuesFrom(P,Y2))]).
entails(scm-avf2,[subClassOf(_,allValuesFrom(P1,Y)),subClassOf(_,allValuesFrom(P2,Y)), pl(P1 \= P2),
		  subPropertyOf(P1,P2)],[subPropertyOf(P1,P2)]).

entails(scm-int, [subClassOf(C,intersectionOf(L)),pl(member(C1,L))],[subClassOf(C,C1)]).
entails(scm-uni, [subClassOf(C,unionOf(L)),pl(member(C1,L))],[subClassOf(C1,C)]).
*/

% ----------------------------------------
% Utility predicates
% --------------------------------------
classAssertionList([],_).
classAssertionList([C|Rest],X) :-
	classAssertion(C,X),
	classAssertionList(Rest,X).


% ----------------------------------------
% Rule engine
% ----------------------------------------


%%	clear_entailments is det
%       Retracts all entailed/3 facts.

clear_entailments :-
	retractall(entailed(_,_,_)).



%%	is_entailed(+Axiom,-Explanation) is nondet
%	Implements a simple backwards reasoning to find entailments
%	for Axiom.
%	Axiom is entailed if either holds or is a consequent in an
%	entails/3 rule and all the antecedants are entailed.
%
%	Note that the behaviour simulates tabling. If an Axiom
%	has been entailed it is not tried again. This prevents endless
%	loops.

is_entailed(Axiom,Expl) :-
	is_entailed(Axiom,Expl,[],_OutVisited).

is_entailed(Axiom,Expl,In,In) :-
	member(Axiom-Expl,In),!.

is_entailed(A,axiom,In,[A-axiom|In]) :-
	axiom(A).

is_entailed(Axiom,entailed(Rule,Expl),In,Out) :-
	% find rules to be executed.
	entails(Rule,Antecedants,Consequents),	member(Axiom,Consequents),
	debug(rl-rules,'rule matched ~w',Rule),
	are_entailed(Antecedants,Expl,[Axiom|In],Out). 	% resolve rules
	% if resolution succeeds assert axiom if not already there
	% assert_entailment(entailed(Axiom,Rule,Expl)). % backtrack to next rule.


%%	are_entailed(+Axiom:List,-Explanation:List) is nondet
%	Calls is_entailed for all Axioms in list

are_entailed([],[],In,In).
are_entailed([Axiom|Rest],[Expl|RestExpl],In,Out) :-
	is_entailed(Axiom,Expl,In,Out1),
	are_entailed(Rest,RestExpl,Out1,Out).



% Testbed

%entails(r1,[e(X,Y)],[s(X,Y)]).

entails(r2,[subClassOf(X,Z),subClassOf(Z,Y)],[subClassOf(X,Y)]).

g :-
	assert_axiom(subClassOf(a,b)),
	assert_axiom(subClassOf(b,c)),
	assert_axiom(subClassOf(c,b)).

% usage:
% :- owl_parse_rdf('testfiles/wine.owl',[clear(complete)]).



























