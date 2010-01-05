/* -*- Mode: Prolog -*- */

:-module(owl2_rl_rules,
	 [fire/0,
	  fire_cycle/0,

	  clear_entailments/0,
	  is_entailed/2,
	  are_entailed/2

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
%  Database of entails facts representing the rule database
%  it is used by the rule engine to infer new axioms based on existing
%  axioms
%

% transitivity of subprop
entails(scm-spo, [subPropertyOf(X,Z),subPropertyOf(Z,Y)],[subPropertyOf(X,Y)]).

% transitivity of subclass
entails(scm-sco, [subClassOf(X,Z),subClassOf(Z,Y)],[subClassOf(X,Y)]).

entails(cax-sco, [subClassOf(C1,C2),classAssertion(C1,I)],[classAssertion(C2,I)]).

entails(cls-hv1, [classAssertion(hasValue(P,V),I)],[propertyAssertion(P,I,V)]).


% ----------------------------------------
% Rule engine
% ----------------------------------------

%%	fire is nondet
%	Forward chaining engine. Fire executes continuoysly
%	fire_cycle/0 until no new entailments have been added to
%	the database.

fire :-
	aggregate_all(count,entailed(_,_,_),C),% print('# of inferred before'-C),nl,
	fire_cycle,
	aggregate_all(count,entailed(_,_,_),C1),% print('# of inferred after'-C1),nl,read(_),
	(   C1 = C -> true ; fire ).


%%	fire_cycle is nondet
%	Execute all possible entailments based on existing entails/3
%	rules and axiom/1 facts.

fire_cycle :-
	forall((entails(Rule,Antecedants,Consequents),
		hold(Antecedants),
		member(Consequent,Consequents)),
	       assert_u(entailed(Consequent,Rule,Antecedants))
	      ).

%%	clear_entailments is det
%       Retracts all entailed/3 facts.

clear_entailments :-
	retractall(entailed(_,_,_)).


%%	holds(+Axiom,-Explanation) is nondet
%	Axiom holds if either an axiom/1 exists --in which
%	case Explanation binds to axiom(Axiom)-- or Axiom is
%	already entailed by -- Explanation binds to
%	entailed(Rule, Expl).
holds(Axiom,axiom(Axiom)) :-
	axiom(Axiom).
holds(Axiom,entailed(Rule,Expl)):-
	entailed(Axiom,Rule,Expl).


%%	hold(+Axiom:list) is nondet
%       All members of Axiom list hold.
hold([]).
hold([Antecedent|Rest]) :-
	holds(Antecedent,_),
	hold(Rest).


%%	is_entailed(+Axiom,-Explanation) is nondet
%	Implements a simple backwards reasoning to find entailments
%	for Axiom.
%	Axiom is entailed if either holds or is a consequent in an
%	entails/3 rule and all the antecedants are entailed.
%
%	Note that the behaviour simulates tabling. If an Axiom
%	has been entailed it is not tried again. This prevents endless
%	loops.

is_entailed(holds(Axiom),Expl) :-
	holds(Axiom,Expl).

is_entailed(Axiom,Expl) :-
	holds(Axiom,Expl).

is_entailed(Axiom,entailed(Rule,Expl)) :-
	not(entailed(Axiom,_,_)),
	% find rules to be executed.
	entails(Rule,Antecedants,Consequents),	member(Axiom,Consequents),
	hold_augment(Axiom,Antecedants,HoldedAntecedants),
	are_entailed(HoldedAntecedants,Expl), 	% resolve rules
	% if resolution succeeds assert axiom if not already there
	not( holds(Axiom,_)), % put this axiom once but generate all entailments for its children...
	assert_u(entailed(Axiom,Rule,Expl)). % backtrack to next rule.


%%	are_entailed(+Axiom:List,-Explanation:List) is nondet
%	Calls is_entailed for all Axioms in list

are_entailed([],[]).
are_entailed([Axiom|Rest],[Expl|RestExpl]) :-
	is_entailed(Axiom,Expl),
	are_entailed(Rest,RestExpl).


%%	hold_augment(+Axiom,+Antecedants,-AugmentedAntecedants) is det
%       If Axiom matches first element of Antecedants (Ant1) then
%	Ant1 is augmented as holds(Ant1).
%
%	Enables write a rule as S:- S,S1. The antecedant S should hold
%       already: is_entailed will not try to resolve it.

hold_augment(_,[],[]):- !.
hold_augment(Axiom,[Ant1|Rest],[holds(Ant1)|Rest]) :-
	functor(Axiom,P,A),functor(Ant1,P,A),!.
hold_augment(_,Antecedants,Antecedants).


assert_u(Fact) :- call(Fact),!.
assert_u(Fact) :- assert(Fact).


/*
Testbed

%entails(r1,[e(X,Y)],[s(X,Y)]).
entails(r2,[s(X,Z),s(Z,Y)],[s(X,Y)]).

axiom1(s(a,b)).
axiom1(s(b,c)).
axiom1(s(c,b)).

% usage:
:- owl_parse_rdf('testfiles/wine.owl',[clear(complete)]).

*/




























