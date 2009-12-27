/* -*- Mode: Prolog -*- */

:-module(owl2rl_rules,
	 [fire/0
	 ]).


/** <module> OWL2 RL Rules Implementation

---+ Synopsis

This module implements the OWL2 RL rule-based reasoninng


---+ Details

See http://www.w3.org/TR/2008/WD-owl2-profiles-20081202/#Reasoning_in_OWL_2_RL_and_RDF_Graphs_using_Rules

---+ Additional Information


@author Vangelis Vassiliadis
@license GPL
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
%       execute all possible entailments in backtracking

fire_cycle :-
	aggregate_all(count,entailed(_,_,_),C),print('# of inferred before'-C),nl,
	fire,
	aggregate_all(count,entailed(_,_,_),C1),print('# of inferred after'-C1),nl,read(_),
	(   C1 = C -> true ; fire_cycle ).

fire :-
	forall((entails(Rule,Antecedants,Consequents),
	       print(Rule),nl,
	       hold(Antecedants),
	       member(Consequent,Consequents)),
	       assert_u(entailed(Consequent,Rule,Antecedants))).

assert_u(Fact) :- call(Fact),!.
assert_u(Fact) :- assert(Fact).


holds(Axiom,axiom1(Axiom)) :-
	axiom(Axiom).
holds(Axiom,entailed(Rule,Expl)):-
       entailed(Axiom,Rule,Expl).

hold([]).
hold([Antecedent|Rest]) :-
	holds(Antecedent,_),
	hold(Rest).


clear :-
	retractall(entailed(_,_,_)).


load_owl :-
	owl_parse_rdf('testfiles/wine.owl',[clear(complete)]).

is_entailed(holds(Axiom),Expl) :-
	holds(Axiom,Expl).

is_entailed(Axiom,Expl) :-
	holds(Axiom,Expl).

is_entailed(Axiom,entailed(Rule,Expl)) :-
	not(entailed(Axiom,_,_)),
	% find rules to be executed.
	entails(Rule,Antecedants,Consequents),	member(Axiom,Consequents),
	hold_augment(Axiom,Antecedants,HoldedAntecedants),
	% resolve rules, if resolution succeeds,
	are_entailed(HoldedAntecedants,Expl),
	% assert axiom if not already there
	% not(entailed(Axiom,_,_)),not(axiom1(Axiom)),
	not( holds(Axiom,_)), % put this axiom once but generate all entailments for its children...
	assert_u(entailed(Axiom,Rule,Expl)). % backtrack to next rule.


are_entailed([],[]).
are_entailed([Axiom|Rest],[Expl|RestExpl]) :-
	is_entailed(Axiom,Expl),
	are_entailed(Rest,RestExpl).


hold_augment(_,[],[]):- !.
hold_augment(Axiom,[Ant1|Rest],[holds(Ant1)|Rest]) :-
	functor(Axiom,P,A),functor(Ant1,P,A),!.
hold_augment(_,Antecedants,Antecedants).


%entails(r1,[e(X,Y)],[s(X,Y)]).
entails(r2,[s(X,Z),s(Z,Y)],[s(X,Y)]).

axiom1(s(a,b)).
axiom1(s(b,c)).
axiom1(s(c,b)).





























