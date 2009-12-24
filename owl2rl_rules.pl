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




/*
someValuesFrom_propchain(_,[]).
someValuesFrom_propchain(X,[P|PL]):-
        subClassOf(X,someValuesFrom(P,Y)),
        someValuesFrom_propchain(Y,PL).

subClassOf(X,allValuesFrom(P,D)) :-
        subClassOf(X,allValuesFrom(P,D1)),
        subClassOf(D1,D).

subClassOf_all(_,[]).
subClassOf_all(X,[D|L]):-
        subClassOf(X,D),
        subClassOf_all(X,L).

pairwise_equivalent_class(X,Y) :- equivalentClasses(L),member(X,L),member(Y,L).
pairwise_equivalent_class(X,Y) :- subClassOf(X,Y),subClassOf(Y,X).

pairwise_disjoint_class(X,Y) :- disjointClasses(L),member(X,L),member(Y,L).
pairwise_disjoint_class(X,Y) :- subClassOf(X,Y),subClassOf(Y,X).

% TODO: make subClassOf nothing?
unsatisfiable(X) :-
        pairwise_disjoint_class(A,B),
        subClassOf(X,A),
        subClassOf(X,B).
*/

% ----------------------------------------
% ABox Reasoning
% ----------------------------------------

% reified version of Grosof
/*
classAssertion(C,I) :- classAssertion(C2,I),subClassOf(C2,C).

classAssertion(C,I) :- propertyDomain(P,C),propertyAssertion(P,I,_).
classAssertion(C,I) :- propertyRange(P,C),propertyAssertion(P,_,I).

propertyAssertion(P,A,B) :- transitiveProperty(P),propertyAssertion(P,A,C),propertyAssertion(P,C,B).
propertyAssertion(P,A,B) :- symmetricProperty(P),propertyAssertion(P,B,A).
propertyAssertion(P,A,B) :- inverseProperties(P,Q),propertyAssertion(Q,B,A).
propertyAssertion(Q,A,B) :- subPropertyOf(P,Q),propertyAssertion(P,A,B).

% role chains
propertyAssertion(P,A,B) :- subPropertyOf(propertyChain([P1|PL]),P),propertyAssertion(P,A,C),propertyAssertion_chain(PL,C,B).

propertyAssertion_chain([],_,_).
propertyAssertion_chain([P|PL],A,B) :- propertyAssertion(P,A,C),propertyAssertion_chain(PL,C,B).

% todo: translate to sameIndividuals/1
sameAs(A,B) :- propertyAssertion(P,X,A),functionalProperty(P),propertyAssertion(P,X,B).
sameAs(A,B) :- propertyAssertion(P,A,X),inverseFunctionalProperty(P),propertyAssertion(P,B,X).

illegal(P) :- propertyAssertion(P,A,A),irreflexiveProperty(P).
illegal(P) :- propertyAssertion(P,A,B),asymmetricProperty(P),propertyAssertion(P,B,A).
*/

% ----------------------------------------
% Rule engine
% ----------------------------------------


%%	fire is nondet
%       execute all possible entailments in backtracking

fire_cycle :-
	aggregate_all(count,inferred(_,_,_),C),print('# of inferred before'-C),nl,
	fire,
	aggregate_all(count,inferred(_,_,_),C1),print('# of inferred after'-C1),nl,read(_),
	(   C1 = C -> true ; fire_cycle ).

fire :-
	forall((entails(Rule,Antecedants,Consequents),
	       print(Rule),nl,
	       hold(Antecedants),
	       member(Consequent,Consequents)),
	       assert_u(entailed(Consequent,Rule,Antecedants))).

assert_u(Fact) :- call(Fact),!.
assert_u(Fact) :- assert(Fact).

holds(Axiom) :-
	axiom(Axiom) ; entailed(Axiom,_,_).

hold([]).
hold([Antecedent|Rest]) :-
	holds(Antecedent),
	hold(Rest).


clear :-
	retractall(entailed(_,_,_)).


load_owl :-
	owl_parse_rdf('testfiles/wine.owl',[clear(complete)]).


is_entailed(Axiom,axiom(Axiom)) :-
	axiom1(Axiom).

is_entailed(Axiom,entailed(Rule,Expl)) :-
	entailed(Axiom,Rule,Expl).

is_entailed(Axiom,entailed(Rule,Expl)) :-
	not(entailed(Axiom,_,_)),
	% find rules to be executed.
	entails(Rule,Antecedants,Consequents),	member(Axiom,Consequents),
	% resolve rules, if resolution succeeds,
	are_entailed(Antecedants,Expl),
	% assert axiom if not already there
	not(entailed(Axiom,_,_)),	% put this axiom once but generate all entailments for its children...	
	assert_u(entailed(Axiom,Rule,Expl)). % backtrack to next rule.


are_entailed([],[]).
are_entailed([Axiom|Rest],[Expl|RestExpl]) :-
	is_entailed(Axiom,Expl),
	are_entailed(Rest,RestExpl).

entails(r1,[e(X,Y)],[s(X,Y)]).
entails(r2,[e(X,Z),s(Z,Y)],[s(X,Y)]).

axiom1(e(a,b)).
axiom1(e(b,c)).
axiom1(e(c,b)).
axiom1(e(a,a)).







