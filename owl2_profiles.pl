/* -*- Mode: Prolog -*- */

:- module(owl2_profiles,
          [
           owl2_profile/1,
           axiom_profile/2,
           axiom_profile/3
          ]).

:- use_module(owl2_model).
:- use_module(owl2_metamodel).


%% owl2_profile(?Profile) is nondet
owl2_profile(owl2_EL).
%owl2_profile(owl2_QL).
%owl2_profile(owl2_RL).

%% construct_profile(+Construct,?Profile,?Passes:boolean)
construct_profile(X,P,T) :- axiom_profile(X,P,T).
construct_profile(X,P,T) :- expression_profile(X,P,T).

%% axiom_profile(+Axiom,?Profile,?Passes:boolean)
:- discontiguous axiom_profile/3.

%% expression_profile(+Axiom,?Profile,?Passes:boolean)
:- discontiguous expression_profile/3.


% generic

expression_profile(C,P,true) :- owl2_profile(P),class(C).
expression_profile(C,P,true) :- owl2_profile(P),property(C).
expression_profile(C,P,true) :- owl2_profile(P),dataRange(C).

% http://www.w3.org/TR/owl2-profiles/#OWL_2_EL

% TRUE

expression_profile(someValuesFrom(P,_),owl2_EL,true) :- objectProperty(P).
expression_profile(hasValue(P,_),owl2_EL,true) :- objectProperty(P).
expression_profile(hasSelf(P),owl2_EL,true) :- objectProperty(P).
expression_profile(oneOf([I]),owl2_EL,true) :- individual(I).
expression_profile(intersectionOf(L),owl2_EL,true) :- forall(member(X,L),expression_profile(X,owl2_EL,true)).
expression_profile(propertyChain(L),owl2_EL,true) :- forall(member(X,L),expression_profile(X,owl2_EL,true)). % inverses..?


expression_profile(allValuesFrom(P,_),owl2_EL,false).
expression_profile(maxCardinality(P,_),owl2_EL,false).
expression_profile(minCardinality(P,_),owl2_EL,false).
expression_profile(exactCardinality(P,_),owl2_EL,false).
expression_profile(maxCardinality(P,_,_),owl2_EL,false).
expression_profile(minCardinality(P,_,_),owl2_EL,false).
expression_profile(exactCardinality(P,_,_),owl2_EL,false).

axiom_profile(subClassOf(A,B),owl2_EL,true) :- expression_profile(A,owl2_EL,true),expression_profile(B,owl2_EL,true).
axiom_profile(equivalentClasses(L),owl2_EL,true) :- forall(member(X,L),expression_profile(X,owl2_EL,true)).
axiom_profile(disjointClasses(L),owl2_EL,true) :- forall(member(X,L),expression_profile(X,owl2_EL,true)).
axiom_profile(subPropertyOf(A,B),owl2_EL,true) :- expression_profile(A,owl2_EL,true),expression_profile(B,owl2_EL,true).
axiom_profile(equivalentProperties(L),owl2_EL,true) :- forall(member(X,L),expression_profile(X,owl2_EL,true)).
axiom_profile(transitiveProperty(_),owl2_EL,true).
axiom_profile(reflexiveProperty(_),owl2_EL,true).
axiom_profile(propertyDomain(A,B),owl2_EL,true) :- expression_profile(A,owl2_EL,true),expression_profile(B,owl2_EL,true).
axiom_profile(propertyRange(A,B),owl2_EL,true) :- expression_profile(A,owl2_EL,true),expression_profile(B,owl2_EL,true).
axiom_profile(sameIndividuals(_,_),owl2_EL,true).
axiom_profile(differentIndividuals(_,_),owl2_EL,true).
axiom_profile(classAssertion(A,_),owl2_EL,true) :- expression_profile(A,owl2_EL,true).
axiom_profile(propertyAssertion(A,_,_),owl2_EL,true) :- expression_profile(A,owl2_EL,true).
axiom_profile(negativePropertyAssertion(A,_,_),owl2_EL,true) :- expression_profile(A,owl2_EL,true).
axiom_profile(functionalProperty(P),owl2_EL,true) :- dataProperty(P).
% TODO hasKey
axiom_profile(hasKey(_,_),owl2_EL,true).

% FALSE

% forbidden expressions:
expression_profile(unionOf(_),owl2_EL,false).
expression_profile(complementOf(_),owl2_EL,false).
expression_profile(oneOf([_,_|_]),owl2_EL,false).

% forbidden axioms:
axiom_profile(disjointProperties(_),owl2_EL,false).
axiom_profile(irreflexiveProperty(_),owl2_EL,false).
axiom_profile(inverseOf(_),owl2_EL,false).
axiom_profile(inverseProperties(_,_),owl2_EL,false).
axiom_profile(functionalProperty(P),owl2_EL,false) :- objectProperty(P).
axiom_profile(inverseFunctionalProperty(_),owl2_EL,false).
axiom_profile(symmetricProperty(_),owl2_EL,false).
axiom_profile(asymmetricProperty(_),owl2_EL,false).



/** <module> Tests/generates profiles for axioms

---+ Synopsis

  
==
==

---+ Details

http://www.w3.org/TR/owl2-profiles/

---+ Status

  Incomplete - only implemented for owl2_el so far

*/
