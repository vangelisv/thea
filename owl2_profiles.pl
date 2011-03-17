/* -*- Mode: Prolog -*- */

:- module(owl2_profiles,
          [
           ontology_profile/2,
           ontology_profile_violation/3,
           trim_axioms_by_profile/1,
           trim_axioms_by_profile/2,
           owl2_profile/1,
           axiom_profile/2,
           axiom_profile/3
          ]).

:- use_module(owl2_model).
:- use_module(owl2_metamodel).

ontology_profile(Ont,Profile) :-
        owl2_profile(Profile),
        ontology(Ont),
        forall(ontologyAxiom(Ont,Ax),
               axiom_profile(Ax,Profile,true)).

ontology_profile_violation(Ont,Profile,Ax) :-
        owl2_profile(Profile),
        ontology(Ont),
        ontologyAxiom(Ont,Ax),
        \+ axiom_profile(Ax,Profile,true).

trim_axioms_by_profile(Ont,Profile) :-
        ontology(Ont),
        owl2_profile(Profile),
        !,
        findall(Ax,ontology_profile_violation(Ont,Profile,Ax),Axs),
        forall(member(Ax,Axs),
               retract_axiom(Ax,Ont)).
trim_axioms_by_profile(Profile) :-
        owl2_profile(Profile),
        !,
        findall(Ax-Ont,ontology_profile_violation(Ont,Profile,Ax),AxOnts),
        forall(member(Ax-Ont,AxOnts),
               retract_axiom(Ax,Ont)).


%% owl2_profile(?Profile) is nondet
%
% true if Profile is the name of an OWL2 profile
owl2_profile(owl2_EL).
owl2_profile(owl2_QL).
owl2_profile(owl2_RL).

%% construct_profile(+Construct,?Profile,?Passes:boolean)
construct_profile(X,P,T) :- axiom_profile(X,P,T).
construct_profile(X,P,T) :- expression_profile(X,P,T).

%% axiom_profile(+Axiom,?Profile)
%% axiom_profile(+Axiom,?Profile,?Passes:boolean)
axiom_profile(A,P) :- axiom_profile(A,P,true).
:- discontiguous axiom_profile/3.

%% expression_profile(+Axiom,?Profile,?Passes:boolean)
:- discontiguous expression_profile/3.


% generic

expression_profile(C,P,true) :- owl2_profile(P),atom(C),!. % assume OWL2-DL
%expression_profile(C,P,true) :- owl2_profile(P),class(C),!.
%expression_profile(C,P,true) :- owl2_profile(P),property(C),!.
%expression_profile(C,P,true) :- owl2_profile(P),dataRange(C),!.

% ----------------------------------------
% all profiles
% ----------------------------------------
axiom_profile(annotationAssertion(_,_,_),_,true).
axiom_profile(ontology(_),_,true).
axiom_profile(annotationProperty(_),_,true).
axiom_profile(Ax,_,true) :- declarationAxiom(Ax).

% ----------------------------------------
% EL
% ----------------------------------------
% http://www.w3.org/TR/owl2-profiles/#OWL_2_EL

% permitted:
% TODO - recurse down
expression_profile(someValuesFrom(P,X),owl2_EL,true) :- objectProperty(P),expression_profile(X,owl2_EL,true).
expression_profile(hasValue(P,_),owl2_EL,true) :- objectProperty(P).
expression_profile(hasSelf(P),owl2_EL,true) :- objectProperty(P).
expression_profile(oneOf([I]),owl2_EL,true) :- individual(I).
expression_profile(intersectionOf(L),owl2_EL,true) :- forall(member(X,L),expression_profile(X,owl2_EL,true)).
expression_profile(propertyChain(L),owl2_EL,true) :- forall(member(X,L),expression_profile(X,owl2_EL,true)). % inverses..?


expression_profile(allValuesFrom(_P,_),owl2_EL,false).
expression_profile(maxCardinality(_P,_),owl2_EL,false).
expression_profile(minCardinality(_P,_),owl2_EL,false).
expression_profile(exactCardinality(_P,_),owl2_EL,false).
expression_profile(maxCardinality(_P,_,_),owl2_EL,false).
expression_profile(minCardinality(_P,_,_),owl2_EL,false).
expression_profile(exactCardinality(_P,_,_),owl2_EL,false).

% TODO - use a generic visitor style approach
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

% forbidden:

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

% ----------------------------------------
% QL
% ----------------------------------------
                                % http://www.w3.org/TR/owl2-profiles/#OWL_2_QL

% permitted:

subClassExpression(A,owl2_QL,true) :- class(A).
subClassExpression(someValuesFrom(P,'owl:Thing'),owl2_QL,true) :- objectPropertyExpression(P).
subClassExpression(someValuesFrom(P,D),owl2_QL,true) :- dataRange(D),dataPropertyExpression(P).

superClassExpression(A,owl2_QL,true) :- class(A).
superClassExpression(intersectionOf(L),owl2_QL,true) :- forall(member(X,L),superClassExpression(X,owl2_QL,true)).
superClassExpression(complementOf(A),owl2_QL,true) :- subClassExpression(A,owl2_QL,true).
superClassExpression(someValuesFrom(P,A),owl2_QL,true) :- class(A),objectPropertyExpression(P).


axiom_profile(subClassOf(A,B),owl2_QL,true) :- subClassExpression(A,owl2_QL,true),superClassExpression(B,owl2_QL,true).
axiom_profile(equivalentClasses(L),owl2_QL,true) :- forall(member(X,L),subClassExpression(X,owl2_QL,true)).
axiom_profile(disjointClasses(L),owl2_QL,true) :- forall(member(X,L),subClassExpression(X,owl2_QL,true)).
axiom_profile(subPropertyOf(A,B),owl2_QL,true) :- expression_profile(A,owl2_QL,true),expression_profile(B,owl2_QL,true).
axiom_profile(equivalentProperties(L),owl2_QL,true) :- forall(member(X,L),expression_profile(X,owl2_QL,true)).
axiom_profile(disjointProperties(_),owl2_QL,true).
axiom_profile(inverseProperties(_,_),owl2_QL,true).
axiom_profile(propertyRange(A,B),owl2_QL,true) :- objectPropertyExpression(A),superClassExpression(B,owl2_QL,true).
axiom_profile(propertyRange(A,_B),owl2_QL,true) :- dataPropertyExpression(A).
axiom_profile(propertyDomain(_A,B),owl2_QL,true) :- superClassExpression(B,owl2_QL,true).


axiom_profile(reflexiveProperty(_),owl2_QL,true).
axiom_profile(symmetricProperty(_),owl2_QL,true).
axiom_profile(asymmetricProperty(_),owl2_QL,true).

axiom_profile(differentIndividuals(_,_),owl2_QL,true).
axiom_profile(classAssertion(A,_),owl2_QL,true) :- expression_profile(A,owl2_QL,true).
axiom_profile(propertyAssertion(A,_,_),owl2_QL,true) :- expression_profile(A,owl2_QL,true).

expression_profile(someValuesFrom(P,_),owl2_QL,true) :- objectProperty(P).
expression_profile(hasValue(P,_),owl2_QL,true) :- objectProperty(P).
expression_profile(hasSelf(P),owl2_QL,true) :- objectProperty(P).
expression_profile(oneOf([I]),owl2_QL,true) :- individual(I).
expression_profile(intersectionOf(L),owl2_QL,true) :- forall(member(X,L),expression_profile(X,owl2_QL,true)).



expression_profile(allValuesFrom(_P,_),owl2_QL,false).
expression_profile(maxCardinality(_P,_),owl2_QL,false).
expression_profile(minCardinality(_P,_),owl2_QL,false).
expression_profile(exactCardinality(_P,_),owl2_QL,false).
expression_profile(maxCardinality(_P,_,_),owl2_QL,false).
expression_profile(minCardinality(_P,_,_),owl2_QL,false).
expression_profile(exactCardinality(_P,_,_),owl2_QL,false).


% forbidden:

% forbidden expressions:
expression_profile(unionOf(_),owl2_QL,false).
expression_profile(complementOf(_),owl2_QL,false).
expression_profile(oneOf([_,_|_]),owl2_QL,false).
expression_profile(propertyChain(_),owl2_QL,false).

% forbidden axioms:
axiom_profile(transitiveProperty(_),owl2_QL,false).
axiom_profile(functionalProperty(_),owl2_QL,false).
axiom_profile(irreflexiveProperty(_),owl2_QL,false).
%axiom_profile(inverseOf(_),owl2_QL,false).
axiom_profile(inverseFunctionalProperty(_),owl2_QL,false).
axiom_profile(hasKey(_,_),owl2_QL,false).
axiom_profile(negativePropertyAssertion(_,_,_),owl2_QL,false).
axiom_profile(sameIndividuals(_,_),owl2_QL,false).



/** <module> Tests/generates profiles for axioms

---+ Synopsis

  
==
==

---+ Details

http://www.w3.org/TR/owl2-profiles/

---+ Status

  Incomplete - only implemented for owl2_el so far

*/
