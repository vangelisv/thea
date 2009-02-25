/* -*- Mode: Prolog -*- */

:- table subClassOf/2.
:- table member/2.

% ----------------------------------------
% TBox Reasoning
% ----------------------------------------

% transitivity of subprop
subPropertyOf(X,Y) :- subPropertyOf(X,Z),subPropertyOf(Z,Y).

% transitivity of subclass
subClassOf(X,Y) :- subClassOf(X,Z),subClassOf(Z,Y).

subClassOf(X,Y) :-
        pairwise_equivalent_class(X,Y).

subClassOf(X,Y) :-
        pairwise_equivalent_class(X,intersectionOf(L)),
        member(Y,L).

subClassOf(X,Y) :-
        pairwise_equivalent_class(Y,unionOf(L)),
        member(X,L).

subClassOf(X,Y) :-
        pairwise_equivalent_class(Y,intersectionOf(L)),
        subClassOf_all(X,L).

subClassOf(X,someValuesFrom(P,D)) :-
        subClassOf(X,someValuesFrom(P,D1)),
        subClassOf(D1,D).
subClassOf(X,someValuesFrom(P,D)) :-
        subClassOf(X,someValuesFrom(P1,D)),
        subPropertyOf(P1,P).
subClassOf(X,someValuesFrom(P,D)) :-
        subClassOf(X,someValuesFrom(P,D1)),
        transitiveProperty(P),
        subClassOf(D1,someValuesFrom(P,D2)).
subClassOf(X,someValuesFrom(P,D)) :-
        subClassOf(X,someValuesFrom(P1,D1)),
        subPropertyOf(propertyChain([P1,P2]),P), % TODO
        subClassOf(D1,someValuesFrom(P2,D2)).
subClassOf(X,someValuesFrom(P,D)) :-
        subClassOf(X,someValuesFrom(P1,D1)),
        subPropertyOf(propertyChain([P1|PL]),P), % TODO
        someValuesFrom_propchain(D1,PL).

someValuesFrom_propchain(_,[]).
someValuesFrom_propchain(X,[P|PL]):-
        subClassOf(X,someValuesFrom(P,Y)),
        someValuesFrom_propchain(Y,PL).

subClassOf(X,allValuesFrom(P,D)) :-
        subClassOf(X,allValuesFrom(P,D1)),
        subClassOf(D1,D).

:- table subClassOf_all/2.
subClassOf_all(_,[]).
subClassOf_all(X,[D|L]):-
        subClassOf(X,D),
        subClassOf_all(X,L).

:- table pairwise_equivalent_class/2.
pairwise_equivalent_class(X,Y) :- equivalentClasses(L),member(X,L),member(Y,L).
pairwise_equivalent_class(X,Y) :- subClassOf(X,Y),subClassOf(Y,X).

:- table pairwise_disjoint_class/2.
pairwise_disjoint_class(X,Y) :- disjointClasses(L),member(X,L),member(Y,L).
pairwise_disjoint_class(X,Y) :- subClassOf(X,Y),subClassOf(Y,X).

% TODO: make subClassOf nothing?
:- table unsatisfiable/1.
unsatisfiable(X) :-
        pairwise_disjoint_class(A,B),
        subClassOf(X,A),
        subClassOf(X,B).

% ----------------------------------------
% ABox Reasoning
% ----------------------------------------

% reified version of Grosof

classAssertion(C,I) :- classAssertion(C2,I),subClassOf(C2,C).

classAssertion(C,I) :- propertyDomain(P,C),propertyAssertion(P,I,_).
classAssertion(C,I) :- propertyRange(P,C),propertyAssertion(P,_,I).

propertyAssertion(P,A,B) :- transitiveProperty(P),propertyAssertion(P,A,C),propertyAssertion(P,C,B).
propertyAssertion(P,A,B) :- symmetricProperty(P),propertyAssertion(P,B,A).
propertyAssertion(P,A,B) :- inverseOf(P,Q),propertyAssertion(Q,B,A).
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




/** <module> 

  ---+ Synopsis


---+ Details

See http://www.w3.org/TR/2008/WD-owl2-profiles-20081202/#Reasoning_in_OWL_2_RL_and_RDF_Graphs_using_Rules

---+ Additional Information



*/
