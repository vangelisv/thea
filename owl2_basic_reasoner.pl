/* -*- Mode: Prolog -*- */

:- module(owl2_basic_reasoner,
          [
           entailed/1
          ]).

:- use_module(owl2_model).

% ----------------------------------------
% TBox Reasoning
% ----------------------------------------

%% entailed(?Axiom) is nondet
% true if Axiom is entailed
entailed(A) :- entailed(A,[]).

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% level 1: subproperties
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

%% entailed(?Axiom,+AlreadyEntailed)

% asserted subprops
entailed(subPropertyOf(X,Y),_) :- subPropertyOf(X,Y).

% transitivity of subprops
entailed(subPropertyOf(X,Y),EL) :- subPropertyOf(X,Z),\+member(X<Z,EL),entailed(subPropertyOf(X,Y),[X<Z|EL]). % TODO: cycles

entailed(A,EL) :- entailed_2(A,EL).

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% level 2:
%  
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

% 
entailed_2(subClassOf(X,Y),EL) :-
        pairwise_equivalent_class(Y,intersectionOf(DL)),
        \+ member(X<Y,EL),
        forall(member(D,DL),
               entailed_5(subClassOf(X,D),EL)).

entailed_2(subClassOf(X,Y),_) :-
        pairwise_equivalent_class(X,Y).

entailed_2(classAssertion(C,I),EL) :-
        classAssertion(C2,I),
        entailed(subClassOf(C2,C),EL).

entailed_2(classAssertion(C,I),EL) :-
        propertyDomain(P,C),
        entailed(propertyAssertion(P,I,_),EL).

entailed_2(classAssertion(C,I),EL) :-
        propertyRange(P,C),
        entailed(propertyAssertion(P,_,I),EL).


% NEXT LEVEL
entailed_2(A,EL) :- entailed_5(A,EL).

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% level 5:
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

% asserted
entailed_5(subClassOf(X,Y),_) :- subClassOf(X,Y).

% transitivity of subclass
entailed_5(subClassOf(X,Y),EL) :- subClassOf(X,Z),\+member(X<Z,EL),entailed(subClassOf(Z,Y),[X<Z|EL]). % TODO: cycles

% subclass over existential restrictions
% X < P some Y :- X < P some YY, YY < Y
entailed_5(subClassOf(X,someValuesFrom(P,Y)), EL) :-
        subClassOf(X,someValuesFrom(P,YY)),
        subClassOf(YY,Y),
        \+ member(X<someValuesFrom(P,Y),EL).




% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% UTIL
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pairwise_equivalent_class(X,Y) :- equivalentClasses(L),member(X,L),member(Y,L).
pairwise_disjoint_class(X,Y) :- disjointClasses(L),member(X,L),member(Y,L).

% TODO: make subClassOf nothing?
unsatisfiable(X) :-
        pairwise_disjoint_class(A,B),
        entailed(subClassOf(X,A)),
        entailed(subClassOf(X,B)).

% ----------------------------------------
% ABox Reasoning
% ----------------------------------------

% reified version of Grosof

/*
classAssertion(C,I) :- propertyRange(P,C),propertyAssertion(P,_,I).

propertyAssertion(P,A,B) :- transitiveProperty(P),propertyAssertion(P,A,C),propertyAssertion(P,C,B).
propertyAssertion(P,A,B) :- symmetricProperty(P),propertyAssertion(P,B,A).
propertyAssertion(P,A,B) :- inverseOf(P,Q),propertyAssertion(Q,B,A).
propertyAssertion(Q,A,B) :- subPropertyOf(P,Q),propertyAssertion(P,A,B).

% role chains
propertyAssertion(P,A,B) :- subPropertyOf(propertyChain([P1|PL]),P),propertyAssertion(P,A,C),propertyAssertion_chain(PL,C,B).

propertyAssertion_chain([],_,_).
propertyAssertion_chain([P|PL],A,B) :- propertyAssertion(P,A,C),propertyAssertion_chain(PL,C,B).


*/

/** <module> simple backward chaining

  ---+ Synopsis

==
:- use_module(bio(owl2_basic_reasoner)).

% 
demo:-
  nl.
  

==

---+ Details

Use a simple incomplete backward chaining algorithm to find entailed axioms

*/
