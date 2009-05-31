/* -*- Mode: Prolog -*- */

:- module(owl2_basic_reasoner,
          [
           entailed/1,
           property_assertion_common_ancestor/5,
           property_assertion_least_common_ancestor/5
          ]).

:- use_module(owl2_model).

% ----------------------------------------
% TBox Reasoning
% ----------------------------------------

%% entailed(?Axiom) is nondet
% true if Axiom is entailed
entailed(A) :-
        entailed(A,[]).

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% level 1: subproperties
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

%% entailed(?Axiom,+AlreadyEntailed)

%entailed(A,EL):- debug(owl2_basic_reasoner,'Testing for: ~w Checked: ~w',[A,EL]),fail.

% asserted subprops
entailed(subPropertyOf(X,Y),_) :- subPropertyOf(X,Y).

% transitivity of subprops
entailed(subPropertyOf(X,Y),EL) :- subPropertyOf(X,Z),\+member(X<Z,EL),entailed(subPropertyOf(X,Y),[X<Z|EL]). % TODO: cycles

entailed(subClassOfReflexive(X,Y), EL) :- entailed(subClassOf(X,Y), EL).
entailed(subClassOfReflexive(X,X), _) :- nonvar(X).


entailed(A,EL) :- entailed_2(A,EL).

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% level 2:
%  
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

% X<Y if X=^DL and X<all D in DL
entailed_2(subClassOf(X,Y),EL) :-
        nonvar(Y),
        pairwise_equivalent_class(Y,intersectionOf(DL)),
        \+ member(X<Y,EL),
        forall(member(D,DL),
               entailed(subClassOf(X,D),EL)).

entailed_2(subClassOf(X,Y),EL) :-
        nonvar(Y),
        nonvar(X),
        Y=intersectionOf(DL),
        \+ member(X<Y,EL),
        forall(member(D,DL),
               entailed(subClassOf(X,D),EL)).

entailed_2(subClassOf(intersectionOf(DL),Y),EL) :-
        nonvar(Y),
        \+ member(intersectionOf(DL)<Y,EL),
        member(D,DL),
        entailed(subClassOfReflexive(D,Y)).

entailed_2(subClassOf(someValuesFrom(P,X),someValuesFrom(P,Y)),EL) :-
        entailed(subClassOf(X,Y),EL).

entailed_2(subClassOf(X,Y),_) :-
        pairwise_equivalent_class(X,intersectionOf(DL)),
        member(Y,DL).

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

entailed_2(classAssertion(C,I),EL) :-
        pairwise_equivalent_class(C,intersectionOf(DL)),
        entailed_2(classAssertion(DL,I),EL).

entailed_2(classAssertion(intersectionOf(DL),I),EL) :-
        nonvar(DL),
        entailed(individual(I)),
        forall(member(D,DL),
               entailed_2(classAssertion(D,I),EL)).

entailed_2(classAssertion(someValuesFrom(P,Y),I),EL) :-
        propertyAssertion(P,I,YI),
        entailed(classAssertion(Y,YI),EL).

entailed_2(classAssertion(Y,I),EL) :-
        entailed(individual(I)),
        pairwise_equivalent_class(Y,intersectionOf(DL)),
        forall(member(D,DL),
               entailed(classAssertion(D,I),EL)).

entailed_2(individual(I),_) :-
        propertyAssertion(_,I,_).
entailed_2(individual(I),_) :-
        propertyAssertion(_,_,I).


% NEXT LEVEL
entailed_2(A,EL) :- entailed_5(A,EL).

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% level 5:
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

% asserted
entailed_5(subClassOf(X,Y),_) :- subClassOf(X,Y).

% asserted
entailed_5(classAssertion(C,I),_) :- classAssertion(C,I).

% asserted
entailed_5(individual(I),_) :- individual(I).

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

pairwise_equivalent_class(X,Y) :- equivalentClasses(L),member(X,L),member(Y,L),X\=Y.
pairwise_disjoint_class(X,Y) :- disjointClasses(L),member(X,L),member(Y,L),X\=Y.

% TODO: make subClassOf nothing?
unsatisfiable(X) :-
        pairwise_disjoint_class(A,B),
        entailed(subClassOf(X,A)),
        entailed(subClassOf(X,B)).

% TODO: move this to a utility module
property_assertion_common_ancestor(P,XI,YI,XC,YC) :-
        propertyAssertion(P,XI,YI),
        % TESTING ONLY:
        sub_atom(XI,0,_,_,'http://ccdb.ucsd.edu/SAO/DPO/2.0/DPO.owl'),
        debug(owl2_basic_reasoner,'Testing ~w ~w ~w',[P,XI,YI]),
        entailed(classAssertion(XC,XI)),
        entailed(classAssertion(YC,YI)).

property_assertion_least_common_ancestor(P,XI,YI,XC,YC) :-
        property_assertion_common_ancestor(P,XI,YI,XC,YC),
        \+ ((property_assertion_common_ancestor(P,XI,YI,XC2,YC2),
             entailed(subClassOfReflexive(XC2,XC)),
             entailed(subClassOfReflexive(YC2,YC)),
             (   XC2\=XC ; YC2\=YC))).


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
