/* -*- Mode: Prolog -*- */

% ----------------------------------------
% TBox Reasoning
% ----------------------------------------




% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% level 1: subproperties, reflexivity
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

%% entailed(?Axiom,+AlreadyEntailed)

%entailed(A,EL):- debug(owl2_basic_reasoner,'Testing for: ~w Checked: ~w',[A,EL]),fail.

% asserted subprops
entailed(subPropertyOf(X,Y),_) :- subPropertyOf(X,Y).

% transitivity of subprops
entailed(subPropertyOf(X,Y),EL) :- \+member(X<Z,EL),subPropertyOf(X,Z),entailed(subPropertyOf(X,Y),[X<Y|EL]). % TODO: cycles

entailed(subClassOfReflexive(X,Y), EL) :- entailed(subClassOf(X,Y), EL).
entailed(subClassOfReflexive(X,X), _) :- class(X).


entailed_2(classAssertion(C,I),EL) :-
        classAssertion(C2,I),
        debug(owl2_basic_reasoner,'testing ~w(~w) based on ~w(~w)',[C,I,C2,I]),
        entailed(subClassOf(C2,C),EL).


entailed_2(individual(I),_) :-
        propertyAssertion(_,I,_).
entailed_2(individual(I),_) :-
        propertyAssertion(_,_,I).

entailed_2(propertyAssertion(P,A,B), EL) :-
        inverseProperties(P,Q),
        entailed_5(propertyAssertion(Q,B,A),EL).

entailed_2(propertyAssertion(P,A,B), EL) :-
        \+ member(P-A-B,EL),
        transitiveProperty(P),
        EL2 = [P-A-Z|EL],
        entailed_5(propertyAssertion(P,A,Z),EL2),
        entailed_2(propertyAssertion(P,Z,B),[P-Z-B|EL2]).



% transitivity of subclass
% we avoid recursion by stratification - 10 cannot call a 5
% 10 is either asserted or a precalculated set of assertions based on eg equivalentClass
entailed_5(subClassOf(X,Y),EL) :-
	debug(reasoner,'[trans] testing for ~w',[subClassOf(X,Y)]),
	entailed_10(subClassOf(X,Z),EL),\+member(X<Z,EL),entailed(subClassOf(Z,Y),[X<Z|EL]). % TODO: cycles


% subclass over existential restrictions
% X < P some Y :- X < P some YY, YY < Y
% TODO - fix this - cause of non-termination
xxxxxxxxentailed_5(subClassOf(X,someValuesFrom(P,Y)), EL) :-
	class(X),
	debug(reasoner,'testing for ~w',[subClassOf(X,someValuesFrom(P,Y))]),
        subClassOf(X,someValuesFrom(P,YY)),
	debug(reasoner,'  testing for ~w',[subClassOf(YY,Y)]),
	class(YY),
        entailed(subClassOf(YY,Y),[P-X-YY|EL]),
	class(Y),
        \+ member(X<someValuesFrom(P,Y),EL).


% e.g. if X = A and B, then treat this like X subClassOf A, X subClassOf B
% this is v. slow when X is nonvar
entailed_pre(subClassOf(X,Y)) :-
        pairwise_equivalent_class(X,intersectionOf(DL)),
        atom(X),
        member(Y,DL).
