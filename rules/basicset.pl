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
	nonvar(I),
        classAssertion(C2,I),
        debug(owl2_basic_reasoner,'testing ~w(~w) based on ~w(~w)',[C,I,C2,I]),
        entailed(subClassOf(C2,C),EL).

entailed_2(classAssertion(C,I),EL) :-
	var(I),
	entailed(subClassOf(C2,C),EL),
        classAssertion(C2,I).


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
	nonvar(X),
	debug(reasoner,'[trans-up] testing for ~w',[subClassOf(X,Y)]),
	entailed_10(subClassOf(X,Z),EL),\+member(X<Z,EL),entailed(subClassOf(Z,Y),[X<Z|EL]). % TODO: cycles
entailed_5(subClassOf(X,Y),EL) :-
	var(X),
	debug(reasoner,'[trans-dn] testing for ~w',[subClassOf(X,Y)]),
	entailed_10(subClassOf(Z,Y),EL),\+member(Z<Y,EL),entailed(subClassOf(X,Z),[Z<Y|EL]). % TODO: cycles


% subclass over existential restrictions
% X < P some Y :- X < P some YY, YY < Y
% TODO - fix this - cause of non-termination
xxxentailed_2(subClassOf(X,Restr), EL) :-
	Restr=someValuesFrom(_,_),
	entailed_5(subClassOf(X,Y),EL),
	subClassOf(Y,Restr).
xxxentailed_2(subClassOf( someValuesFrom(R,X), someValuesFrom(R,Y) ), EL) :-
	\+ ((var(X),var(Y))),
	entailed(subClassOf(X,Y),EL).

