/* -*- Mode: Prolog -*- */

:- module(owl2_basic_reasoner,
          [
           entailed/1,
           property_assertion_common_ancestor/5,
           property_assertion_least_common_ancestor/5
          ]).

:- use_module(owl2_model).
:-['rules/basicset'].

:- multifile entailed/2.
:- multifile entailed_2/2.
:- multifile entailed_5/2.

:- discontiguous entailed/2.
:- discontiguous entailed_2/2.
:- discontiguous entailed_5/2.

%% entailed(?Axiom) is nondet
% true if Axiom is entailed
entailed(A) :-
        entailed(A,[]).


entailed(A,EL) :- entailed_2(A,EL).
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
entailed_5(propertyAssertion(P,I,J),_) :- propertyAssertion(P,I,J).

% asserted
entailed_5(individual(I),_) :- individual(I).



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
