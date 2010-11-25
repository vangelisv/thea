/* -*- Mode: Prolog -*- */


:-module(owl2_rl_rules,
	 [
	  is_entailed/2,

	  get_tbox_entailments/0,
	  set_tbox/1,
	  clear_entailments/0
	 ]).


/** <module> OWL2 RL Rules Implementation

---+ Synopsis

This module implements the OWL2 RL rule-based reasoninng

---+ Details

See http://www.w3.org/TR/2008/WD-owl2-profiles-20081202/#Reasoning_in_OWL_2_RL_and_RDF_Graphs_using_Rules

---+ Additional Information


@author Vangelis Vassiliadis
@license GPL
@tbd
 Ontology - specific reasoning: Execute entailments only for axioms of
 specific Ontology based on axiom/2 facts.

*/


:- use_module(owl2_from_rdf).
:- use_module(owl2_model).

:- dynamic entails/3.
:- dynamic entailment/2.
:- dynamic tbox/1.

:- discontiguous is_entailed/2.
:- discontiguous is_entailed/3.

t(Axiom,Count) :- time(aggregate_all(count,(is_entailed(Axiom,_Expl)),Count)).

t(Count) :- time(aggregate_all(count,(is_entailed(_Axiom,_Expl)),Count)).

tbox_axiom_pred(subClassOf/2).
tbox_axiom_pred(equivalentClasses/1).
tbox_axiom_pred(subPropertyOf/2).
tbox_axiom_pred(equivalentProperties/1).
tbox_axiom_pred(propertyDomain/2).
tbox_axiom_pred(propertyRange/2).


clear_entailments :-
	retractall(entailment(_,_)).

get_tbox_entailments :-
	clear_entailments,
	set_tbox(none),
	forall((tbox_axiom_pred(F/A),functor(Pred,F,A),is_entailed(Pred,Expl)),
	       u_assert(entailment(Pred,Expl))),
	set_tbox(saved).

set_tbox(X) :-
	retractall(tbox(_)),
	assert(tbox(X)).


u_assert(X) :-
	call(X),!.
u_assert(X) :-
        debug(rl_rules,'asserting : ~w',[X]),
        assert(X).



% -----------------------------
%

%% is_entailed(?Axiom,?Expl) is semidet
is_entailed(Axiom,Expl) :-
	tbox(saved),!,
	tbox_axiom_pred(F/A),
	functor(Axiom,F,A),
	entailment(Axiom,Expl).


is_entailed(subClassOf(X,Y),Expl) :-
	is_entailed(subClassOf(X,Y),Expl,[X]).

is_entailed(subClassOf(X,X),axiom(class(X)),_) :- axiom(class(X)).

is_entailed(subClassOf(X,Y),axiom(equivalentClasses-sym([X,Y])),_) :-
	eq_classes(X,Y).

is_entailed(subClassOf(X,Y),axiom(subClassOf(X,Y)),_Visited) :-
	axiom(subClassOf(X,Y)).

is_entailed(subClassOf(X,Y),scm-sco(axiom(subClassOf(X,Z)),Expl),Visited) :-
	axiom(subClassOf(X,Z)),not(member(Z,Visited)),
	is_entailed(subClassOf(Z,Y),Expl,[X,Z|Visited]).


is_entailed(subClassOf(C1,C2),scm-int(E1),_Visited) :-
	(   axiom(subClassOf(C1,intersectionOf(L))),E1=axiom(subClassOf(C1,intersectionOf(L))) ;
	axiom(equivalentClasses([C1,intersectionOf(L)])), E1 = axiom(equivalentClasses([C1,intersectionOf(L)])) ;
	axiom(equivalentClasses([intersectionOf(L),C1])), E1 = axiom(equivalentClasses([C1,intersectionOf(L)]))),
	% class_exp(intersectionOf(L)),
	member(C2,L).
	% is_entailed(subClassOf(C1,intersectionOf(L)),E1,[scm-int(C1)|Visited]).


is_entailed(subClassOf(C1,unionOf(L)),scm-uni(unionOf(L)),_Visited) :-
	class_exp(unionOf(L)),member(C1,L).


is_entailed(subClassOf(hasValue(P1,I),hasValue(P2,I)),scm-hv(class_exp(hasValue(P1,I)),
							     class_exp(hasValue(P2,I)),
							     E3),Visited) :-
	class_exp(hasValue(P1,I)),
	class_exp(hasValue(P2,I)),
	not(member(hv(P2),Visited)),
	is_entailed(subPropertyOf(P1,P2),E3,[hv(P1)|Visited]).


is_entailed(subClassOf(someValuesFrom(P,Y1),someValuesFrom(P,Y2)),
	    scm-svf1(class_exp(someValuesFrom(P,Y1)),
		     class_exp(someValuesFrom(P,Y2)),
		     E3),Visited) :-
	class_exp(someValuesFrom(P,Y1)),
	class_exp(someValuesFrom(P,Y2)),Y2 \= Y1,
	not(member(svf(Y2),Visited)),
	is_entailed(subClassOf(Y1,Y2),E3,[svf(Y1)|Visited]).


is_entailed(subClassOf(allValuesFrom(P,Y1),allValuesFrom(P,Y2)),
	    scm-avf1(class_exp(allValuesFrom(P,Y1)),
		     class_exp(allValuesFrom(P,Y2)),
		     E3),Visited) :-
	class_exp(allValuesFrom(P,Y1)),
	class_exp(allValuesFrom(P,Y2)),Y2 \= Y1,
	not(member(avf(Y2),Visited)),
	is_entailed(subClassOf(Y1,Y2),E3,[avf(Y1)|Visited]).


is_entailed(subClassOf(someValuesFrom(P1,Y),someValuesFrom(P2,Y)),
	    scm-svf2(class_exp(someValuesFrom(P1,Y)),
		     class_exp(someValuesFrom(P2,Y)),
		     E3),Visited) :-
	class_exp(someValuesFrom(P1,Y)),
	class_exp(someValuesFrom(P2,Y)),P2 \= P1,
	not(member(svf2(P2),Visited)),
	is_entailed(subPropertyOf(P1,P2),E3,[svf2(P1)|Visited]).

is_entailed(subClassOf(allValuesFrom(P1,Y),allValuesFrom(P2,Y)),
	    scm-avf1(class_exp(allValuesFrom(P1,Y)),
		     class_exp(allValuesFrom(P2,Y)),
		     E3),Visited) :-
	class_exp(allValuesFrom(P1,Y)),
	class_exp(allValuesFrom(P2,Y)),P2 \= P1,
	not(member(avf2(P2),Visited)),
	is_entailed(subPropertyOf(P1,P2),E3,[avf2(P1)|Visited]).


is_entailed(equivalentClasses([X,Y]),Expl) :-
	is_entailed(equivalentClasses([X,Y]),Expl,[X]).

is_entailed(equivalentClasses([X,X]),axiom(equivalentClasses([X,X])),[X]) :- axiom(class(X)).
is_entailed(equivalentClasses([X,Y]),axiom(equivalentClasses([X,Y])),Visited) :-
	axiom(equivalentClasses([X,Y])), not(member(Y,Visited)).

is_entailed(equivalentClasses([X,Y]),axiom(equivalentClasses([X,Y])),Visited) :-
	axiom(equivalentClasses([Y,X])), not(member(Y,Visited)).

is_entailed(equivalentClasses([X,Y]),eq-sym(axiom(equivalentClasses([X,Z])),Expl),Visited) :-
	axiom(equivalentClasses([X,Z])),not(member(Z,Visited)),
	is_entailed(equivalentClasses([Z,Y]),Expl,[Z|Visited]).



is_entailed(sameIndividual([X,Y]),Expl) :-
	var(X),
	is_entailed(sameIndividual([Y,X]),Expl,[Y]).

is_entailed(sameIndividual([X,Y]),Expl) :-
	nonvar(X),
	is_entailed(sameIndividual([X,Y]),Expl,[X]).


is_entailed(sameIndividual([X,Y]),axiom(sameIndividual([X,Y])),Visited) :-
	axiom(sameIndividual([X,Y])), not(member(Y,Visited)).

is_entailed(sameIndividual([X,Y]),axiom(sameIndividual([X,Y])),Visited) :-
	axiom(sameIndividual([Y,X])), not(member(Y,Visited)).

is_entailed(sameIndividual([X,Y]),eq-sym(axiom(sameIndividual([X,Z])),Expl),Visited) :-
	axiom(sameIndividual([X,Z])),not(member(Z,Visited)),
	is_entailed(sameIndividual([Z,Y]),Expl,[Z|Visited]).


is_entailed(sameIndividual([Y1,Y2]),prp-fp(E1,E2),Visited) :-
	not(member(prp-fp(Y1),Visited)), not(member(prp-fp(Y2),Visited)),
	axiom(functionalProperty(P)),
	is_entailed(propertyAssertion(P,X,Y1),E1,[prp-fp(Y1)|Visited]),
	is_entailed(propertyAssertion(P,X,Y2),E2,[prp-fp(Y2)|Visited]),
	Y1 \= Y2.


is_entailed(sameIndividual([X1,X2]),prp-ifp(E1,E2),Visited) :-
	not(member(prp-ifp(X1),Visited)), not(member(prp-ifp(X2),Visited)),
	axiom(inverseFunctionalProperty(P)),
	is_entailed(propertyAssertion(P,X1,Y),E1,[prp-ifp(X1)|Visited]),
	is_entailed(propertyAssertion(P,X2,Y),E2,[prp-ifp(X2)|Visited]).


is_entailed(sameIndividual([Y1,Y2]),cls-maxc2(E1,E2,E3)) :-
	(   axiom(equivalentClasses([C,maxCardinality(1,P)]))
	;
	    axiom(subClassOf(C,maxCardinality(1,P)))
	),
	is_entailed(classAssertion(C,U),E1),
	is_entailed(propertyAssertion(P,U,Y1),E2),
	is_entailed(propertyAssertion(P,U,Y2),E3).



is_entailed(propertyAssertion(P,X,V),Expl) :-
	is_entailed(propertyAssertion(P,X,V),Expl,[]).

is_entailed(propertyAssertion(P,X,V),axiom(propertyAssertion(P,X,V)),Visited) :-
	axiom(propertyAssertion(P,X,V)),not(member(P-X-V,Visited)).

is_entailed(propertyAssertion(P,X,Y),prp-symp(propertyAssertion(P,Y,X)),Visited) :-
	axiom(symmetricProperty(P)),
	axiom(propertyAssertion(P,Y,X)), not(member(P-Y-X,Visited)).

is_entailed(propertyAssertion(P,I1,V),eq-rep-s(Expl1,Expl2),Visited) :-
	 is_entailed(sameIndividual([I1,I2]),Expl1),
	 not(member(eq-rep-s(I2),Visited)),
	 debug(rl-rules,'pa(P,I1,V1) eq-rep-s I2 ~w ~w ~w ~w',[P,I1,V,I2]),
	 is_entailed(propertyAssertion(P,I2,V),Expl2,[eq-rep-s(I1)|Visited]).

is_entailed(propertyAssertion(P,X,V2),eq-rep-o(Expl1,Expl2),Visited) :-
	 nonvar(V2), % include this 2/6/10 for optimisation -- but not complete!
	 is_entailed(sameIndividual([V1,V2]),Expl1),
	 not(member(eq-rep-o(V2),Visited)),
	 is_entailed(propertyAssertion(P,X,V1),Expl2,[eq-rep-o(V1)|Visited]).

is_entailed(propertyAssertion(P,X,Y),prp-trp(axiom(propertyAssertion(P,X,Z)),Expl),Visited) :-
	axiom(transitiveProperty(P)),
	axiom(propertyAssertion(P,X,Z)),not(member(prp-trp(Z),Visited)),
	is_entailed(propertyAssertion(P,Z,Y),Expl,[prp-trp(Z)|Visited]).

is_entailed(propertyAssertion(P2,X,Y),prp-spo1(subPropertyOf(P1,P2),Expl),Visited) :-
	axiom(subPropertyOf(P1,P2)),
	not(member(prp-spo1(P2),Visited)),
	is_entailed(propertyAssertion(P1,X,Y),Expl,[prp-spo1(P1)|Visited]).


is_entailed(propertyAssertion(P2,X,Y),prp-eqp1(equivalentProperties([P1,P2]),Exp2),Visited):-
	eq_properties(P1,P2),
	not(member(prp-eqp1(P2),Visited)),
	is_entailed(propertyAssertion(P1,X,Y),Exp2,[prp-eqp1(P1)|Visited]).

is_entailed(propertyAssertion(P2,X,Y),prp-inv1(inverseOf([P1,P2]),Exp2),Visited):-
	inv_properties(P1,P2),
	not(member(prp-inv1(P2),Visited)),
	is_entailed(propertyAssertion(P1,X,Y),Exp2,[prp-inv1(P1)|Visited]).

is_entailed(propertyAssertion(P,I,V),cls-hv1(Exp1)):-
	% not(member(cls-hv1(P,V),Visited)),
	is_entailed(classAssertion(hasValue(P,V),I),Exp1).


eq_properties(P1,P2) :-
	(   axiom(equivalentProperties([P1,P2])),!;axiom(equivalentProperties([P2,P1]))).

inv_properties(P1,P2) :-
	(   axiom(inverseProperties(P1,P2));axiom(inverseProperties(P2,P1))).

eq_classes(C1,C2) :-
	(   axiom(equivalentClasses([C1,C2])),! ; axiom(equivalentClasses([C2,C1]))).

class_exp(Exp) :-
	(   axiom(subClassOf(_,Exp)) ; axiom(subClassOf(Exp,_)) ;
	axiom(equivalentClasses(E)), member(Exp,E)).


is_entailed(false,eq-diff1) :-
	axiom(sameIndividual(X,Y)),
	(   axiom(differentIndividuals([X,Y])) ; axiom(differentIndividuals([Y,X]))).

is_entailed(false,prp-irp(axiom(irreflexive(P)),E)) :-
	axiom(ireflexiveProperty(P)),
	is_entailed(propertyAssertion(P,X,X),E).

is_entailed(false,prp-asyp(E1,E2)) :-
	axiom(assymetricProperty(P)),
	is_entailed(propertyAssertion(P,X,Y),E1),
	is_entailed(propertyAssertion(P,Y,X),E2).

is_entailed(false,prp-pdw(axiom(disjointProperties([P1,P2])),E2,E3)) :-
       axiom(disjointProperties([P1,P2])),
       is_entailed(propertyAssertion(P1,X,Y),E2),
       is_entailed(propertyAssertion(P2,X,Y),E3).

is_entailed(false,prp-adp(axiom(disjointProperties([P1,P2])),E2,E3)) :-
	axiom(disjointProperties([P1,P2])),
	is_entailed(propertyAssertion(P1,X,Y),E2),
	is_entailed(propertyAssertion(P2,X,Y),E3).

is_entailed(false,cls-nothing2,axiom(classAssertion('owl:Nothing',X))) :-
	axiom(classAssertion('owl:Nothing',X)).


is_entailed(classAssertion(C,X),E) :-
	is_entailed(classAssertion(C,X),E,[]).

is_entailed(classAssertion(C,X),axiom(classAssertion(C,X)),Visited) :-
	axiom(classAssertion(C,X)),not(member(C-X,Visited)).



is_entailed(classAssertion(C2,X),cls-int1(E1,E2),Visited) :-
	is_entailed(subClassOf(C1,intersectionOf(L)),E1),
	member(C2,L),C2 \= C1, not(member(sc(C1),Visited)),
	is_entailed(classAssertion(C1,X),E2,[sc(C2)|Visited]).


is_entailed(classAssertion(unionOf(L),X),cls-uni(unionOf(L),E2),Visited) :-
	class_exp(unionOf(L)),
	member(C1,L),not(member(sc(C1),Visited)),
	is_entailed(classAssertion(C1,X),E2,[sc(unionOf(L))|Visited]).

is_entailed(classAssertion(C,X),oneOf(E1,E2),_):-
	class_exp(oneOf(L)),member(X,L),
	is_entailed(subClassOf(oneOf(L),C1),E1),
	is_entailed(subClassOf(C1,C),E2).


is_entailed(classAssertion(someValuesFrom(P,C1),X),cls-svf1(E2,E3),Visited) :-
	(   axiom(equivalentClasses([_,someValuesFrom(P,C1)]))
	;
	    axiom(subClassOf(_,someValuesFrom(P,C1)))
	),
	is_entailed(propertyAssertion(P,X,V),E2),
	not(member(cls-svf1(C1),Visited)),
	is_entailed(classAssertion(C1,V),E3,[cls-svf1(C1)|Visited]).

is_entailed(classAssertion(Y,V),cls-avf(subClassOf(C,allValuesFrom(P,Y)),E1,E2),Visited) :-
	(   axiom(equivalentClasses([C,allValuesFrom(P,Y)]))
	;
	    axiom(subClassOf(C,allValuesFrom(P,Y)))
	),
	is_entailed(propertyAssertion(P,U,V),E1),
	not(member(cls-avf(P,Y),Visited)),
	is_entailed(classAssertion(C,U),E2,[cls-avf(P,Y)|Visited]).


is_entailed(classAssertion(C2,I),cax-sco-eqc(E1,E2),Visited) :-
	is_entailed(subClassOf(C1,C2),E1),C1 \= C2 ,
	not(member(sc(C1),Visited)),
	is_entailed(classAssertion(C1,I),E2,[sc(C2)|Visited]).

is_entailed(classAssertion(C,Y),prp-rng(E1,E2)) :-
	is_entailed(propertyRange(P,C),E1),
	is_entailed(propertyAssertion(P,_,Y),E2,[]).

is_entailed(classAssertion(C,X),prp-dom(E1,E2)) :-
	is_entailed(propertyDomain(P,C),E1),
	is_entailed(propertyAssertion(P,X,_),E2,[]).



is_entailed(subPropertyOf(X,Y),Expl) :-
	is_entailed(subPropertyOf(X,Y),Expl,[X]).

is_entailed(subPropertyOf(X,X),axiom(objectProperty(X)),[X]) :- axiom(objectProperty(X)).

is_entailed(subPropertyOf(X,X),axiom(datatypeProperty(X)),[X]) :- axiom(datatypeProperty(X)).

is_entailed(subPropertyOf(X,Y),axiom(equivalentProperties-sym([X,Y])),_) :-
	eq_properties(X,Y).

is_entailed(subPropertyOf(X,Y),axiom(subPropertyOf(X,Y)),_Visited) :-
	axiom(subPropertyOf(X,Y)).

is_entailed(subPropertyOf(X,Y),scm-spo(axiom(subPropertyOf(X,Z)),Expl),Visited) :-
	axiom(subPropertyOf(X,Z)),not(member(sp(Z),Visited)),
	is_entailed(subPropertyOf(Z,Y),Expl,[sp(Z)|Visited]).


is_entailed(propertyDomain(P,C),Expl) :-
	is_entailed_pd1(propertyDomain(P,C),Expl);
	is_entailed_pd2(propertyDomain(P,C),Expl).


is_entailed_pd1(propertyDomain(P,D2),scm-dom1(E1,E2)) :-
	axiom(propertyDomain(P,D)),E1 = axiom(propertyDomain(P,D)),
	is_entailed(subClassOf(D2,D),E2).

is_entailed_pd2(propertyDomain(P1,D),scm-dom2(E1,E2)) :-
	axiom(propertyDomain(P,D)),E1 = axiom(propertyDomain(P,D)),
	is_entailed(subPropertyOf(P1,P),E2).


is_entailed(propertyRange(P,C),Expl) :-
	is_entailed_pr1(propertyRange(P,C),Expl);
	is_entailed_pr2(propertyRange(P,C),Expl).

is_entailed_pr1(propertyRange(P,C2),scm-rng1(E1,E2)) :-
	axiom(propertyRange(P,C)),E1 = axiom(propertyRange(P,C)),
	is_entailed(subClassOf(C2,C),E2).


is_entailed_pr2(propertyRange(P1,C),scm-rng2(E1,E2)) :-
	axiom(propertyRange(P,C)),E1 = axiom(propertyRange(P,C)),
	is_entailed(subPropertyOf(P1,P),E2).


%% entails(+RuleID,+Antecedents:list, +Consequents:list) is nondet
%
%  Database of entails facts representing the rule database
%  it is used by the rule engine to infer new axioms based on existing
%  axioms
%

% Semantics of Equality (Table 4)



% ----------------------------------------
% Rule Engine
% ----------------------------------------

%% entails(+RuleID,+Antecedents:list, +Consequents:list) is nondet
%
%  Database of entails facts representing the rule database%  it is used by the rule engine to infer new axioms based on existing
%  axioms
%

% Semantics of Equality (Table 4)

entails(eq-sym, [sameAs(X,Y)],[sameAs(Y,X)]).
entails(eq-trans, [sameAs(X,Z),sameAs(Z,Y)],[sameAs(X,Y)]).

entails(eq-rep-s,[sameAs(X,Y),propertyAssertion(P,X,V)],[propertyAssertion(P,Y,V)]).
entails(eq-rep-p,[sameAs(P1,P2),propertyAssertion(P1,X,V)],[propertyAssertion(P2,X,V)]).
entails(eq-rep-o,[sameAs(V1,V2),propertyAssertion(P,X,V1)],[propertyAssertion(P,X,V2)]).
entails(eq-diff1,[sameAs(X,Y),differentFrom(X,Y)],[false]).
% entails(eq-diff2, Todo...

% Table 5. The Semantics of Axioms about Properties
entails(prp-dom, [propertyDomain(P,C),propertyAssertion(P,X,_)],[classAssertion(C,X)]).
entails(prp-rng, [propertyRange(P,C),propertyAssertion(P,_,Y)],[classAssertion(C,Y)]).
entails(prp-fp,  [functionalProperty(P),propertyAssertion(P,X,Y1),propertyAssertion(P,X,Y2),pl(Y1 \= Y2)],[sameAs(Y1,Y2)]).
entails(prp-ifp, [inverseFunctionalProperty(P),propertyAssertion(P,X1,Y),propertyAssertion(P,X2,Y)],[sameAs(X1,X2)]).
entails(prp-irp, [irreflexiveProperty(P),propertyAssertion(P,X,X)],[false]).
entails(prp-symp,[symmetricProperty(P),propertyAssertion(P,X,Y)],[propertyAssertion(P,Y,X)]).
entails(prp-asyp,[asymmetricProperty(P),propertyAssertion(P,X,Y),propertyAssertion(P,Y,X)],[false]).
entails(prp-trp, [transitiveProperty(P),propertyAssertion(P,X,Y),propertyAssertion(P,Y,Z)],[propertyAssertion(P,X,Z)]).
entails(prp-spo1,[subPropertyOf(P1,P2),propertyAssertion(P1,X,Y)],[propertyAssertion(P2,X,Y)]).
% Todo entails(prp-spo2... Propertychain
entails(prp-eqp1,[equivalentProperties([P1,P2]),propertyAssertion(P1,X,Y)],[propertyAssertion(P2,X,Y)]).
entails(prp-eqp2,[equivalentProperties([P1,P2]),propertyAssertion(P2,X,Y)],[propertyAssertion(P1,X,Y)]).
entails(prp-pdw, [disjointProperties(P1,P2),propertyAssertion(P1,X,Y),propertyAssertion(P2,X,Y)],[false]).
entails(prp-adp, [disjointProperties(P),pl(select(P1,P,Rest)),pl(member(P2,Rest)),
		  propertyAssertion(P1,X,Y),
		  propertyAssertion(P2,X,Y)],[false]).
entails(prp-inv1,[inverseOf(P1,P2),propertyAssertion(P1,X,Y)],[propertyAssertion(P2,Y,X)]).
entails(prp-inv2,[inverseOf(P1,P2),propertyAssertion(P2,X,Y)],[propertyAssertion(P1,Y,X)]).

/* Todo
entails(prp-key, [hasKey(C,[P]),classAssertion(C,X), % Todo for Key lists with more than 1 properties
		  propertyAssertion(P,X,Z),propertyAssertion(P,Y,Z)],[sameAs(X,Y)]).
*/

% Table 6. The Semantics of Classes
% Todo what about
% cls-thing 	 true 	 T(owl:Thing, rdf:type, owl:Class)
% cls-nothing1   true    T(owl:Nothing, rdf:type, owl:Class)
entails(cls-nothing2,[classAssertion('owl:Nothing',_X)],[false]).
entails(cls-int1, [equivalentClasses([C,intersectionOf(L)]),
		   pl(classAssertionList(L,X))],[classAssertion(C,X)]).


entails(cls-int2, [equivalentClasses([C,intersectionOf(L)]),
		   classAssertion(C,X),pl(member(C1,L))],[classAssertion(C1,X)]).

entails(cls-int2s, [subClassOf(C,intersectionOf(L)),
		   classAssertion(C,X),pl(member(C1,L))],[classAssertion(C1,X)]).

entails(cls-uni,  [equivalentClasses([C,unionOf(L)]),classAssertion(C1,X),
		   pl(member(C1,L))],[classAssertion(C,X)]).

entails(cls-unis,  [subClassOf([unionOf(L),C]),classAssertion(C1,X),
		   pl(member(C1,L))],[classAssertion(C,X)]).
entails(ls-svf1, [subClassOf(C,someValuesFrom(P,Y)),propertyAssertion(P,X,V),
		  classAssertion(Y,V)],[classAssertyion(C,X)]).
%entails(ls-svf2 ToDo...
entails(cls-avf,[classAssertion(allValuesFrom(P,Y),U),propertyAssertion(P,U,V)],
	[classAssertion(Y,V)]).
entails(cls-hv1, [classAssertion(hasValue(P,V),I)],[propertyAssertion(P,I,V)]).
entails(cls-hv2, [classAssertion(hasValue(P,V),I),propertyAssertion(P,X,I)],[classAssertion(hasValue(P,V),X)]).


/* Todo
entails(cls-maxc1, [classAssertion(maxCardinality(0,P),U),propertyAssertion(P,U,_)],[false]).
*/
entails(cls-maxc2, [classAssertion(maxCardinality(1,P),U),
		    propertyAssertion(P,U,Y1),
		    propertyAssertion(P,U,Y2), pl(Y1 \= Y2)],[sameAs(Y1,Y2)]).


% ToDo entails maxqc1-4
entails(cls-oo,  [subClassOf(_,oneOf(L)),pl(member(X,L))],[classAssertion(oneOf(L),X)]).
entails(cls-oo2,  [subClassOf(_,allValuesFrom(_,oneOf(L))),pl(member(X,L))],[classAssertion(oneOf(L),X)]).
entails(cls-oo3,  [subClassOf(_,someValuesFrom(_,oneOf(L))),pl(member(X,L))],[classAssertion(oneOf(L),X)]).


% Table 7. The Semantics of Class Axioms
entails(cax-sco, [subClassOf(C1,C2),classAssertion(C1,I)],[classAssertion(C2,I)]).
entails(cax-eqc1, [equivalentClasses(L),pl(member(C1,L)),pl(member(C2,L)),pl(C1 \= C2),
		   classAssertion(C1,X)],[classAssertion(C2,X)]).
% rule cax-eqc2 is already satisfied from the member permutations
% of the above.

entails(cax-dw,	[disjointWith(L),pl(member(C1,L)),pl(member(C2,L)),pl(C1 \= C2),
		   classAssertion(C1,X),classAssertion(C2,X)],[false]).
entails(cax-adc, [allDisjointClasses(L),pl(member(C1,L)),pl(member(C2,L)),pl(C1 \= C2),
		   classAssertion(C1,X),classAssertion(C2,X)],[false]).


% Table 8. The Semantics of Datatypes - Nothing to be implemented here.

% Table 9. The Semantics of Schema Vocabulary
entails(scm-cls, [class(C)],[subClassOf(C,C)]). % ToDo also Thing and Nothing
entails(scm-sco, [subClassOf(X,Z),subClassOf(Z,Y)],[subClassOf(X,Y)]).
entails(scm-eqc, [equivalentClasses([C1,C2])],[subClassOf(C1,C2),subClassOf(C2,C1)]).
entails(scm-op,  [objectProperty(P)],[subPropertyOf(P,P)]).
entails(scm-dp,  [datatypePropert(P)],[subPropertyOf(P,P)]).
entails(scm-spo, [subPropertyOf(X,Z),subPropertyOf(Z,Y)],[subPropertyOf(X,Y)]).
entails(scm-eqp, [equivalentProperties([P1,P2])],[subPropertyOf(P1,P2),subPropertyOf(P2,P1)]).
entails(scm-dom1,[propertyDomain(P,C1),subClassOf(C1,C2)],[propertyDomain(P,C2)]).
entails(scm-dom2,[propertyDomain(P2,C),subPropertyOf(P1,P2)],[propertyDomain(P1,C)]).
entails(scm-rng1,[propertyRange(P,C1),subClassOf(C1,C2)],[propertyRange(P,C2)]).
entails(scm-rng2,[propertyRange(P2,C),subPropertyOf(P1,P2)],[propertyRange(P1,C)]).
entails(scm-hv,[subClassOf(_,hasValue(P1,I)),subClassOf(_,hasValue(P2,I),_),subPropertyOf(P1,P2)],
	[subClassOf(hasValue(P1,I),hasValue(P2,I))]).

entails(scm-svf1,[subClassOf(_,someValuesFrom(P,Y1)),subClassOf(_,someValuesFrom(P,Y2)), pl( Y1 \= Y2),
		  subClassOf(Y1,Y2)],[subClassOf(someValuesFrom(P,Y1),someValuesFrom(P,Y2))]).


entails(scm-svf2,[subClassOf(_,someValuesFrom(P1,Y)),subClassOf(_,someValuesFrom(P2,Y)), pl( P1 \= P2),
		  subPropertyOf(P1,P2)],[subClassOf(someValuesFrom(P1,Y),someValuesOf(P2,Y))]).

entails(scm-avf1,[subClassOf(_,allValuesFrom(P,Y1)),subClassOf(_,allValuesFrom(P,Y2)), pl(Y1 \= Y2),
		  subClassOf(Y1,Y2)],[subClassOf(allValuesFrom(P,Y1),allValuesFrom(P,Y2))]).


entails(scm-avf2,[subClassOf(_,allValuesFrom(P1,Y)),subClassOf(_,allValuesFrom(P2,Y)), pl(P1 \= P2),
		  subPropertyOf(P1,P2)],[subClassOf(allValuesFrom(P1,Y),allValuesFrom(P2,Y))]).

entails(scm-int, [subClassOf(C,intersectionOf(L)),pl(member(C1,L))],[subClassOf(C,C1)]).
entails(scm-uni, [subClassOf(C,unionOf(L)),pl(member(C1,L))],[subClassOf(C1,C)]).

% ----------------------------------------
% Hooks into owl2_reasoner
% ----------------------------------------

:- multifile owl2_reasoner:reasoner_ask_hook/2.
:- multifile owl2_reasoner:initialize_reasoner_hook/3.

owl2_reasoner:reasoner_ask_hook(rl_rules,G) :-
        is_entailed(G,_).
owl2_reasoner:initialize_reasoner_hook(rl_rules,rl_rules,_) :-
        get_tbox_entailments.

