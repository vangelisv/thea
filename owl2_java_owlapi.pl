/* -*- Mode: Prolog -*- */

:- module(owl2_owlapi,
          [
           create_manager/1,
           create_factory/2,
           create_ontology/3,
           create_reasoner/3,
           add_axiom/4
           ]).

:- use_module(library(jpl)).
:- use_module(owl2_model).


prefix('org.semanticweb.owl.model').

atom_javaURI(X,U):-
        jpl_call('java.net.URI',create,[X],U).

create_manager(Manager) :-
        jpl_call('org.semanticweb.owl.apibinding.OWLManager',createOWLOntologyManager,[],Manager).

create_ontology(Manager,Name,Ont) :-
        atom_javaURI(Name,URI),
        jpl_call(Manager,createOntology,[URI],Ont).

create_factory(Manager,Fac) :-
        jpl_call(Manager,getOWLDataFactory,[],Fac).

create_reasoner(Manager,RN,Reasoner) :-
        reasoner_factory(RN,RFacClass),
        jpl_new(RFacClass,[],RFac),
        jpl_call(RFac,createReasoner,[Manager],Reasoner).

reasoner_factory(factpp,'FaCTPlusPlusReasonerFactory').
reasoner_factory(pellet,'org.semanticweb.reasonerfactory.pellet.PelletReasonerFactory').

add_axiom(Manager,Factory,Axiom,Obj) :-
        owlterm_java(Factory,Axiom,Obj),
        writeln(Obj).

owlterm_java(Fac,OWLTerm,Obj) :-
        OWLTerm =.. [P,X],
        decl_method(P,M),
        !,
        atom_javaURI(X,U),
        jpl_call(Fac,M,[U],Obj).
owlterm_java(Fac,OWLTerm,Obj) :-
        atom(OWLTerm),          % undeclared atom; TODO; numbers eg card
        !,
        atom_javaURI(OWLTerm,U),
        jpl_call(Fac,getOWLClass,[U],Obj). % TODO
owlterm_java(Fac,OWLTerm,Obj) :- % e.g subClassOf
        OWLTerm =.. [P|Args],
        axiom_method(P,M),
        maplist(owlterm_java(Fac),Args,Objs),
        jpl_call(Fac,M,Objs,Obj).
todoowlterm_java(Fac,UntypedAxiom,Obj) :- % e.g. subObjectPropertyOf
        UntypedAxiom =.. [P|Args],
        nonvar(P),
        expr_method(P,M),
        TypeCheckGoal =.. [P,UntypedTerm], % eg 
        TypeCheckGoal,
        maplist(owlterm_java(Fac),Args,Objs),
        jpl_call(Fac,M,Objs,Obj).
owlterm_java(Fac,UntypedTerm,Obj) :-
        expr_method(P,M),
        TypeCheckGoal =.. [P,UntypedTerm], % eg objectIntersectionOf(intersectionOf(L))
        TypeCheckGoal,
        UntypedTerm =.. [_|Args],
        maplist(owlterm_java(Fac),Args,Objs),
        jpl_call(Fac,M,Objs,Obj).

translate_to_java(P,Args,Objs) :-
        axiom_args(P,ArgTypes),
        translate_args_to_java(P,Args,ArgTypes,Objs).

translate_args_to_java(_,[],[],[]).
translate_args_to_java(P,[A|Args],[T|ArgTypes],[Obj|Objs]) :-
        translate_arg_to_java(P,A,T,Obj),
        translate_args_to_java(P,Args,ArgTypes,Objs).

translate_arg_to_java(P,L,set(T),Set) :-
        is_list(L),
        !,
        findall(T,member(_,L),Ts),
        translate_args_to_java(P,Args,Ts,Objs),
        jpl_new('java.util.Set',Objs,Set).

translate_arg_to_java(_,X,T,Obj) :-
        atom(X),
        !,
        atom_javaURI(X,U),
        jpl_new(T,U,Obj).

translate_arg_to_java(_,X,T,Obj) :-
        atom(X),
        !,
        atom_javaURI(X,U),
        jpl_new(T,U,Obj).
translate_arg_to_java(_,X,T,Obj) :-
        atom(X),
        class(X),
        !,
        atom_javaURI(X,U),
        jpl_call(Fac,getOWLClass,U,Obj).
translate_arg_to_java(_,X,T,Obj) :-
        atom(X),
        objectProperty(X),
        !,
        atom_javaURI(X,U),
        jpl_call(Fac,getOWLObjectProperty,U,Obj).
translate_arg_to_java(_,X,T,Obj) :-
        objectIntersectionOf(X),
        !,
        X=intersectionOf(L),
        maplist(L,Args),
        jpl_call(Fac,getOWLObjectIntersectionOf,Args,Obj).
translate_arg_to_java(_,X,T,Obj) :-
        dataIntersectionOf(X),
        !,
        jpl_call(Fac,getOWLDataIntersectionOf,U,Obj).

add_axiom_method(AP,M) :-
        atom_chars(AP,[C|Chars]),
        upcase_atom(AP,AP2),
        atom_chars(AP2,[C2|Chars]),
        sformat(M,'get~wAxiom',[AP2]).



axiom_args(disjointClasses,[set('OWLDescription')]).

decl_method(class,getOWLClass).
decl_method(objectProperty,getOWLObjectProperty).
decl_method(dataType,getOWLDatatype).
decl_method(dataProperty,getOWLDataProperty).
decl_method(owlIndividual,getOWLIndividual). % anonymous individuals?

axiom_method(subClassOf,getOWLSubObjectPropertyAxiom).
axiom_method(disjointClasses,getOWLDisjointClassesAxiom).

% typed
axiom_method(subPropertyOf,subObjectPropertyOf,getOWLSubObjectPropertyAxiom).
axiom_method(subPropertyOf,subDataPropertyOf,getOWLSubDataPropertyAxiom).
axiom_method(disjointProperties,disjointObjectProperties,getOWLDisjointObjectPropertiesAxiom).
axiom_method(disjointProperties,disjointDataProperties,getOWLDisjointDataPropertiesAxiom).

axiom_method(transitiveProperty,getOWLTransitiveObjectPropertyAxiom).
axiom_method(objectPropertyDomain,getOWLObjectPropertyDomainAxiom).

% expressions: untyped descriptions are terms used as arguments
expr_method(objectIntersectionOf,getOWLObjectIntersectionOf).
expr_method(dataIntersectionOf,getOWLDataIntersectionOf).
expr_method(objectComplementOf,getOWLObjectComplementOf).
expr_method(dataComplementOf,getOWLDataComplementOf).
expr_method(objectUnionOf,getOWLObjectUnionOf).
expr_method(dataUnionOf,getOWLDataUnionOf).

expr_method(objectSomeValuesFrom,getOWLObjectSomeRestriction).
expr_method(dataSomeValuesFrom,getOWLDataSomeRestriction).


        

/** <module> bridge to java OWLAPI

---+ Synopsis

  HIGHLY INCOMPLETE!!


---+ Details

  This module is intended to interface with the OWLAPI

  http://owlapi.sourceforge.net/

  This provides access to reasoners such as Pellet and FaCT++

  JPL is required for this module

  Note that this module is not required for the rest of Thea2

@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/
