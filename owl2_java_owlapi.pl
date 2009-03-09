/* -*- Mode: Prolog -*- */

:- module(owl2_owlapi,
          [
           create_manager/1,
           create_factory/2,
           create_ontology/3,
           create_ontology/1,
           create_reasoner/3,
           add_axiom/4
           ]).

:- use_module(library(jpl)).
:- use_module(owl2_model).
:- use_module(owl2_metamodel).


prefix('org.semanticweb.owl.model').

atom_javaURI(X,U):-
        jpl_call('java.net.URI',create,[X],U).

%% create_manager(?Manager)
create_manager(Manager) :-
        jpl_call('org.semanticweb.owl.apibinding.OWLManager',createOWLOntologyManager,[],Manager).

require_manager(Manager) :-
        var(Manager),
        !,
        create_manager(Manager).
require_manager(_).

%% create_ontology(?Manager,+Name,?Ont)
% @param Manager - manager instance will be created unless this is ground
create_ontology(Manager,Name,Ont) :-
        require_manager(Manager),
        atom_javaURI(Name,URI),
        jpl_call(Manager,createOntology,[URI],Ont).

%% create_ontology(?Ont)
% create an ontology from the current prolog db
create_ontology(Ont) :-
        create_factory(Man,Fac),
        create_ontology(Man,test,Ont),
        forall(axiom(Ax),
               (   debug(owl2,'Adding axiom: ~w',[Ax]),
                   add_axiom(Man,Fac,Ax,_))).


%% create_factory(?Manager,?Factory) is det
% @param Manager - manager instance will be created unless this is ground
create_factory(Manager,Fac) :-
        require_manager(Manager),
        jpl_call(Manager,getOWLDataFactory,[],Fac).

%% create_reasoner(?Manager,?Type,?Reasoner) is nondet
% if Type is ground then this predicate is deterministic
% @param Type - factpp or pellet
create_reasoner(Manager,RN,Reasoner) :-
        require_manager(Manager),
        reasoner_factory(RN,RFacClass),
        jpl_new(RFacClass,[],RFac),
        jpl_call(RFac,createReasoner,[Manager],Reasoner).

reasoner_factory(factpp,'FaCTPlusPlusReasonerFactory').
reasoner_factory(pellet,'org.semanticweb.reasonerfactory.pellet.PelletReasonerFactory').

%% add_axiom(+Manager,+Factory,+Axiom,?Obj) is det
add_axiom(_Manager,Factory,Axiom,Obj) :-
        owlterm_java(Factory,_,Axiom,Obj),
        writeln(Obj).

%% owlterm_java(+Factory,?Type,+OWLTerm,?Obj) is det
% translate OWL Axiom or OWL Expression from prolog term to java object

owlterm_java(Fac,_,OWLTerm,Obj) :-
        OWLTerm =.. [P,X],
        decl_method(P,M),       % declaration axiom
        !,
        atom_javaURI(X,U),
        jpl_call(Fac,M,[U],Obj).

owlterm_java(Fac,_,OWLTerm,Obj) :-
        atom(OWLTerm),          % undeclared atom; TODO; numbers eg card
        !,
        debug(owl2,'converting to URI: ~w',[OWLTerm]),
        atom_javaURI(OWLTerm,U),
        jpl_call(Fac,getOWLClass,[U],Obj). % TODO

% some axioms such as subClassOf are effectively untyped
owlterm_java(Fac,_Type,OWLTerm,Obj) :- % e.g subClassOf
        OWLTerm =.. [P|Args],
        axiom_method(P,M),
        !,
        axiom_arguments(P,ArgTypes),
        debug(owl2,'using java method: ~w, expecting arguments: ~w',[M,ArgTypes]),
        translate_args_to_java(Fac,Args,ArgTypes,Objs),
        jpl_call(Fac,M,Objs,Obj).

% axioms such as subPropertyOf have two typed variants.
% we use type checking which may require the axiom declaration
% to be asserted in the prolog owl2_model database
owlterm_java(Fac,_,UntypedAxiom,Obj) :- % e.g. subObjectPropertyOf
        UntypedAxiom =.. [P|Args],      % e.g subPropertyOf p1 p2
        nonvar(P),
        axiom_method(P,PT,M),              % e.g. subObjectPropertyOf
        TypeCheckGoal =.. [PT|Args],       
        TypeCheckGoal,
        debug(owl2,'typed axiom: if ~w is ~w',[UntypedAxiom,PT]),
        axiom_arguments(PT,ArgTypes),
        translate_args_to_java(Fac,Args,ArgTypes,Objs),
        debug(owl2,'  translated ~w :: ~w',[Args,ArgTypes]),
        jpl_call(Fac,M,Objs,Obj).

xxxowlterm_java(Fac,_,UntypedAxiom,Obj) :- % e.g. subObjectPropertyOf
        UntypedAxiom =.. [P|Args],
        nonvar(P),
        expr_method(P,M),
        TypeCheckGoal =.. [P,_UntypedTerm], % eg 
        TypeCheckGoal,
        maplist(owlterm_java(Fac,_),Args,Objs),
        jpl_call(Fac,M,Objs,Obj).
owlterm_java(Fac,_,UntypedTerm,Obj) :-
        expr_method(P,M),
        TypeCheckGoal =.. [P,UntypedTerm], % eg objectIntersectionOf(intersectionOf(L))
        TypeCheckGoal,
        UntypedTerm =.. [_|Args],
        maplist(owlterm_java(Fac,_),Args,Objs),
        jpl_call(Fac,M,Objs,Obj).

translate_to_java(Fac,P,Args,Objs) :-
        axiom_args(P,ArgTypes),
        translate_args_to_java(Fac,P,Args,ArgTypes,Objs).

translate_args_to_java(_Fac,[],[],[]).
translate_args_to_java(Fac,[A|Args],[T|ArgTypes],[Obj|Objs]) :-
        debug(owl2,' translating: ~w  :: ~w',[A,T]),
        translate_arg_to_java(Fac,A,T,Obj),
        debug(owl2,' translated: ~w --> ~w',[A,Obj]),
        translate_args_to_java(Fac,Args,ArgTypes,Objs).

translate_arg_to_java(Fac,L,list(T),Set) :-
        is_list(L),
        !,
        findall(T,member(_,L),Ts),
        translate_args_to_java(Fac,L,Ts,Objs),
        jpl_new('java.util.HashSet',[],Set),
        debug(owl2,' new set: ~w = ~w -- adding objs ~w',[L,Set,Objs]),
        forall(member(Obj,Objs),
               jpl_call(Set,add,[Obj],_)),
        debug(owl2,' made set: ~w ',[Set]).


translate_arg_to_java(Fac,X,classExpression,Obj) :-
        atom(X),
        !,
        owlterm_java(Fac,_,class(X),Obj).
translate_arg_to_java(Fac,X,objectPropertyExpressionOrChain,Obj) :-
        atom(X),
        !,
        owlterm_java(Fac,_,objectProperty(X),Obj).
translate_arg_to_java(Fac,X,objectPropertyExpression,Obj) :-
        atom(X),
        !,
        owlterm_java(Fac,_,objectProperty(X),Obj).
translate_arg_to_java(Fac,X,individual,Obj) :-
        atom(X),
        !,
        owlterm_java(Fac,_,individual(X),Obj).

translate_arg_to_java(_Fac,X,T,Obj) :- % TODO
        atom(X),
        !,
        atom_javaURI(X,U),
        jpl_new(T,U,Obj).

translate_arg_to_java(Fac,X,_T,Obj) :-
        atom(X),
        class(X),
        !,
        atom_javaURI(X,U),
        jpl_call(Fac,getOWLClass,U,Obj).
translate_arg_to_java(Fac,X,_T,Obj) :-
        atom(X),
        objectProperty(X),
        !,
        atom_javaURI(X,U),
        jpl_call(Fac,getOWLObjectProperty,U,Obj).

% typed expressions
translate_arg_to_java(Fac,UntypedExpr,_T,Obj) :-
        UntypedExpr =.. [UntypedPred|Args], % eg intersectionOf(L)
        owlpredicate_typed(UntypedPred,TypedPred),
        owlpredicate_arguments(TypedPred,ArgTypes),
        expr_method(TypedPred,Method),
        Check =.. [TypedPred,UntypedExpr], % eg objectIntersectionOf(intersectionOf(L))
        Check,
        !,
        debug(owl2,'  typed expr ~w -> ~w :: ~w',[UntypedExpr,TypedPred,ArgTypes]),
        translate_args_to_java(Fac,Args,ArgTypes,Objs),
        debug(owl2,'  expr ~w -> java ~w.~w',[UntypedExpr,Method,Objs]),
        jpl_call(Fac,Method,Objs,Obj),
        debug(owl2,'  made obj for ~w = ~w',[UntypedExpr,Obj]).

translate_arg_to_java(Fac,Expr,_T,Obj) :-
        Expr =.. [Pred|Args],   % eg oneOf(L)
        owlpredicate_arguments(Pred,ArgTypes),
        expr_method(Pred,Method),
        !,
        translate_args_to_java(Fac,Args,ArgTypes,Objs),
        jpl_call(Fac,Method,Objs,Obj).

%add_axiom_method(AP,M) :-
%        atom_chars(AP,[C|Chars]),
%        upcase_atom(AP,AP2),
%        atom_chars(AP2,[C2|Chars]),
%        sformat(M,'get~wAxiom',[AP2]).



axiom_args(disjointClasses,[set('OWLDescription')]).

%% decl_method(?Predicate,?JavaMethod)
% maps a declaration axiom predicate to the java factory method that will instantiate an instance
decl_method(P,M) :-
        decl_method(P,M,_).

decl_method(class,getOWLClass,classExpression).
decl_method(objectProperty,getOWLObjectProperty,propertyExpression).
decl_method(dataType,getOWLDatatype,datatype,_).
decl_method(dataProperty,getOWLDataProperty,_).
decl_method(individual,getOWLIndividual,_). % anonymous individuals?

axiom_method(subClassOf,getOWLSubClassAxiom).
axiom_method(equivalentClasses,getOWLEquivalentClassesAxiom).
%axiom_method(subPropertyOf,getOWLSubObjectPropertyAxiom).
axiom_method(disjointClasses,getOWLDisjointClassesAxiom).

%% axiom_method(?UntypedAxiom,?TypedAxiom,?JavaGetMethod)
% typed
% TODO: move to metamodel
axiom_method(subPropertyOf,subObjectPropertyOf,getOWLSubObjectPropertyAxiom).
axiom_method(subPropertyOf,subDataPropertyOf,getOWLSubDataPropertyAxiom).
axiom_method(disjointProperties,disjointObjectProperties,getOWLDisjointObjectPropertiesAxiom).
axiom_method(disjointProperties,disjointDataProperties,getOWLDisjointDataPropertiesAxiom).

axiom_method(transitiveProperty,getOWLTransitiveObjectPropertyAxiom).
axiom_method(objectPropertyDomain,getOWLObjectPropertyDomainAxiom).


expr_method(objectIntersectionOf,getOWLObjectIntersectionOf).
expr_method(dataIntersectionOf,getOWLObjectIntersectionOf).
expr_method(objectSomeValuesFrom,getOWLObjectSomeRestriction).
expr_method(dataSomeValuesFrom,getOWLDataSomeRestriction).
expr_method(objectAllValuesFrom,getOWLObjectAllRestriction).
expr_method(dataAllValuesFrom,getOWLDataAllRestriction).
expr_method(objectComplementOf,getOWLObjectComplementOf).
expr_method(dataComplementOf,getOWLDataComplementOf).
expr_method(objectUnionOf,getOWLObjectUnionOf).
expr_method(dataUnionOf,getOWLDataUnionOf).
expr_method(objectOneOf,getOWLObjectOneOf).
expr_method(dataOneOf,getOWLDataOneOf).




        

/** <module> bridge to java OWLAPI

---+ Synopsis

  HIGHLY INCOMPLETE!!

  Set your CLASSPATH to include owlapi-bin.jar, pellet.jar, ...

  start SWI

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
