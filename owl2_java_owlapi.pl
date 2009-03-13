/* -*- Mode: Prolog -*- */

:- module(owl2_java_owlapi,
          [
           create_manager/1,
           create_factory/2,
           create_ontology/3,
           build_ontology/1,
           build_ontology/3,
           save_ontology/3,
           create_reasoner/3,
           reasoner_classify/1,
           reasoner_classify/2,
           reasoner_classify/3,
           reasoner_classify_using/3,
           is_consistent/1,
           inconsistent_class/2,
           reasoner_nr_subClassOf/4,
           reasoner_subClassOf/4,
           reasoner_equivalent_to/4,
           add_axiom/5
           ]).

:- use_module(library(jpl)).
:- use_module(owl2_model).
:- use_module(owl2_metamodel).

:- multifile owlterm_java/4.


prefix('org.semanticweb.owl.model').

atom_javaURI(X,U):-
        sub_atom(X,_,_,_,':'),
        !,
        jpl_call('java.net.URI',create,[X],U).
atom_javaURI(X,U):-
        ontology(Ont),
        !,
        concat_atom([Ont,X],'#',X2),
        jpl_call('java.net.URI',create,[X2],U).
atom_javaURI(X,U):-
        concat_atom(['http://foo.org',X],'#',X2),
        jpl_call('java.net.URI',create,[X2],U).

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

%% build_ontology(?Ont)
% create an ontology from the current prolog db
build_ontology(Ont) :-
        create_factory(Man,Fac),
        build_ontology(Man,Fac,Ont).
        
%% build_ontology(+Man,+Fac,?Ont)
% create an ontology from the current prolog db
build_ontology(Man,Fac,Ont) :-
        require_manager(Man),
        ontology(OntName),
        create_ontology(Man,OntName,Ont),
        forall(axiom(Ax),
               (   debug(owl2,'Adding axiom: ~w',[Ax]),
                   add_axiom(Man,Fac,Ont,Ax,_))).

%% save_ontology(+Man,+Ont,+File) is det
save_ontology(Man,Ont,File) :-
        atom_javaURI(File,URI),
        jpl_call(Man,saveOntology,[Ont,URI],_).


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
        debug(owl2,'got reasoner factory: ~w',[RFac]),
        jpl_call(RFac,createReasoner,[Manager],Reasoner).

reasoner_factory(pellet,'org.mindswap.pellet.owlapi.PelletReasonerFactory').
reasoner_factory(factpp,'org.semanticweb.reasonerfactory.factpp.FaCTPlusPlusReasonerFactory').

reasoner_classify(Reasoner) :-
        debug(owl2,'classifying...',[]),
        jpl_call(Reasoner,classify,[],_).

reasoner_classify(Reasoner,Ont) :-
        reasoner_classify(Reasoner,Man,Ont).

reasoner_classify(Reasoner,Man,Ont) :-
        require_manager(Man),
        jpl_call(Man,getImportsClosure,[Ont],IC),
        jpl_call(Reasoner,loadOntologies,[IC],_),
        reasoner_classify(Reasoner).

reasoner_classify_using(Reasoner,Ont,RN) :-
        require_manager(Man),
        create_reasoner(Man,RN,Reasoner),
        reasoner_classify(Reasoner,Man,Ont).


is_consistent(Reasoner) :-
        jpl_call(Reasoner,isConsistent,[],'@'(true)).

inconsistent_class(Reasoner,Class) :-
        jpl_call(Reasoner,getInconsistentClasses,[],JOWLClasses),
        member(JOWLClass,JOWLClasses),
        java_namedentity(JOWLClass,Class).

java_namedentity(J,C) :-
        jpl_call(J,getURI,[],URI),
        jpl_call(URI,toString,[],C).

ecsets_class(JPSetSet,P) :-
        jpl_call(JPSetSet,toArray,[],JPSetArr),
        jpl_array_to_list(JPSetArr,JPSets),
        member(JPSet,JPSets),
        jpl_call(JPSet,toArray,[],JPArr),
        jpl_array_to_list(JPArr,JPs),
        (   JPs=[JP],
            java_namedentity(JP,P)
        ->  true
        ;   JPs=[]
        ->  fail
        ;   maplist(java_namedentity,JPs,Ps),
            P=equivalentClasses(Ps)).

cxj(Fac,C,JC) :-
        (   atom(C)
        ->  owlterm_java(Fac,_,class(C),JC)
        ;   translate_arg_to_java(Fac,C,_,JC)).


%% reasoner_nr_subClassOf(+R,+Fac,?C,?P)
% ?C ?P - find superclasses for all named classes C
% +C ?P - find superclasses
% ?C +P - find subclasses
%
% an unbound variable may be bound to a named class
% or to a class expression using equivalentClasses/1 -- TODO - this is an axiom not expression        
        
% reasoner_nr_subClassOf(+R,+Fac,?C,?P)
reasoner_nr_subClassOf(R,Fac,C,P) :-
        var(C),
        var(P),
        !,
        class(C),
        reasoner_nr_subClassOf(R,Fac,C,P).

% reasoner_nr_subClassOf(+R,+Fac,+C,?P) 
reasoner_nr_subClassOf(R,Fac,C,P) :-
        nonvar(C),
        !,
        cxj(Fac,C,JC),
        jpl_call(R,getSuperClasses,[JC],JPSetSet),
        ecsets_class(JPSetSet,P).

% reasoner_nr_subClassOf(+R,+Fac,?C,+P) 
reasoner_nr_subClassOf(R,Fac,C,P) :-
        nonvar(P),
        !,
        cxj(Fac,P,PC),
        jpl_call(R,getSubClasses,[JP],JCSetSet),
        ecsets_class(JCSetSet,C).


%% reasoner_subClassOf(+R,+Fac,?C,?P)
% ?C ?P - find superclasses for all named classes C
% +C ?P - find superclasses
% ?C +P - find subclasses

% reasoner_subClassOf(+R,+Fac,?C,?P)
reasoner_subClassOf(R,Fac,C,P) :-
        var(C),
        var(P),
        !,
        class(C),
        reasoner_subClassOf(R,Fac,C,P).

% reasoner_subClassOf(+R,+Fac,+C,?P) 
reasoner_subClassOf(R,Fac,C,P) :-
        nonvar(C),
        !,
        cxj(Fac,C,JC),
        jpl_call(R,getAncestorClasses,[JC],JPSetSet),
        ecsets_class(JPSetSet,P).

% reasoner_subClassOf(+R,+Fac,?C,+P) 
reasoner_subClassOf(R,Fac,C,P) :-
        nonvar(P),
        !,
        cxj(Fac,P,JP),
        jpl_call(R,getDescendantClasses,[JP],JCSetSet),
        ecsets_class(JCSetSet,C).

reasoner_nr_individualOf(R,Fac,I,C) :-
        reasoner_individualOf(R,Fac,I,C,true).
reasoner_individualOf(R,Fac,I,C) :-
        reasoner_individualOf(R,Fac,I,C,false).

reasoner_individualOf(R,Fac,I,C,IsDirect) :-
        nonvar(C),
        nonvar(I),
        !,
        individual(I),
        reasoner_individualOf(R,Fac,I,C,IsDirect).

reasoner_individualOf(R,Fac,I,C,IsDirect) :-
        nonvar(C),
        !,
        cxj(Fac,C,JC),
        (   IsDirect
        ->  Bool='@'(true)
        ;   Bool='@'(false)),
        jpl_call(R,getIndividuals,[JC,Bool],ISet),
        jset_member(ISet,JI),
        java_namedentity(JI,I).

reasoner_individualOf(R,Fac,I,C,IsDirect) :-
        nonvar(I),
        !,
        cxj(Fac,I,JI),
        (   IsDirect
        ->  Bool='@'(true)
        ;   Bool='@'(false)),
        jpl_call(R,getTypes,[IC,Bool],JCSetSet),
        ecsets_class(JCSetSet,C).

% java util
jset_member(JPSet,JP) :-
        jpl_call(JPSet,toArray,[],JPArr),
        jpl_array_to_list(JPArr,JPs),
        member(JP,JPs).

reasoner_equivalent_to(R,Fac,C,C2) :-
        owlterm_java(Fac,_,class(C),JC),
        jpl_call(R,getEquivalentClasses,[JC],JPSet),
        jset_member(JPSet,JP),
        java_namedentity(JP,P).

%% add_axiom(+Manager,+Factory,+Ont,+Axiom,?Obj) is det
add_axiom(Manager,Factory,Ont,Axiom,JAx) :-
        owlterm_java(Factory,_,Axiom,JAx),
        debug(owl2,' axiom ~w = ~w',[Axiom,JAx]),
        (   owl2_model:declarationAxiom(Axiom)
        ->  true
        ;   jpl_new('org.semanticweb.owl.model.AddAxiom',[Ont,JAx],AddAxiom),
            jpl_call(Manager,applyChange,[AddAxiom],_)).

%% owlterm_java(+Factory,?Type,+OWLTerm,?Obj) is det
% translate OWL Axiom or OWL Expression from prolog term to java object

% special rules for annotationAssertions
owlterm_java(Fac,_,annotationAssertion(AP,Sub,literal(Val)),Obj) :-
        !,
        jpl_call(Fac,getOWLUntypedConstant,[Val],JVal),
        translate_arg_to_java(Fac,Sub,entity,JEntity),
        atom_javaURI(AP,JAP),
        jpl_call(Fac,getOWLConstantAnnotation,[JAP,JVal],JAnno),
        jpl_call(Fac,getOWLEntityAnnotationAxiom,[JEntity,JAnno],Obj).

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

% as above, but we must re-order
owlterm_java(Fac,_Type,OWLTerm,Obj) :- % e.g classAssertion
        OWLTerm =.. [P|Args],
        axiom_method(P,M,Objs,ReorderedObjs),
        !,
        axiom_arguments(P,ArgTypes),
        debug(owl2,'using java method: ~w, expecting arguments: ~w',[M,ArgTypes]),
        translate_args_to_java(Fac,Args,ArgTypes,Objs),
        jpl_call(Fac,M,ReorderedObjs,Obj).

% axioms such as subPropertyOf have two typed variants.
% we use type checking which may require the axiom declaration
% to be asserted in the prolog owl2_model database
owlterm_java(Fac,_,UntypedAxiom,Obj) :- % e.g. subObjectPropertyOf
        UntypedAxiom =.. [UntypedPred|Args],      % e.g subPropertyOf p1 p2
        nonvar(UntypedPred),
        owlpredicate_typed(UntypedPred,TypedPred),
        axiom_method(TypedPred,M),   % e.g. subObjectPropertyOf
        TypeCheckGoal =.. [TypedPred|Args],       
        TypeCheckGoal,
        !,
        debug(owl2,'typed axiom: if ~w is ~w',[UntypedAxiom,TypedPred]),
        owlpredicate_arguments(TypedPred,ArgTypes),
        translate_args_to_java(Fac,Args,ArgTypes,Objs),
        debug(owl2,'  translated ~w :: ~w method: ~w',[Args,ArgTypes,M]),
        jpl_call(Fac,M,Objs,Obj).

% on occasion the owlapi has a different ordering of arguments...
owlterm_java(Fac,_,UntypedAxiom,Obj) :- 
        UntypedAxiom =.. [UntypedPred|Args],
        nonvar(UntypedPred),
        owlpredicate_typed(UntypedPred,TypedPred),
        TypeCheckGoal =.. [TypedPred|Args],       
        TypeCheckGoal,
        debug(owl2,'typed axiom: if ~w is ~w',[UntypedAxiom,TypedPred]),
        owlpredicate_arguments(TypedPred,ArgTypes),
        translate_args_to_java(Fac,Args,ArgTypes,Objs),
        axiom_method(TypedPred,M,Objs,ObjsReordered), % e.g. subObjectPropertyOf
        debug(owl2,'  translated ~w :: ~w method: ~w',[Args,ArgTypes,M]),
        jpl_call(Fac,M,ObjsReordered,Obj).


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

translate_arg_to_java(_Fac,X,T,X) :- nonvar(T),T=int,!.

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
translate_arg_to_java(Fac,X,dataPropertyExpression,Obj) :-
        atom(X),
        !,
        owlterm_java(Fac,_,dataProperty(X),Obj).
translate_arg_to_java(Fac,X,objectProperty,Obj) :-
        atom(X),
        !,
        owlterm_java(Fac,_,objectProperty(X),Obj).
translate_arg_to_java(Fac,X,individual,Obj) :-
        atom(X),
        !,
        owlterm_java(Fac,_,individual(X),Obj).
translate_arg_to_java(Fac,X,entity,Obj) :-
        atom(X),
        !,
        owlterm_java(Fac,_,entity(X),Obj).
translate_arg_to_java(Fac,literal(Val),literal,Obj) :- % todo - typed constants
        !,
        jpl_call(Fac,getOWLUntypedConstant,[Val],Obj).

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
        debug(owl2,'  testing for java expr method ~w',[TypedPred]),
        expr_method(TypedPred,Method),
        Check =.. [TypedPred,UntypedExpr], % eg objectIntersectionOf(intersectionOf(L))
        debug(owl2,'  checking expr ~w',[Check]),
        Check,
        !,
        debug(owl2,'  typed expr ~w -> ~w :: ~w',[UntypedExpr,TypedPred,ArgTypes]),
        translate_args_to_java(Fac,Args,ArgTypes,Objs),
        debug(owl2,'  expr ~w -> java ~w.~w',[UntypedExpr,Method,Objs]),
        jpl_call(Fac,Method,Objs,Obj),
        debug(owl2,'  made obj for ~w = ~w',[UntypedExpr,Obj]).

translate_arg_to_java(Fac,UntypedExpr,_T,Obj) :-
        UntypedExpr =.. [UntypedPred|Args], % eg intersectionOf(L)
        owlpredicate_typed(UntypedPred,TypedPred),
        owlpredicate_arguments(TypedPred,ArgTypes),
        Check =.. [TypedPred,UntypedExpr], % eg objectIntersectionOf(intersectionOf(L))
        debug(owl2,'  checking expr ~w',[Check]),
        Check,
        debug(owl2,'  typed expr ~w -> ~w :: ~w',[UntypedExpr,TypedPred,ArgTypes]),
        translate_args_to_java(Fac,Args,ArgTypes,Objs),
        debug(owl2,'  testing for REORDERED java expr method ~w',[TypedPred]),
        expr_method(TypedPred,Method,Objs,ReorderedObjs),
        !,
        debug(owl2,'  expr ( reordered ) ~w -> java ~w.~w',[UntypedExpr,Method,ReorderedObjs]),
        jpl_call(Fac,Method,ReorderedObjs,Obj),
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



%% decl_method(?Predicate,?JavaMethod)
% maps a declaration axiom predicate to the java factory method that will instantiate an instance
decl_method(P,M) :-
        decl_method(P,M,_).

decl_method(class,getOWLClass,classExpression).
decl_method(objectProperty,getOWLObjectProperty,propertyExpression).
decl_method(annotationProperty,getOWLAnnotationProperty,iri).
decl_method(dataType,getOWLDatatype,datatype,_).
decl_method(dataProperty,getOWLDataProperty,_).
decl_method(individual,getOWLIndividual,_). % anonymous individuals?
decl_method(entity,getOWLIndividual,_). % anonymous individuals?

axiom_method(subClassOf,getOWLSubClassAxiom).
axiom_method(equivalentClasses,getOWLEquivalentClassesAxiom).
%axiom_method(subPropertyOf,getOWLSubObjectPropertyAxiom).
axiom_method(disjointClasses,getOWLDisjointClassesAxiom).
axiom_method(inverseProperties,getOWLInverseObjectPropertiesAxiom).

axiom_method(symmetricProperty,getOWLSymmetricObjectPropertyAxiom).
axiom_method(asymmetricProperty,getOWLAsymmetricObjectPropertyAxiom).
axiom_method(reflexiveProperty,getOWLReflexiveObjectPropertyAxiom).
axiom_method(irreflexiveProperty,getOWLIrreflexiveObjectPropertyAxiom).

axiom_method(functionalObjectProperty,getOWLFunctionalObjectPropertyAxiom).
axiom_method(dataObjectProperty,getOWLFunctionalDataPropertyAxiom).



%axiom_method(objectPropertyAssertion,getOWLObjectPropertyAssertionAxiom).
%axiom_method(dataPropertyAssertion,getOWLDataPropertyAssertionAxiom).
axiom_method(objectPropertyAssertion,getOWLObjectPropertyAssertionAxiom,[P,S,T],[S,P,T]).
axiom_method(dataPropertyAssertion,getOWLDataPropertyAssertionAxiom,[P,S,V],[S,P,V]).


%% axiom_method(?Pred,?JavaGetMethod)
axiom_method(subObjectPropertyOf,getOWLSubObjectPropertyAxiom).
axiom_method(subDataPropertyOf,getOWLSubDataPropertyAxiom).
axiom_method(disjointObjectProperties,getOWLDisjointObjectPropertiesAxiom).
axiom_method(disjointDataProperties,getOWLDisjointDataPropertiesAxiom).
axiom_method(equivalentObjectProperties,getOWLEquivalentObjectPropertiesAxiom).
axiom_method(equivalentDataProperties,getOWLEquivalentDataPropertiesAxiom).

axiom_method(transitiveProperty,getOWLTransitiveObjectPropertyAxiom).
axiom_method(inverseFunctionalProperty,getOWLInverseFunctionalObjectPropertyAxiom).
axiom_method(symmetricProperty,getOWLSymmetricObjectPropertyAxiom).
axiom_method(dataPropertyDomain,getOWLDataPropertyDomainAxiom).
axiom_method(objectPropertyDomain,getOWLObjectPropertyDomainAxiom).
axiom_method(dataPropertyRange,getOWLDataPropertyRangeAxiom).
axiom_method(objectPropertyRange,getOWLObjectPropertyRangeAxiom).

axiom_method(classAssertion,getOWLClassAssertionAxiom,[D,I],[I,D]).

expr_method(objectIntersectionOf,getOWLObjectIntersectionOf).
expr_method(dataIntersectionOf,getOWLObjectIntersectionOf).
expr_method(objectSomeValuesFrom,getOWLObjectSomeRestriction).
expr_method(dataSomeValuesFrom,getOWLDataSomeRestriction).
expr_method(objectHasValue,getOWLObjectValueRestriction).
expr_method(dataHasValue,getOWLDataValueRestriction).
expr_method(objectAllValuesFrom,getOWLObjectAllRestriction).
expr_method(dataAllValuesFrom,getOWLDataAllRestriction).
expr_method(objectComplementOf,getOWLObjectComplementOf).
expr_method(dataComplementOf,getOWLDataComplementOf).
expr_method(objectUnionOf,getOWLObjectUnionOf).
expr_method(dataUnionOf,getOWLDataUnionOf).
expr_method(objectOneOf,getOWLObjectOneOf).
expr_method(dataOneOf,getOWLDataOneOf).

expr_method(inverseOf,getOWLObjectPropertyInverse).

% TODO: non-QCR
expr_method(objectMinCardinality,getOWLObjectMinCardinalityRestriction,[N,P,CE],[P,N,CE]).
expr_method(dataMinCardinality,getOWLDataMinCardinalityRestriction,[N,P,CE],[P,N,CE]).
expr_method(objectMaxCardinality,getOWLObjectMaxCardinalityRestriction,[N,P,CE],[P,N,CE]).
expr_method(dataMaxCardinality,getOWLDataMaxCardinalityRestriction,[N,P,CE],[P,N,CE]).
expr_method(objectExactCardinality,getOWLObjectExactCardinalityRestriction,[N,P,CE],[P,N,CE]).
expr_method(dataExactCardinality,getOWLDataExactCardinalityRestriction,[N,P,CE],[P,N,CE]).




        

/** <module> bridge to java OWLAPI

---+ Synopsis

using OWLAPI to save files:
  
==
[owl2_model].
[owl2_java_owlapi].
[owl2_from_rdf].
owl_parse_rdf('testfiles/Hydrology.owl'),
create_factory(Man,Fac),
build_ontology(Man,Fac,Ont),
save_ontology(Man,Ont,'file:///tmp/foo').
==  

using a reasoner:

==
create_reasoner(Man,pellet,Reasoner),
create_factory(Man,Fac),
build_ontology(Man,Fac,Ont),
reasoner_classify(Reasoner,Man,Ont),
save_ontology(Man,Ont,'file:///tmp/foo').
==  

==
[owl2_model].
[owl2_java_owlapi].
[owl2_from_rdf].
owl_parse_rdf('testfiles/music_ontology.owl'),
create_factory(Man,Fac),
build_ontology(Man,Fac,Ont),
writeln(classifying),
create_reasoner(Man,pellet,Reasoner),
reasoner_classify(Reasoner,Man,Ont),
writeln(classified),
class(C),
writeln(c=C),
reasoner_subClassOf(Reasoner,Fac,C,P),
writeln(p=P).
==  

queries:

==
someValuesFrom('http://purl.org/ontology/mo/performed','http://purl.org/ontology/mo/Performance')
==

---+ Use

  JPL Required

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
