s/* -*- Mode: Prolog -*- */

:- module(owl2_java_owlapi,
          [
           version_info/1,
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
           unsatisfiable_class/2,
           inferred_axiom/3,
           reasoner_nr_subClassOf/4,
           reasoner_subClassOf/4,
           reasoner_subClassOf/5,
           reasoner_equivalent_to/4,
           reasoner_individualOf/4,
           reasoner_nr_individualOf/4,
           reasoner_objectPropertyAssertion/5,
           add_axiom/5,
           show_java_memory_info/0
           ]).

:- use_module(library(jpl)).
:- use_module(owl2_model).
:- use_module(owl2_metamodel).

:- multifile owlterm_java/4.

prefix('org.semanticweb.owlapi.model').

nothing('http://www.w3.org/2002/07/owl#Nothing').


version_info(Info) :-
        jpl_call('org.semanticweb.owlapi.util.VersionInfo',getVersionInfo,[],VI),
        jpl_call(VI,getVersion,[],Info).

atom_javaIRI('owl:Thing',U):-
        !,
        atom_javaIRI('http://www.w3.org/2002/07/owl#Thing',U).
atom_javaIRI('owl:Nothing',U):-
        !,
        atom_javaIRI('http://www.w3.org/2002/07/owl#Nothing',U).
atom_javaIRI(X,U):-
        sub_atom(X,_,_,_,':'),
        !,
        jpl_call('org.semanticweb.owlapi.model.IRI',create,[X],U).
atom_javaIRI(X,U):-
        ontology(Ont),
        !,
        concat_atom([Ont,X],'#',X2),
        jpl_call('org.semanticweb.owlapi.model.IRI',create,[X2],U).
atom_javaIRI(X,U):-
        concat_atom(['http://foo.org',X],'#',X2),
        jpl_call('org.semanticweb.owlapi.model.IRI',create,[X2],U).

%% create_manager(?Manager)
create_manager(Manager) :-
        jpl_call('org.semanticweb.owlapi.apibinding.OWLManager',createOWLOntologyManager,[],Manager).

require_manager(Manager) :-
        var(Manager),
        !,
        create_manager(Manager).
require_manager(_).

%% create_ontology(?Manager,+Name,?Ont)
% @param Manager - manager instance will be created unless this is ground
create_ontology(Manager,Name,Ont) :-
        require_manager(Manager),
        atom_javaIRI(Name,IRI),
        jpl_call(Manager,createOntology,[IRI],Ont).

% example: filter(Axiom,axiom_profile(Axiom,owl2_EL))
get_axiom_filter(Opts,A,G) :-
        memberchk(filter(A,G),Opts),
        !.
get_axiom_filter(_,_,true).

additional_axiom(Opts,A) :-
        memberchk(adder(A,G),Opts),
        G.

        
%% build_ontology(?Ont)
% create an ontology from the current prolog db
build_ontology(Ont) :-
        create_factory(Man,Fac),
        build_ontology(Man,Fac,Ont).

%% build_ontology(+Man,+Fac,?Ont)
%% build_ontology(+Man,+Fac,?Ont,Opts:list)
% create an ontology from the current prolog db
build_ontology(Man,Fac,Ont) :-
        build_ontology(Man,Fac,Ont,[]).
build_ontology(Man,Fac,Ont,Opts) :-
        setof(OntIRI,member(ontology(OntIRI),Opts),OntIRIs),
        !,
        get_axiom_filter(Opts,Ax,FilterGoal),
        require_manager(Man),
        (   ontology(OntName)
        ->  true
        ;   OntName='http://example.org'),
        create_ontology(Man,OntName,Ont),
        forall((member(OntIRI,OntIRIs),
                (   ontologyAxiom(OntIRI,Ax)
                ;   additional_axiom(Opts,Ax)),
                valid_axiom(Ax),
                FilterGoal),
               add_axiom(Man,Fac,Ont,Ax,_)),
        debug(owl2,'Built ontology',[]).
build_ontology(Man,Fac,Ont,Opts) :-
        get_axiom_filter(Opts,Ax,FilterGoal),
        require_manager(Man),
        (   ontology(OntName)
        ->  true
        ;   OntName='http://example.org'),
        create_ontology(Man,OntName,Ont),
        forall(((    axiom(Ax)
                 ;   additional_axiom(Opts,Ax)),
                debug(owl2,'Testing axiom against filter: ~w',[Ax]),
                valid_axiom(Ax),
                FilterGoal),
               add_axiom(Man,Fac,Ont,Ax,_)),
        debug(owl2,'Built ontology',[]).

/*
build_ontology(Man,Fac,Ont,_Opts) :-
        require_manager(Man),
        (   ontology(OntName)
        ->  true
        ;   OntName='http://example.org'),
        create_ontology(Man,OntName,Ont),
        forall(axiom(Ax),
               add_axiom(Man,Fac,Ont,Ax,_)),
        debug(owl2,'Built ontology',[]).
*/

build_single_ontology(Man,Fac,OntIRI,Ont) :-
        require_manager(Man),
        create_ontology(Man,OntIRI,Ont),
        forall(ontologyAxiom(OntIRI,Ax),
               add_axiom(Man,Fac,Ont,Ax,_)),
        debug(owl2,'Built ontology',[]).


:- multifile owl2_io:load_axioms_hook/3.
owl2_io:load_axioms_hook(File,owlapi,Opts) :-
	owl2_io:load_axioms_hook(File,owlapi(_),Opts).
owl2_io:load_axioms_hook(File,owlapi(_Fmt),_Opts) :-
        create_factory(Man,_Fac),
        load_ontology(Man,_Ont,File).

:- multifile owl2_io:save_axioms_hook/3.
owl2_io:save_axioms_hook(File,owlapi,Opts) :-
        !,
	owl2_io:save_axioms_hook(File,owlapi(''),Opts).
owl2_io:save_axioms_hook(File,owlapi(Fmt),Opts) :-
        member(ontology(OntIRI),Opts),
        !,
        create_factory(Man,Fac),
        build_single_ontology(Man,Fac,OntIRI,Ont),
        save_ontology(Man,Ont,Fmt,File).
owl2_io:save_axioms_hook(File,owlapi(Fmt),_Opts) :-
        create_factory(Man,Fac),
        build_ontology(Man,Fac,Ont),
        save_ontology(Man,Ont,Fmt,File).

%% load_ontology(+Man,?Ont,+File) is det
% TODO - need to maps java axioms to owlpl axioms
load_ontology(Man,Ont,File) :-
        atom_javaIRI(File,IRI),
        jpl_call(Man,loadOntologyFromPhysicalIRI,[IRI],Ont).


%% save_ontology(+Man,+Ont,+File) is det
save_ontology(Man,Ont,File) :-
        save_ontology(Man,Ont,'',File).
save_ontology(Man,Ont,Fmt,File) :-
        (   var(File)
        ->  tmp_file(owl,File),
            Tmp=true
        ;   Tmp=fail),
        atom_concat('file://',File,FileIRI),
        atom_javaIRI(FileIRI,IRI),
        fmt_cls(Fmt,FmtCls),
        jpl_new(FmtCls,[],FmtObj),
        jpl_call(Man,saveOntology,[Ont,FmtObj,IRI],_),
        (   Tmp
        ->  sformat(Cmd,'cat ~w',[File]),
            shell(Cmd)
        ;   true).

fmt_cls(owlxml,'org.semanticweb.owlapi.io.OWLXMLOntologyFormat') :- !.
fmt_cls(manchester,'org.coode.owlapi.manchesterowlsyntax.ManchesterOWLSyntaxOntologyFormat') :- !.
fmt_cls(_,'org.semanticweb.owlapi.io.RDFXMLOntologyFormat') :- !.




%% create_factory(?Manager,?Factory) is det
% @param Manager - manager instance will be created unless this is ground
create_factory(Manager,Fac) :-
        require_manager(Manager),
        jpl_call(Manager,getOWLDataFactory,[],Fac).

%% create_reasoner(?Ont,?Type,?Reasoner) is nondet
% if Type is ground then this predicate is deterministic
% @param Type - factpp or pellet
create_reasoner(Ont,RN,Reasoner) :-
        reasoner_factory(RN,RFacClass),
        !,
        jpl_new(RFacClass,[],RFac),
        debug(owl2,'got reasoner factory: ~w',[RFac]),
        jpl_call(RFac,createReasoner,[Ont],Reasoner).
create_reasoner(Ont,jcel,Reasoner) :-
        !,
        jpl_new('de.tudresden.inf.lat.jcel.owlapi.main.JcelReasoner',[Ont],Reasoner),
        jpl_get('org.semanticweb.owlapi.reasoner.InferenceType','CLASS_HIERARCHY',InfType),
        jpl_datums_to_array([InfType],InfTypeArr),
        jpl_call(Reasoner,precomputeInferences,[InfTypeArr],_).


wrapped_reasoner(jcel).
wrapped_reasoner(R) :- reasoner_factory(R,_).

reasoner_factory(pellet,'com.clarkparsia.pellet.owlapiv3.PelletReasonerFactory').
reasoner_factory(hermit,'org.semanticweb.HermiT.Reasoner$ReasonerFactory').
reasoner_factory(factpp,'uk.ac.manchester.cs.factplusplus.owlapiv3.FaCTPlusPlusReasonerFactory').
reasoner_factory(cb,'org.semanticweb.cb.owlapi.CBReasonerFactory').
reasoner_factory(jagr,'owltools.reasoner.GraphReasonerFactory').

% DEPRECATED
reasoner_classify(Reasoner) :-
        throw(deprecated('no need to call classify with owlapi v3')),
        debug(owl2,'classifying...',[]),
        jpl_call(Reasoner,classify,[],_).

% DEPRECATED
reasoner_classify(Reasoner,Ont) :-
        throw(deprecated('no need to call classify with owlapi v3')),
        reasoner_classify(Reasoner,_Man,Ont).

% DEPRECATED
reasoner_classify(Reasoner,Man,_Ont) :-
        throw(deprecated('no need to call classify with owlapi v3')),
        require_manager(Man),
        %jpl_call(Man,getImportsClosure,[Ont],IC),
        %jpl_call(Reasoner,loadOntologies,[IC],_),
        reasoner_classify(Reasoner).

% DEPRECATED
reasoner_classify_using(Reasoner,Ont,RN) :-
        throw(deprecated('no need to call classify with owlapi v3')),
        require_manager(Man),
        create_reasoner(Ont,RN,Reasoner),
        reasoner_classify(Reasoner,Man,Ont).


is_consistent(Reasoner) :-
        jpl_call(Reasoner,isConsistent,[],'@'(true)).

inconsistent_class(Reasoner,Class) :-
        jpl_call(Reasoner,getInconsistentClasses,[],JOWLClasses),
        member(JOWLClass,JOWLClasses),
        java_namedentity(JOWLClass,Class).

unsatisfiable_class(Reasoner,Class) :-
        jpl_call(Reasoner,getUnsatisfiableClasses,[],Node),
        jpl_call(Node,getEntities,[],ESet),
        jpl_call(ESet,toArray,[],EArr),
        jpl_array_to_list(EArr,PList),
        member(JOWLClass,PList),
        java_namedentity(JOWLClass,Class).

java_namedentity(J,C) :-
        jpl_call(J,getIRI,[],IRI),
        jpl_call(IRI,toString,[],C).

jpl_implements_method(JOb,MN) :-
        jpl_call(JOb,getClass,[],Z),jpl_call(Z,getMethods,[],ML),jpl_array_to_list(ML,Ms),member(M,Ms),jpl_call(M,getName,[],MN).

jpl_instantiates_interface(JOb,IN) :-
        jpl_call(JOb,getClass,[],JC),
        jpl_extends_interface(JC,IN).

jpl_extends_interface(JC,IN) :-
        jpl_call(JC,getInterfaces,[],ML),jpl_array_to_list(ML,Ms),member(M,Ms),jpl_call(M,getName,[],IN).
jpl_extends_interface(JC,IN) :-
        jpl_call(JC,getSuperclass,[],SC),
        SC \= '@'(null),
        jpl_extends_interface(SC,IN).

% note: currently incomplete
class_expression_to_prolog(J,C) :-
        jpl_implements_method(J,getIRI),
        !,
        java_namedentity(J,C).
class_expression_to_prolog(J,intersectionOf(PL)) :-
        jpl_instantiates_interface(J,'org.semanticweb.owlapi.model.OWLObjectIntersectionOf'),
        !,
        jpl_call(J,getOperands,[],OJL),
        jpl_array_to_list(OJL,OL),
        maplist(class_expression_to_prolog,OL,PL).
class_expression_to_prolog(J,unionOf(PL)) :-
        jpl_instantiates_interface(J,'org.semanticweb.owlapi.model.OWLObjectUnionOf'),
        !,
        jpl_call(J,getOperands,[],OJL),
        jpl_array_to_list(OJL,OL),
        maplist(class_expression_to_prolog,OL,PL).
class_expression_to_prolog(J,someValuesFrom(PPr,PF)) :-
        jpl_instantiates_interface(J,'org.semanticweb.owlapi.model.OWLObjectSomeValuesFrom'),
        !,
        jpl_call(J,getFiller,[],JF),
        class_expression_to_prolog(JF,PF),
        jpl_call(J,getProperty,[],JPr),
        class_expression_to_prolog(JPr,PPr).




%% nodeset_entity(+NodeSet,?E) is nondet
nodeset_entity(NodeSet,E) :-
        jpl_call(NodeSet,'getNodes',[],NodeSetG),
        jpl_call(NodeSetG,'toArray',[],NodeArr),
        jpl_array_to_list(NodeArr,Nodes),
        member(Node,Nodes),
        node_entity(Node,E).
/*
        jpl_call(Node,getEntities,[],ESet),
        jpl_call(ESet,toArray,[],EArr),
        jpl_array_to_list(EArr,Es),
        (   Es=[JE],
            java_namedentity(JE,E)
        ->  true
        ;   Es=[]
        ->  fail
        ;   maplist(java_namedentity,Es,PEs),
            E=equivalentClasses(PEs)).
*/

node_entity(Node,E) :-
        jpl_call(Node,getEntities,[],ESet),
        jpl_call(ESet,toArray,[],EArr),
        jpl_array_to_list(EArr,JEs),
        member(JE,JEs),
        java_namedentity(JE,E).

%% ecsets_class(+JPSetSet,?P) is nondet
% Set<Set<OWLClass>> --> class expression
%  class expression will be
%   equivalentClasses(ECL) |
%   other class expression
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

%% pimap_property_individual(PIMap,P,I) is nondet
%  Map<OWLObjectProperty, Set<OWLNamedIndividual>> --> ?Property ?Individual
% (currently object properties only?)
pimap_property_individual(PIMap,P,I) :-
        jpl_call(PIMap,keySet,[],JPSet),
        jpl_call(JPSet,toArray,[],JPArr),
        jpl_array_to_list(JPArr,JPs),
        member(JP,JPs),
        jpl_call(PIMap,get,[JP],JISet),
        jpl_call(JISet,toArray,[],JIArr),
        jpl_array_to_list(JIArr,JIs),
        member(JI,JIs),
        java_namedentity(JP,P),
        java_namedentity(JI,I).

% converts prolog reference to java
pl2javaref(Fac,C,JC) :-
        (   atom(C)
        ->  owlterm_java(Fac,_,C,JC)
        ;   translate_arg_to_java(Fac,C,_,JC)).


%% inferred_axiom(+R,+Fac,?Axiom)
inferred_axiom(R,Fac,subClassOf(A,B)) :-
        reasoner_subClassOf(R,Fac,A,B).
inferred_axiom(R,Fac,classAssertion(C,I)) :-
        reasoner_individualOf(R,Fac,I,C).
inferred_axiom(R,Fac,classAssertion(C,I)) :-
        reasoner_nr_individualOf(R,Fac,I,C).
inferred_axiom(R,Fac,propertyAssertion(P,I,I2)) :-
        reasoner_objectPropertyAssertion(R,Fac,I,P,I2).

        
%% reasoner_nr_subClassOf(+R,+Fac,?C,?P)
% ?C ?P - find superclasses for all named classes C
% +C ?P - find superclasses
% ?C +P - find subclasses
%
% an unbound variable may be bound to a named class
% or to a class expression using equivalentClasses/1 -- TODO - this is an axiom not expression        
%
% DEPRECATED - use reasoner_nr_subClassOf/5 with final argument 'true'

% reasoner_nr_subClassOf(+R,+Fac,?C,?P)
reasoner_nr_subClassOf(R,Fac,C,P) :-
        throw(not_implemented),
        var(C),
        var(P),
        !,
        class(C),
        reasoner_nr_subClassOf(R,Fac,C,P).

% reasoner_nr_subClassOf(+R,+Fac,+C,?P) 
reasoner_nr_subClassOf(R,Fac,C,P) :-
        throw(not_implemented),
        nonvar(C),
        !,
        pl2javaref(Fac,C,JC),
        jpl_call(R,getSuperClasses,[JC],JPSetSet),
        ecsets_class(JPSetSet,P).

% reasoner_nr_subClassOf(+R,+Fac,?C,+P) 
reasoner_nr_subClassOf(R,Fac,C,P) :-
        throw(not_implemented),
        nonvar(P),
        !,
        pl2javaref(Fac,P,JP),
        jpl_call(R,getSubClasses,[JP,@(true)],JCSetSet),
        nodeset_entity(JCSetSet,C).

% experimental
%  gets inferred superclasses of P, where the superclass can be a named class or anon class expression
reasoner_superClassExpressionOf(R,Fac,P,C) :-
        class(C),
        pl2javaref(Fac,C,JC),
        jpl_call(R,getSuperClassExpressions,[JC,@(false)],JPSet),
        jpl_call(JPSet,toArray,[],JPArr),
        jpl_array_to_list(JPArr,JPL),
        member(JP,JPL),
        class_expression_to_prolog(JP,P).

%% reasoner_subClassOf(+R,+Fac,?C,?P)
% ?C ?P - find superclasses for all named classes C
% +C ?P - find superclasses
% ?C +P - find subclasses
reasoner_subClassOf(R,Fac,C,P) :-
        reasoner_subClassOf(R,Fac,C,P,false).

%% reasoner_subClassOf(+R,+Fac,?C,?P,+IsDirect)
% ?C ?P - find superclasses for all named classes C
% +C ?P - find superclasses
% ?C +P - find subclasses
% IsDirect - true or false

% reasoner_subClassOf(+R,+Fac,?C,?P,+IsDirect)
reasoner_subClassOf(R,Fac,C,P,IsDirect) :-
        var(C),
        var(P),
        !,
        class(C), % named classes only
        reasoner_subClassOf(R,Fac,C,P,IsDirect).

% reasoner_subClassOf(+R,+Fac,+C,?P,+IsDirect) 
reasoner_subClassOf(R,Fac,C,P,IsDirect) :-
        nonvar(C),
        !,
        pl2javaref(Fac,C,JC),
        debug(reasoner,'~w ==> getSuperClasses(~w,@(~w))',[C,JC,IsDirect]),
        jpl_call(R,getSuperClasses,[JC,@(IsDirect)],JPSetSet),
        nodeset_entity(JPSetSet,P).

% reasoner_subClassOf(+R,+Fac,?C,+P,+IsDirect) 
reasoner_subClassOf(R,Fac,C,P,IsDirect) :-
        nonvar(P),
        !,
        % allow queries of form: subClassOf(X,part_of some Y)
        %  - generate all possible values
        generate_ground_class_expression(P), 
        debug(reasoner,'getSubClasses( ~w )',[P]),
        pl2javaref(Fac,P,JP),
        jpl_call(R,getSubClasses,[JP,@(IsDirect)],JCSetSet),
        debug(reasoner,'getSubClasses( ~w ) = ~w',[P,JCSetSet]),
        nodeset_entity(JCSetSet,C).

generate_ground_class_expression(X) :-
        ground(X),
        !.
generate_ground_class_expression(X) :-
        classExpression(X).



%% reasoner_nr_individualOf(+R,+Fac,?I,?C)
% ?I ?C - find classes for all named individuals I
% +I ?C - find classes
% ?I +C - find individuals

reasoner_nr_individualOf(R,Fac,I,C) :-
        reasoner_individualOf(R,Fac,I,C,true).

%% reasoner_individualOf(+R,+Fac,?I,?C)
reasoner_individualOf(R,Fac,I,C) :-
        reasoner_individualOf(R,Fac,I,C,false).

reasoner_individualOf(R,Fac,I,C,IsDirect) :-
        var(C),
        var(I),
        !,
        is_class(C),
        reasoner_individualOf(R,Fac,I,C,IsDirect).


reasoner_individualOf(R,Fac,I,C,IsDirect) :-
        nonvar(C),
        !,
        pl2javaref(Fac,C,JC),
        (   IsDirect
        ->  Bool='@'(true)
        ;   Bool='@'(false)),
        jpl_call(R,getInstances,[JC,Bool],ISet),
        nodeset_entity(ISet,I).

reasoner_individualOf(R,Fac,I,C,IsDirect) :-
        nonvar(I),
        !,
        pl2javaref(Fac,I,JI),
        (   IsDirect
        ->  Bool='@'(true)
        ;   Bool='@'(false)),
        jpl_call(R,getTypes,[JI,Bool],JCSetSet),
        ecsets_class(JCSetSet,C).

reasoner_objectPropertyAssertion(R,Fac,P,I,I2) :-
        (   var(I)
        ->  classAssertion(_,I), % TODO - better way to get enumerate individuals?
            \+ objectProperty(I), % see issue 16 - hack for now
            \+ is_class(I)
        ;   true),
        (   var(P)
        ->  objectProperty(P)
        ;   true),
        debug(reasoner,'I=~w',[I]),
        pl2javaref(Fac,I,JI),
        pl2javaref(Fac,P,JP),
        jpl_call(R,getObjectPropertyValues,[JI,JP],NodeSet),
        nodeset_entity(NodeSet,I2).



% java util
jset_member(JPSet,JP) :-
        jpl_call(JPSet,toArray,[],JPArr),
        jpl_array_to_list(JPArr,JPs),
        member(JP,JPs).

%% reasoner_equivalent_to(+R,+Fac,+C,?P)
reasoner_equivalent_to(R,Fac,C,P) :-
        (   var(C)
        ->  is_class(C)
        ;   true),
        owlterm_java(Fac,_,class(C),JC),
        jpl_call(R,getEquivalentClasses,[JC],JP),
        node_entity(JP,P).

%% add_axiom(+Manager,+Factory,+Ont,+Axiom,?Obj) is det
% adds an axiom to Ont from the prolog databases
add_axiom(Manager,Factory,Ont,Axiom,JAx) :-
        debug(owl2,' converting axiom: ~w ',[Axiom]),
        owlterm_java(Factory,_,Axiom,JAx),
        debug(owl2,' axiom ~w = ~w',[Axiom,JAx]),
        (   owl2_model:declarationAxiom(Axiom)
        ->  true
        ;   JAx=ignore
        ->  true
        ;   jpl_new('org.semanticweb.owlapi.model.AddAxiom',[Ont,JAx],AddAxiom),
            jpl_call(Manager,applyChange,[AddAxiom],_)),
        debug(owl2,' added axiom ~w = ~w',[Axiom,JAx]),
        !.
add_axiom(_,_,_,Axiom,_) :-
        throw(error(add_axiom(Axiom))).


%% owlterm_java(+Factory,?Type,+OWLTerm,?Obj) is det
% translate OWL Axiom or OWL Expression from prolog term to java object

% --------
% SPECIAL CASES
% --------

% special rules for ontology declarations - should have been handled previously
owlterm_java(_,_,ontology(_),_) :- !.

% special rules for ontologyAnnotations
% TODO: this can go when we have sorted out ontologyAnnotation/2. See issue#17.
% also - not currently clear to me how to make an axiom for ontologyAnnotation/2 in the owlapi
owlterm_java(_,_,propertyAssertion(_,Sub,_),ignore) :-
        ontology(Sub),
        !.
owlterm_java(_,_,propertyAssertion(P,S,V),ignore) :-
        \+ property(P),
        print_message(warning,no_translation(propertyAssertion(P,S,V),'property is of unknown type')),
        !.
owlterm_java(_,_,annotationAssertion(_,Sub,_),ignore) :-
        ontology(Sub),
        !.
% todo:
owlterm_java(_,_,annotation(_),ignore) :-
        !.
owlterm_java(_,_,annotation(_,_,_),ignore) :-
        !.

% disable this for now whilst we figure out owlapi3
% (not required for reasoning anyway)
owlterm_java(_,_,annotationAssertion(_,_,_),ignore) :-
        !.

% special rules for annotationAssertions
owlterm_java(Fac,_,annotationAssertion(AP,Sub,Val),Obj) :-
        !,
        %(   atom(Val)
        %->  trace
        %;   true),
        %(   Sub='http://www.obofoundry.org/ro/ro.owl#part_of'
        %->  trace
        %;   true),
        %owlterm_java(Fac,_,Sub,JEntity), % e.g. "fred"
        (   translate_arg_to_java(Fac,Val,literal,JVal)
        ->  true,
            debug(owl2,'~w is liter => ~w',[Val,JVal])
        ;   atom_javaIRI(Val,JVal),
            debug(owl2,'treating ~w as IRI ~w',[Val,JVal])),
        %translate_arg_to_java(Fac,Val,literal,JVal), % e.g. "fred"
        %translate_arg_to_java(Fac,Sub,entity,JEntity), % e.g. db:fred
        %atom_javaIRI(Sub,JEntity), % e.g. db:fred
        % force argument to be an annotation property
        atom_javaIRI(Sub,JEntity), % e.g. label
        atom_javaIRI(AP,AP_IRI), % e.g. label
        jpl_call(Fac,getOWLAnnotationProperty,[AP_IRI],JAP),
        debug(owl2,'ap(~w) => ~w',[AP,JAP]),
        %owlterm_java(Fac,_,AP,JAP),
        debug(owl2,'annot assertion(~w ~w ~w)',[JAP,JEntity,JVal]),
        jpl_call(Fac,getOWLAnnotationAssertionAxiom,[JAP,JEntity,JVal],Obj).

% --------
% DECLARATIONS
% --------
owlterm_java(Fac,_,OWLTerm,Obj) :-
        OWLTerm =.. [P,X],
        decl_method(P,M),       % declaration axiom
        !,
        debug(owl2,'decl(~w,~w) -- converting to IRI: ~w',[P,M,X]),
        atom_javaIRI(X,U),
        debug(owl2,'calling: ~w . ~w( ~w )',[Fac,M,U]),
        jpl_call(Fac,M,[U],Obj),
        debug(owl2,'called: ~w . ~w( ~w ) = ~w',[Fac,M,U,Obj]).


% --------
% ATOMS
% --------
owlterm_java(Fac,_,OWLTerm,Obj) :-
        atom(OWLTerm),          % undeclared atom; TODO; numbers eg card
        !,
        debug(owl2,'converting to IRI: ~w',[OWLTerm]),
        atom_javaIRI(OWLTerm,U),
        (   is_class(OWLTerm)
        ->  M=getOWLClass
        ;   objectProperty(OWLTerm)
        ->  M=getOWLObjectProperty
        ;   \+ \+ classAssertion(_,M)
        ->  M=getOWLNamedIndividual
        ;   throw(OWLTerm)),
        debug(owl2,'  using: ~w',[M]),
        jpl_call(Fac,M,[U],Obj). % TODO

% --------
% UNTYPED AXIOMS
% --------
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

% special case translation for property chains
owlterm_java(Fac,_,subPropertyOf(propertyChain(PL),P),Obj) :- % e.g. subObjectPropertyOf
        !,
        debug(owl2,'  translating chain ~w',[PL]),
        translate_args_to_java(Fac,[PL,P],[list(objectPropertyExpression),objectPropertyExpression],Objs),
        debug(owl2,'  translated chain to: ~w',[Objs]),
        jpl_call(Fac,getOWLSubPropertyChainOfAxiom,Objs,Obj).

% --------
% TYPED AXIOMS
% --------

% axioms such as subPropertyOf have two typed variants.
% we use type checking which may require the axiom declaration
% to be asserted in the prolog owl2_model database
owlterm_java(Fac,_,UntypedAxiom,Obj) :- % e.g. subObjectPropertyOf
        UntypedAxiom =.. [UntypedPred|Args],      % e.g subPropertyOf p1 p2
        nonvar(UntypedPred),
        owlpredicate_typed(UntypedPred,TypedPred),
        axiom_method(TypedPred,M),   % e.g. subObjectPropertyOf
        TypeCheckGoal =.. [TypedPred|Args],       
        %(   UntypedPred=propertyAssertion
        %->  trace
        %;   true),
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


%% translate_arg_to_java(+Fac,+X,+Type,?Obj)
translate_args_to_java(_Fac,[],[],[]).
translate_args_to_java(Fac,[A|Args],[T|ArgTypes],[Obj|Objs]) :-
        debug(owl2,' translating: ~w  :: ~w',[A,T]),
        translate_arg_to_java(Fac,A,T,Obj),
        debug(owl2,' translated: ~w --> ~w',[A,Obj]),
        translate_args_to_java(Fac,Args,ArgTypes,Objs).

translate_arg_to_java(Fac,L,set(T),Set) :-
        is_list(L),
        !,
        findall(T,member(_,L),Ts),
        translate_args_to_java(Fac,L,Ts,Objs),
        jpl_new('java.util.HashSet',[],Set),
        debug(owl2,' new set: ~w = ~w -- adding objs ~w',[L,Set,Objs]),
        forall(member(Obj,Objs),
               jpl_call(Set,add,[Obj],_)),
        debug(owl2,' made set: ~w ',[Set]).

translate_arg_to_java(Fac,L,list(T),List) :-
        is_list(L),
        !,
        findall(T,member(_,L),Ts),
        translate_args_to_java(Fac,L,Ts,Objs),
        jpl_new('java.util.ArrayList',[],List),
        debug(owl2,' new set: ~w = ~w -- adding objs ~w',[L,List,Objs]),
        forall(member(Obj,Objs),
               jpl_call(List,add,[Obj],_)),
        debug(owl2,' made list: ~w ',[List]).

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
translate_arg_to_java(Fac,Val,literal,Obj) :- % todo - caused by bug in rdf parser
        atom(Val),
        sub_atom(Val,0,_,_,'__'),
        !,
        translate_arg_to_java(Fac,literal(''),literal,Obj).
translate_arg_to_java(Fac,literal(lang(_,Val)),literal,Obj) :- % todo - LANG
        !,
        jpl_call(Fac,getOWLStringLiteral,[Val],Obj).
translate_arg_to_java(Fac,literal(type(_,Val)),literal,Obj) :- % todo - typed constants
        !,
        jpl_call(Fac,getOWLStringLiteral,[Val],Obj).
translate_arg_to_java(Fac,literal(Val),literal,Obj) :- % todo - typed constants
        !,
        jpl_call(Fac,getOWLStringLiteral,[Val],Obj).

translate_arg_to_java(_Fac,X,T,Obj) :- % TODO
        atom(X),
        !,
        atom_javaIRI(X,U),
        jpl_new(T,U,Obj).

translate_arg_to_java(Fac,X,_T,Obj) :-
        atom(X),
        is_class(X),
        !,
        debug(owl2,'converting to IRI: ~w',[X]),
        atom_javaIRI(X,U),
        jpl_call(Fac,getOWLClass,U,Obj).
translate_arg_to_java(Fac,X,_T,Obj) :-
        atom(X),
        \+ \+ classAssertion(_,X),
        !,
        atom_javaIRI(X,U),
        jpl_call(Fac,getOWLNamedIndividual,U,Obj).

translate_arg_to_java(Fac,X,_T,Obj) :-
        atom(X),
        objectProperty(X),
        !,
        atom_javaIRI(X,U),
        jpl_call(Fac,getOWLObjectProperty,U,Obj).




% typed expressions
translate_arg_to_java(Fac,UntypedExpr,_T,Obj) :-
        UntypedExpr =.. [UntypedPred|Args], % eg intersectionOf(L)
        owlpredicate_typed(UntypedPred,TypedPred),
        owlpredicate_arguments(TypedPred,ArgTypes),
        debug(owl2,'  testing for java expr method ~w',[TypedPred]),
        expr_method(TypedPred,Method),
        Check =.. [TypedPred,UntypedExpr], % eg objectIntersectionOf(intersectionOf(L))
        debug(owl2,'  checking expr ~q',[Check]),
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
        debug(owl2,'  checking expr ~q',[Check]),
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
decl_method(dataType,getOWLDatatype,datatype).
decl_method(dataProperty,getOWLDataProperty,_).
decl_method(individual,getOWLNamedIndividual,_). % anonymous individuals?
decl_method(namedIndividual,getOWLNamedIndividual,_). % anonymous individuals?
decl_method(entity,getOWLNamedIndividual,_). % anonymous individuals?

:- discontiguous axiom_method/2,axiom_method/4.

% ----------------------------------------
% untyped axioms
% ----------------------------------------

axiom_method(subClassOf,getOWLSubClassOfAxiom).
axiom_method(equivalentClasses,getOWLEquivalentClassesAxiom).
axiom_method(disjointClasses,getOWLDisjointClassesAxiom).
axiom_method(inverseProperties,getOWLInverseObjectPropertiesAxiom).

axiom_method(symmetricProperty,getOWLSymmetricObjectPropertyAxiom).
axiom_method(asymmetricProperty,getOWLAsymmetricObjectPropertyAxiom).
axiom_method(reflexiveProperty,getOWLReflexiveObjectPropertyAxiom).
axiom_method(irreflexiveProperty,getOWLIrreflexiveObjectPropertyAxiom).

axiom_method(functionalObjectProperty,getOWLFunctionalObjectPropertyAxiom).
axiom_method(dataObjectProperty,getOWLFunctionalDataPropertyAxiom).

axiom_method(objectPropertyAssertion,getOWLObjectPropertyAssertionAxiom).
axiom_method(dataPropertyAssertion,getOWLDataPropertyAssertionAxiom).

axiom_method(differentIndividuals,getOWLDifferentIndividualsAxiom).

% ----------------------------------------
% typed axioms
% ----------------------------------------


%% axiom_method(?Pred,?JavaGetMethod)
axiom_method(subObjectPropertyOf,getOWLSubObjectPropertyOfAxiom).
axiom_method(subDataPropertyOf,getOWLSubDataPropertyOfAxiom).
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

axiom_method(classAssertion,getOWLClassAssertionAxiom).

expr_method(objectIntersectionOf,getOWLObjectIntersectionOf).
expr_method(dataIntersectionOf,getOWLObjectIntersectionOf).
expr_method(objectSomeValuesFrom,getOWLObjectSomeValuesFrom).
expr_method(dataSomeValuesFrom,getOWLDataSomeValiesFrom).
expr_method(objectHasValue,getOWLObjectHasValue).
expr_method(dataHasValue,getOWLDataHasValue).
expr_method(objectAllValuesFrom,getOWLObjectAllValuesFrom).
expr_method(dataAllValuesFrom,getOWLDataAllValuesFrom).
expr_method(objectComplementOf,getOWLObjectComplementOf).
expr_method(dataComplementOf,getOWLDataComplementOf).
expr_method(objectUnionOf,getOWLObjectUnionOf).
expr_method(dataUnionOf,getOWLDataUnionOf).
expr_method(objectOneOf,getOWLObjectOneOf).
expr_method(dataOneOf,getOWLDataOneOf).

expr_method(inverseOf,getOWLObjectInverseOf).

% TODO: is this still required? holdover from owlapi<3, when args were not in same order
expr_method(objectMinCardinality,getOWLObjectMinCardinality,[N,P,CE],[N,P,CE]).
expr_method(objectMinCardinality,getOWLObjectMinCardinality,[N,P],[N,P]).
expr_method(dataMinCardinality,getOWLDataMinCardinality,[N,P,CE],[N,P,CE]).
expr_method(dataMinCardinality,getOWLDataMinCardinality,[N,P],[N,P]).
expr_method(objectMaxCardinality,getOWLObjectMaxCardinality,[N,P,CE],[N,P,CE]).
expr_method(objectMaxCardinality,getOWLObjectMaxCardinality,[N,P],[N,P]).
expr_method(dataMaxCardinality,getOWLDataMaxCardinality,[N,P,CE],[N,P,CE]).
expr_method(dataMaxCardinality,getOWLDataMaxCardinality,[N,P],[N,P]).
expr_method(objectExactCardinality,getOWLObjectExactCardinality,[N,P,CE],[N,P,CE]).
expr_method(objectExactCardinality,getOWLObjectExactCardinality,[N,P],[N,P]).
expr_method(dataExactCardinality,getOWLDataExactCardinality,[N,P,CE],[N,P,CE]).
expr_method(dataExactCardinality,getOWLDataExactCardinality,[N,P],[N,P]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% incrememntal classifier     %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
  TODO - need to detect which changes have been applied

  add reasoner_reclassify_hook
  
new_incremental_classifier(pellet_incremental(R)) :-
	require_manager(Man),
	create_factory(Man,Fac),
        build_ontology(Man,Fac,Ont),
        jpl_new('com.clarkparsia.modularity.IncrementalClassifier',[Ont],R),
        jpl_call(R,classify,[],_).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Hooks for owl2_reasoner.pl  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- multifile owl2_reasoner:initialize_reasoner_hook/3.
:- multifile owl2_reasoner:reasoner_tell_hook/2.
:- multifile owl2_reasoner:reasoner_tell_all_hook/1.
:- multifile owl2_reasoner:reasoner_ask_hook/2.
:- multifile owl2_reasoner:reasoner_check_consistency_hook/2.
:- multifile owl2_reasoner:reasoner_unsatisfiable_class_hook/2.


owl2_reasoner:initialize_reasoner_hook(Type,R,Opts) :-
	wrapped_reasoner(Type), % choose arbitrary if not defined
        !,
	owl2_reasoner:initialize_reasoner_hook(owlapi(Type),R,Opts).
owl2_reasoner:initialize_reasoner_hook(owlapi(Type),owlapi_reasoner(R,Fac,Opts),Opts) :-
	!,
	require_manager(Man),
	create_factory(Man,Fac),
        statistics(cputime,T1),
        print_message(informational,bench(reasoner,T1)),
        build_ontology(Man,Fac,Ont,Opts),
	create_reasoner(Ont,Type,R),
        statistics(cputime,T2),
        print_message(informational,bench(reasoner,T1,T2)).


%reasoner_tell_hook(R,Axiom) :- foo.

owl2_reasoner:reasoner_tell_all_hook(owlapi_reasoner(OWLReasoner,Fac,Opts)) :-
	build_ontology(Man,Fac,Ont,Opts),
	reasoner_classify(OWLReasoner,Man,Ont).

	
%owl2_reasoner:reasoner_ask_hook(R,Axiom) :-
%	var(Axiom), % allow all?
%	!,
%	throw(error(reasoner(R,Axiom))).

owl2_reasoner:reasoner_ask_hook(owlapi_reasoner(R,Fac,_Opts),subClassOf(A,B)) :-
        \+ is_jagr(R),
	reasoner_subClassOf(R,Fac,A,B),
        \+ nothing(A).


owl2_reasoner:reasoner_ask_hook(owlapi_reasoner(R,Fac,_Opts),subClassOf(A,B),IsDirect) :-
	reasoner_subClassOf(R,Fac,A,B,IsDirect),
        \+ nothing(A).

% experimental
owl2_reasoner:reasoner_ask_hook(owlapi_reasoner(R,Fac,_Opts),subClassOf(A,B)) :-
        is_jagr(R),
	reasoner_superClassExpressionOf(R,Fac,B,A),
        \+ nothing(A).

owl2_reasoner:reasoner_ask_hook(owlapi_reasoner(R,Fac,_Opts),directSubClassOf(A,B)) :-
	reasoner_subClassOf(R,Fac,A,B,true),
        \+ nothing(A).


owl2_reasoner:reasoner_ask_hook(owlapi_reasoner(R,Fac,_Opts),equivalentClasses([A,B])) :-
	reasoner_equivalent_to(R,Fac,A,B),
        \+ nothing(A).

owl2_reasoner:reasoner_ask_hook(owlapi_reasoner(R,Fac,_Opts),classAssertion(C,I)) :-
	reasoner_individualOf(R,Fac,I,C).

owl2_reasoner:reasoner_ask_hook(owlapi_reasoner(R,Fac,_Opts),classAssertion(C,I),IsDirect) :-
	reasoner_individualOf(R,Fac,I,C,IsDirect).

owl2_reasoner:reasoner_ask_hook(owlapi_reasoner(R,Fac,_Opts),propertyAssertion(P,A,B)) :-
	reasoner_objectPropertyAssertion(R,Fac,P,A,B).

owl2_reasoner:reasoner_unsatisfiable_class_hook(owlapi_reasoner(R,_Fac,_Opts),C) :-
	unsatisfiable_class(R,C).

owl2_reasoner:reasoner_check_consistency_hook(owlapi_reasoner(R,_Fac,_Opts),V) :-
        debug(reasoner,'checking consistency',[]),
        (   is_consistent(R)
        ->  V=true
        ;   V=false).

% true if R is the experimental java graph reasoner (in owltools)
%  this allows super class expressions to be anonymous
is_jagr(R) :-
        jpl_call(R,getClass,[],JC),jpl_call(JC,getName,[],CN),
        CN='owltools.reasoner.GraphReasoner'.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% utils                       %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

show_java_memory_info :-
        java_memory_info(M,TM,FM,MM),
        Mb is M/(1024*1024),
        MMb is MM/(1024*1024),
        format(user_error,'Mem: ~wmb (~wb) // Max: ~wmb (~wb) // ~w - ~w',[Mb,M,MMb,MM,TM,FM]).
        %print_message(informational,memory(M,TM,FM)).
java_memory_info(M,TM,FM,MM) :-
        java_gc,
        java_gc,
        java_gc,
        jpl_call('java.lang.Runtime',getRuntime,[],RunTime),
        jpl_call(RunTime,totalMemory,[],TM),
        jpl_call(RunTime,freeMemory,[],FM),
        jpl_call(RunTime,maxMemory,[],MM),
        M is TM-FM.

java_gc :-     jpl_call('java.lang.System',gc,[],_).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% messages                    %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prolog:message(bench(M,T1)) -->
        ['initializing: ',M,' ',T1].
prolog:message(bench(M,T1,T2)) -->
        {TD is T2-T1},
        ['completed: ',M,' ',T2,' time: ',TD].


/** <module> bridge to java OWLAPI

---+ Synopsis

using a reasoner:

==
reasoner_test :-
        initialize_reasoner(pellet,R),
        forall(reasoner_ask(subClassOf(A,B)),
               format('~w SubClassOf ~w~n',[A,B])).
==  

To use this interactively, make sure to start-up prolog with JPL and
everything in your classpath. You can use the thea-jpl wrapper script.

To start a prolog session:

==
thea-jpl --prolog
==

To query reasoner results:

==
thea-jpl --reasoner pellet testfiles/pizza.owl --reasoner-ask "subClassOf(A,B)"
==

---+ Details

This module is intended to interface with the OWLAPI

  http://owlapi.sourceforge.net/

This provides access to reasoners such as Pellet and FaCT++, as well
as OWLAPI parsers and renderers. You can also use this if you want
additional capabilities provided by the OWLAPI.


---+ Hooks

It provides hooks into both owl2_reasoner.pl and owl2_io.pl

---++ I/O Hooks

You can use the format =|owlapi(Format)|= to use the OWLAPI for
reading or writing. E.g.

==
save_axioms('my.owl',owlapi(manchester)).
==

Supported values:

 * owlapi(manchester)
 * owlapi(owlxml)
 * owlapi(owlrdf)

 More can be added easily on request

---++ Reasoner Hooks

You can use any of

 * pellet
 * factpp
 * hermit

As arguments to reasoner_initialize/2.




---+ Pre-Requisites

JPL is required for this module

Note that this module is not required for the rest of Thea2

@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/
	
