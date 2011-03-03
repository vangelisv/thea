/* -*- Mode: Prolog -*- */

% **********************************************************************
%                                OWL to RDF Generator
% Author: Vangelis Vassiliadis
% Change Log:
%         Feb 09: Ported to Thea2 [CJM]
%	  Mar 07: Version 0.5.5: Changes to the use_module and
%	  definitions for Thea 0.5.5 release. 
%         Sep 05: Initial release 0.4 (as part of Thea OWL
%	  Prolog library) 
%             
% **********************************************************************

:- module(owl2_export_rdf,
	  [owl_generate_rdf/2, % FileName, RDF_Load_Mode (complete/not)
           owl_generate_rdf/3, % Ontology,FileName, RDF_Load_Mode (complete/not)
           owl_generate_rdf/4, % Ontology,FileName, RDF_Load_Mode (complete/not), Opts
           owl_synchronize_to_rdf/0,
           owl_synchronize_to_rdf/1,
           owl_rdf2n3/0 		    
	  ]).

:- use_module(owl2_model).
:- use_module(owl2_from_rdf).
:- use_module(swrl_rdf_hooks).
:- use_module(library('semweb/rdf_db')).

:- multifile owl2_io:save_axioms_hook/3.

owl2_io:save_axioms_hook(File,ttl,Opts) :-
        ensure_loaded(library('semweb/rdf_turtle_write')),
        owl2_io:save_axioms_hook(File,owl,[rdf_syntax(ttl)|Opts]).

owl2_io:save_axioms_hook(File,owl,Opts) :-
        (   member(rdf_load_mode(RDF_Load_Mode),Opts)
        ->  true
        ;   RDF_Load_Mode=complete),
        (   var(File)
        ->  tmp_file(owl,File),
            IsTemp=true
        ;   IsTemp=false),

        % ontology merging:
        %  user can select this to indicate that
        %  all axioms should be treated as belonging to the
        %  same ontology.
        % 
        % bind Merge only if user selects this option
        (   member(merge(Merge),Opts)
        ->  true
        ;   true),

        % select which ontology is to be saved. If none is selected,
        %  choose this automatically, but only if there is only one
        %  ontology in memory
        (   member(ontology(O),Opts)
        ->  true
        ;   setof(O,ontology(O),[O])
        ->  debug(owl_generate_rdf,'auto-selecting ontology: ~w',[O])
        ;   true),

        % if the user did not select an ontology, and there is not
        %  exactly 1 ontology in memory, then they are implicitly choosing
        %  to ignore ontologyAxiom/2 and save all axioms in one file.
        (   (   var(O)
            ;   \+ ontologyAxiom(_,_))
        ->  (   nonvar(Merge),Merge=false
            ->  throw(error('cannot override merge(true) unless there is a single ontology'))
            ;   Merge=true,
                debug(owl_generate_rdf,'ignoring source of axiom, bundling all together',[]))
        ;   true),

        % add Merge to Opts
        (   var(Merge)
        ->  Opts2=Opts
        ;   Opts2=[merge(Merge)|Opts]),

        owl_generate_rdf(O,File,RDF_Load_Mode,Opts2),

        % hack to allow 'saving' to standard output
        (   IsTemp
        ->  sformat(Cmd,'cat ~w',[File]),
            shell(Cmd)
        ;   true).

%owl2_from_rdf:owl_repository('http://www.w3.org/TR/2003/PR-owl-guide-20031209/food','testfiles/food.owl').

%% owl_generate_rdf(+FileName,+RDF_Load_Mode) is det
% see owl_generate_rdf/1 - derives ontology using ontology/1
owl_generate_rdf(FileName,RDF_Load_Mode) :-
        setof(Ontology,ontology(Ontology),[Ontology]),
        !,
        owl_generate_rdf(Ontology,FileName,RDF_Load_Mode).
owl_generate_rdf(FileName,RDF_Load_Mode) :-
        % leave Ontology variable unbound - this will save all ontologies together
        owl_generate_rdf(_,FileName,RDF_Load_Mode).

%% owl_generate_rdf(+Ontology,+FileName,+RDF_Load_Mode) is det
%
% as owl_generate_rdf/4, default Opts
owl_generate_rdf(Ontology,FileName,RDF_Load_Mode) :- 
        owl_generate_rdf(Ontology,FileName,RDF_Load_Mode,[]).


%% owl_generate_rdf(+Ontology,+FileName,+RDF_Load_Mode,+Opts:list) is det
%
%   writes in-memory ontology from owl2_model to an RDF-OWL file.
%   You can also use save_axioms/2 with Format='owl'.
%   With save_axioms/3 use rdf_load_mode(RDF_Load_Mode) as
%   an option.
%
% @param Ontology - IRI
% @param FileName - path to save
% @param RDF_Load_Mode (complete/not)
owl_generate_rdf(Ontology,FileName,RDF_Load_Mode,Opts) :- 
        owl_generate_rdf_in_memory(Ontology,RDF_Load_Mode,Opts),
        (   member(rdf_syntax(ttl),Opts) % TODO - use rdf_db hooks
        ->  rdf_save_turtle(FileName,Opts)
	;   rdf_db:rdf_save(FileName)).

owl_generate_rdf_in_memory(Ontology,RDF_Load_Mode,Opts) :- 
	(   RDF_Load_Mode=complete -> rdf_retractall(_,_,_); true),
	retractall(blanknode_gen(_,_)),retractall(blanknode(_,_,_)),
        % export a fake ontology directive if none specified
        (   var(Ontology)
        ->  owl2_export_axiom(ontology('http://example.org'),_)
	;   owl2_export_axiom(ontology(Ontology),_)),
        (   member(merge(true),Opts)
        ->  true
        ;   SrcOntology=Ontology),
	debug(owl_generate_rdf,'exporting Ont:~w',[Ontology]),
	debug(owl_generate_rdf,'  gen: ontologyAxiom/2',[]),
	forall(ontologyAxiom(SrcOntology,Axiom),
	       (owl2_export_axiom(Axiom,main_triple(S,P,O)),
		owl2_export_annotation(Axiom,'owl:Axiom',S,P,O))),
	debug(owl_generate_rdf,'  stray axioms',[]),
        % TODO - better way of doing this - stray axioms
        (   var(Ontology)
	->  forall((axiom(Axiom),\+ontologyAxiom(_,Axiom)),
                   (   owl2_export_axiom(Axiom,main_triple(S,P,O)),
                       owl2_export_annotation(Axiom,'owl:Axiom',S,P,O)))
        ;   true),
        % TODO - make SWRL export a hook?
        forall(axiom(implies(A,C)),
               owl2_export_axiom(implies(A,C),_)).

%% owl_synchronize_to_rdf is det
%% owl_synchronize_to_rdf(Ont) is det
%
% translates existing owl2_model.pl database into current rdf_db
owl_synchronize_to_rdf :-
        setof(Ont,ontology(Ont),Onts),
        !,
        maplist(owl_synchronize_to_rdf,Onts).
owl_synchronize_to_rdf :-
        owl_generate_rdf_in_memory(_,false,[]).
owl_synchronize_to_rdf(Ont) :-
        !,
        debug(owl_sync,'Synchronizing: ~w',[Ont]),
        % this is not very efficient!
        % see email http://groups.google.com/group/thea-owl-lib/browse_thread/thread/d9fcc8cc28a2d347
        % TODO: refactor code such that rdf can be asserted in other graphs during export
        owl_generate_rdf_in_memory(Ont,false,[]),
        debug(owl_sync,'Copying from user to ~w',[Ont]),
        rdf_transaction(forall(rdf(S,P,O,user),
                               rdf_assert(S,P,O,Ont))),
        debug(owl_sync,' Clearing user',[]),
        rdf_retractall(_,_,_,user).





/*
owl_rdf2n3
     Prints out the RDF triples in N3 notation.   
*/
owl_rdf2n3 :-	
    rdf_db:rdf(S,P,O),
    collapse_ns(S,S1,':',[]),collapse_ns(P,P1,':',[]),collapse_ns(O,O1,':',[]),
    write(S1), write(' '), write(P1), write(' '), write(O1), write(' .'),nl,
    fail.

%% owl2_export_axiom(+Axiom, ?Triple) is semidet
/*
owl2_export_axiom(X,main_triple(Node,_,_)) :-
	blanknode_gen(Node,X),!.
*/

owl2_export_axiom(ontology(O),main_triple(O,'rdf:type','owl:Ontology')) :-
	owl_rdf_assert(O,'rdf:type','owl:Ontology'),
	forall(ontologyImport(O,Import),owl_rdf_assert(O,'owl:imports',Import)),!.


owl2_export_axiom(class(C1),main_triple(C,'rdf:type','owl:Class')) :-
        translate_iri(C1,C),
	owl_rdf_assert(C,'rdf:type','owl:Class'),!.

owl2_export_axiom(objectProperty(C1),main_triple(C,'rdf:type','owl:ObjectProperty')) :-
        translate_iri(C1,C),
	owl_rdf_assert(C,'rdf:type','owl:ObjectProperty'),!.

owl2_export_axiom(dataProperty(C1),main_triple(C,'rdf:type','owl:DatatypeProperty')) :-
        translate_iri(C1,C),
	owl_rdf_assert(C,'rdf:type','owl:DatatypeProperty'),!.

owl2_export_axiom(annotationProperty(C1),main_triple(C,'rdf:type','owl:AnnotationProperty')) :-
        translate_iri(C1,C),
	owl_rdf_assert(C,'rdf:type','owl:AnnotationProperty'),!.

owl2_export_axiom(namedIndividual(C1),main_triple(C,'rdf:type','owl:NamedIndividual')) :-
        translate_iri(C1,C),
	owl_rdf_assert(C,'rdf:type','owl:NamedIndividual'),!.

owl2_export_axiom(subClassOf(C1,C2),main_triple(TC1,'rdfs:subClassOf',TC2)) :-
	owl2_export_axiom(C1,main_triple(TC1,_,_)),
	owl2_export_axiom(C2,main_triple(TC2,_,_)),
	owl_rdf_assert(TC1,'rdfs:subClassOf',TC2),!.

owl2_export_axiom(equivalentClasses([X,Y]),main_triple(Tx,'owl:equivalentClass',Ty)) :-
	owl2_export_axiom(X,main_triple(Tx,_,_)),
	owl2_export_axiom(Y,main_triple(Ty,_,_)),!,
	owl_rdf_assert(Tx,'owl:equivalentClass',Ty).

owl2_export_axiom(equivalentClasses(L),main_triple(Tx,'owl:equivalentClass',Ty)) :-
        select(X,L,L2),
        select(Y,L2,[_|_]),
	owl2_export_axiom(X,main_triple(Tx,_,_)),
	owl2_export_axiom(Y,main_triple(Ty,_,_)),!,
	owl_rdf_assert(Tx,'owl:equivalentClass',Ty).

owl2_export_axiom(disjointClasses([X,Y]),main_triple(Tx,'owl:disjointWith',Ty)) :-
	owl2_export_axiom(X,main_triple(Tx,_,_)),
	owl2_export_axiom(Y,main_triple(Ty,_,_)),!,
	owl_rdf_assert(Tx,'owl:disjointWith',Ty).

owl2_export_axiom(disjointClasses(List),main_triple(BNode,'rdf:type','owl:AlldisjointClasses')) :-
	as2rdf_bnode(disjointClasses(List),BNode),
	owl2_export_list(List,LNode),
	owl_rdf_assert(BNode,'rdf:type','owl:AlldisjointClasses'),
	owl_rdf_assert(BNode,'owl:members',LNode),!.

owl2_export_axiom(disjointUnion(C,Rest),main_triple(Tc,'owl:disjointUnionOf',LNode)) :-
	owl2_export_axiom(C,main_triple(Tc,_,_)),
	owl2_export_list(Rest,LNode),
	owl_rdf_assert(Tc,'owl:disjointUnionOf',LNode),!.

owl2_export_axiom(subPropertyOf(propertyChain(PL),P2),main_triple(Tp2,'owl:propertyChainAxiom',LNode)) :-
	owl2_export_axiom(P2,main_triple(Tp2,_,_)),
	owl2_export_list(PL,LNode),
	owl_rdf_assert(Tp2,'owl:propertyChainAxiom',LNode),!.

owl2_export_axiom(subPropertyOf(P1,P2),main_triple(Tp1,'rdfs:subPropertyOf',Tp2)) :-
	owl2_export_axiom(P1,main_triple(Tp1,_,_)),
	owl2_export_axiom(P2,main_triple(Tp2,_,_)),
	owl_rdf_assert(Tp1,'rdfs:subPropertyOf',Tp2),!.


owl2_export_axiom(equivalentProperties([X,Y]),main_triple(Tx,'owl:equivalentProperty',Ty)) :-
	owl2_export_axiom(X,main_triple(Tx,_,_)),
	owl2_export_axiom(Y,main_triple(Ty,_,_)),
	owl_rdf_assert(Tx,'owl:equivalentProperty',Ty),!.

% TODO: >2 equivalent Properties

owl2_export_axiom(disjointProperties([X,Y]),main_triple(Tx,'owl:propertyDisjointWith',Ty)) :-
	owl2_export_axiom(X,main_triple(Tx,_,_)),
	owl2_export_axiom(Y,main_triple(Ty,_,_)),
	owl_rdf_assert(Tx,'owl:propertyDisjointWith',Ty),!.


owl2_export_axiom(disjointProperties(List),main_triple(BNode,'rdf:type','owl:AlldisjointProperties')) :-
	as2rdf_bnode(disjointClasses(List),BNode),
	owl2_export_list(List,LNode),
	owl_rdf_assert(BNode,'rdf:type','owl:AlldisjointProperties'),
	owl_rdf_assert(BNode,'owl:members',LNode),!.


owl2_export_axiom(propertyDomain(PE,CE),main_triple(Tpe,'rdfs:domain',Tce)) :-
	owl2_export_axiom(PE,main_triple(Tpe,_,_)),
	owl2_export_axiom(CE,main_triple(Tce,_,_)),
	owl_rdf_assert(Tpe,'rdfs:domain',Tce),!.

owl2_export_axiom(propertyRange(PE,CE),main_triple(Tpe,'rdfs:range',Tce)) :-
	owl2_export_axiom(PE,main_triple(Tpe,_,_)),
	owl2_export_axiom(CE,main_triple(Tce,_,_)),
	owl_rdf_assert(Tpe,'rdfs:range',Tce),!.

owl2_export_axiom(inverseProperties(P1,P2),main_triple(Tp1,'owl:inverseOf',Tp2)) :-
	owl2_export_axiom(P1,main_triple(Tp1,_,_)),
	owl2_export_axiom(P2,main_triple(Tp2,_,_)),
	owl_rdf_assert(Tp1,'owl:inverseOf',Tp2),!.

owl2_export_axiom(functionalProperty(P),main_triple(Tp,'rdf:type','owl:FunctionalProperty')) :-
	owl2_export_axiom(P,main_triple(Tp,_,_)),owl_rdf_assert(Tp,'rdf:type','owl:FunctionalProperty'),!.

owl2_export_axiom(inverseFunctionalProperty(P),main_triple(Tp,'rdf:type','owl:InverseFunctionalProperty')) :-
	owl2_export_axiom(P,main_triple(Tp,_,_)),owl_rdf_assert(Tp,'rdf:type','owl:InverseFunctionalProperty'),!.

owl2_export_axiom(reflexiveProperty(P),main_triple(Tp,'rdf:type','owl:ReflexiveProperty')) :-
	owl2_export_axiom(P,main_triple(Tp,_,_)),owl_rdf_assert(Tp,'rdf:type','owl:ReflexiveProperty'),!.

owl2_export_axiom(irreflexiveProperty(P),main_triple(Tp,'rdf:type','owl:IrreflexiveProperty')) :-
	owl2_export_axiom(P,main_triple(Tp,_,_)),owl_rdf_assert(Tp,'rdf:type','owl:IrreflexiveProperty'),!.

owl2_export_axiom(symmetricProperty(P),main_triple(Tp,'rdf:type','owl:SymmetricProperty')) :-
	owl2_export_axiom(P,main_triple(Tp,_,_)),owl_rdf_assert(Tp,'rdf:type','owl:SymmetricProperty'),!.

owl2_export_axiom(asymmetricProperty(P),main_triple(Tp,'rdf:type','owl:AsymmetricProperty')) :-
	owl2_export_axiom(P,main_triple(Tp,_,_)),owl_rdf_assert(Tp,'rdf:type','owl:AsymmetricProperty'),!.

owl2_export_axiom(transitiveProperty(P),main_triple(Tp,'rdf:type','owl:TransitiveProperty')) :-
	owl2_export_axiom(P,main_triple(Tp,_,_)),owl_rdf_assert(Tp,'rdf:type','owl:TransitiveProperty'),!.

owl2_export_axiom(hasKey([C|Rest]),main_triple(Tc,'owl:hasKey',LNode)) :-
	owl2_export_axiom(C,main_triple(Tc,_,_)),
	owl2_export_list(Rest,LNode),
	owl_rdf_assert(Tc,'owl:hasKey',LNode),!.

% 
% Individuals
% 

owl2_export_axiom(sameIndividual([X,Y]),main_triple(Tx,'owl:sameAs',Ty)) :-
	owl2_export_axiom(X,main_triple(Tx,_,_)),
	owl2_export_axiom(Y,main_triple(Ty,_,_)),
	owl_rdf_assert(Tx,'owl:sameAs',Ty),!.

owl2_export_axiom(differentIndividuals([X,Y]),main_triple(Tx,'owl:differentFrom',Ty)) :-
	owl2_export_axiom(X,main_triple(Tx,_,_)),
	owl2_export_axiom(Y,main_triple(Ty,_,_)),
	owl_rdf_assert(Tx,'owl:differentFrom',Ty),!.

owl2_export_axiom(differentIndividuals(List),main_triple(BNode,'rdf:type','owl:AllDifferent')) :-
	as2rdf_bnode(differentIndividuals(List),BNode),
	owl2_export_list(List,LNode),
	owl_rdf_assert(BNode,'rdf:type','owl:AllDifferent'),
	owl_rdf_assert(BNode,'owl:members',LNode),!.

owl2_export_axiom(classAssertion(CE,A),main_triple(Ta,'rdf:type',Tce)) :-
	owl2_export_axiom(A,main_triple(Ta,_,_)),
	owl2_export_axiom(CE,main_triple(Tce,_,_)),
	owl_rdf_assert(Ta,'rdf:type',Tce),
        !.

owl2_export_axiom(propertyAssertion(P,A1,A2),main_triple(Ta1,Tp,Ta2)) :-
	owl2_export_axiom(P,main_triple(Tp,_,_)),
	owl2_export_axiom(A1,main_triple(Ta1,_,_)),
	owl2_export_axiom(A2,main_triple(Ta2,_,_)),
	owl_rdf_assert(Ta1,Tp,Ta2),!.

owl2_export_axiom(propertyAssertion(inverseOf(P),A1,A2),main_triple(Ta2,Tp,Ta1)) :-
	owl2_export_axiom(P,main_triple(Tp,_,_)),
	owl2_export_axiom(A1,main_triple(Ta1,_,_)),
	owl2_export_axiom(A2,main_triple(Ta2,_,_)),
	owl_rdf_assert(Ta2,Tp,Ta1),!.

owl2_export_axiom(negativePropertyAssertion(P,A1,A2),main_triple(BNode,'rdf:type','owl:NegativePropertyAssertion')) :-
	as2rdf_bnode(negativePropertyAssertion(P,A1,A2),BNode),
	owl2_export_axiom(P,main_triple(Tp,_,_)),
	owl2_export_axiom(A1,main_triple(Ta1,_,_)),
	owl2_export_axiom(A2,main_triple(Ta2,_,_)),
	owl_rdf_assert(BNode,'rdf:type','owl:NegativePropertyAssertion'),
	owl_rdf_assert(BNode,'owl:sourceIndividual',Ta1),
	owl_rdf_assert(BNode,'owl:assertionProperty',Tp),
	owl_rdf_assert(BNode,'owl:targetIndividual',Ta2),!.


%
% AnnotationAssestions
%

owl2_export_axiom(annotationAssertion(AP,As,Av),main_triple(TAs,AP,TAv)) :- 
        atom(As), % TODO: see issue#8. axiom annotations result is As=annotation(_,_,_)
        !,
	owl2_export_axiom(As,main_triple(TAs,_,_)),
	owl2_export_axiom(Av,main_triple(TAv,_,_)),
	owl_rdf_assert(TAs,AP,TAv),!.

owl2_export_axiom(annotationAssertion(Ap,As,Av),main_triple(TAs,TAp,TAv)) :-
        as2rdf_bnode(As,BNode), % As is complex term - see above
	owl2_export_axiom(Ap,main_triple(TAp,_,_)),
	owl2_export_axiom(Av,main_triple(TAv,_,_)),
	owl2_export_axiom(BNode,main_triple(TAs,_,_)).
        %reify(TAs,TAp,TAv,_,_,_).

%
% Property Expressions (Descriptions)
%
owl2_export_axiom(inverseOf(P),main_triple(BNode,'owl:inverseOf',P)) :-
	as2rdf_bnode(inverseOf(P),BNode),
	owl_rdf_assert(BNode,'owl:inverseOf', P),!.

%
% Class Expressions (Descriptions)
%

owl2_export_axiom(intersectionOf([E|Rest]),main_triple(BNode,'rdf:type',Type)) :-
	as2rdf_bnode(intersectionOf([E|Rest]),BNode),
	owl2_export_list([E|Rest],LNode),
	(   classExpression(E) -> Type = 'owl:Class'; Type = 'owl:Datatype'),
	owl_rdf_assert(BNode,'rdf:type',Type),
	owl_rdf_assert(BNode,'owl:intersectionOf', LNode),
        !.
owl2_export_axiom(unionOf([E|Rest]),main_triple(BNode,'rdf:type',Type)) :-
	as2rdf_bnode(unionOf([E|Rest]),BNode),
	owl2_export_list([E|Rest],LNode),
	(   classExpression(E) -> Type = 'owl:Class'; Type = 'owl:Datatype'),
	owl_rdf_assert(BNode,'rdf:type',Type),
	owl_rdf_assert(BNode,'owl:unionOf', LNode),!.	  

owl2_export_axiom(oneOf([E|Rest]),main_triple(BNode,'rdf:type',Type)) :-
	as2rdf_bnode(oneOf([E|Rest]),BNode),
	owl2_export_list([E|Rest],LNode),
	(   classExpression(E)
        ->  Type = 'owl:Class'
        ;   Type = 'owl:Datatype'),
	owl_rdf_assert(BNode,'rdf:type',Type),
	owl_rdf_assert(BNode,'owl:oneOf', LNode),!.	  

owl2_export_axiom(datatypeRestriction(DT,FVs),main_triple(BNode,'rdf:type','rdfs:Datatype')) :-
	as2rdf_bnode(datatypeRestriction(DT,FVs),BNode),
	owl_rdf_assert(BNode,'rdf:type','rdfs:Datatype'),
	owl2_export_axiom(DT,main_triple(Tpe,_,_)),owl_rdf_assert(BNode,'owl:onDatatype',Tpe),
	owl2_export_list(FVs,LNode),
	owl_rdf_assert(BNode,'owl:withRestrictions',LNode).

owl2_export_axiom(facetRestriction(F,V),main_triple(BNode,F2,V2)) :-
	(   sub_atom(F,_,_,_,'#')
	->  F2=F2
	;   atom_concat('xsd:',F,F2)),
	as2rdf_bnode(facetRestriction(F2,V2),BNode),
	owl_rdf_assert(BNode,F,V). % TODO -- check

owl2_export_axiom(complementOf(E),main_triple(BNode,'rdf:type',Type)) :-
	as2rdf_bnode(complementOf(E),BNode),
	owl2_export_axiom(E,main_triple(Te,_,_)),
	(   classExpression(E) -> Type = 'owl:complementOf'; Type = 'owl:datatypeComplementOf'),
	owl_rdf_assert(BNode,'owl:complementOf', Te),!.	  



owl2_export_axiom(someValuesFrom(PE,CEorDR),main_triple(BNode,'rdf:type','owl:Restriction')) :-
	as2rdf_bnode(someValuesFrom(PE,CEorDR),BNode),
	owl_rdf_assert(BNode,'rdf:type','owl:Restriction'),
	owl2_export_axiom(PE,main_triple(Tpe,_,_)),owl_rdf_assert(BNode,'owl:onProperty',Tpe),
	owl2_export_axiom(CEorDR,main_triple(Tce,_,_)),owl_rdf_assert(BNode,'owl:someValuesFrom',Tce),!.


owl2_export_axiom(allValuesFrom(PE,CEorDR),main_triple(BNode,'rdf:type','owl:Restriction')) :-
	as2rdf_bnode(allValuesFrom(PE,CEorDR),BNode),
	owl_rdf_assert(BNode,'rdf:type','owl:Restriction'),
	owl2_export_axiom(PE,main_triple(Tpe,_,_)),owl_rdf_assert(BNode,'owl:onProperty',Tpe),
	owl2_export_axiom(CEorDR,main_triple(Tce,_,_)),owl_rdf_assert(BNode,'owl:allValuesFrom',Tce),!.

owl2_export_axiom(hasValue(PE,Value),main_triple(BNode,'rdf:type','owl:Restriction')) :-
	as2rdf_bnode(hasValue(PE,Value),BNode),
	owl_rdf_assert(BNode,'rdf:type','owl:Restriction'),
	owl2_export_axiom(PE,main_triple(Tpe,_,_)),owl_rdf_assert(BNode,'owl:onProperty',Tpe),
	owl2_export_axiom(Value,main_triple(TValue,_,_)),owl_rdf_assert(BNode,'owl:hasValue',TValue),!.

owl2_export_axiom(hasSelf(OPE),main_triple(BNode,'rdf:type','owl:Restriction')) :-
	as2rdf_bnode(hasValue(OPE),BNode),
	owl_rdf_assert(BNode,'rdf:type','owl:Restriction'),
	owl2_export_axiom(OPE,main_triple(Tope,_,_)),owl_rdf_assert(BNode,'owl:onProperty',Tope),
	owl_rdf_assert(BNode,'owl:hasSelf',	literal(type('http://www.w3.org/2001/XMLSchema#boolean','true'))),!.


owl2_export_axiom(minCardinality(N,OPE),main_triple(BNode,'rdf:type','owl:Restriction')) :-
	as2rdf_bnode(minCardinality(N,OPE),BNode),
	owl_rdf_assert(BNode,'rdf:type','owl:Restriction'),
	owl_rdf_assert(BNode,'owl:minCardinality',literal(type('http://www.w3.org/2001/XMLSchema#nonNegativeInteger',N))),
	owl2_export_axiom(OPE,main_triple(Tope,_,_)),owl_rdf_assert(BNode,'owl:onProperty',Tope),!.

owl2_export_axiom(minCardinality(N,OPE,CEorDR),main_triple(BNode,'rdf:type','owl:Restriction')) :-
	as2rdf_bnode(minCardinality(N,OPE,CEorDR),BNode),
	owl_rdf_assert(BNode,'rdf:type','owl:Restriction'),
	owl_rdf_assert(BNode,'owl:minQualifiedCardinality',literal(type('http://www.w3.org/2001/XMLSchema#nonNegativeInteger',N))),
	owl2_export_axiom(OPE,main_triple(Tope,_,_)),owl_rdf_assert(BNode,'owl:onProperty',Tope),
	owl2_export_axiom(CEorDR,main_triple(Tce,_,_)),	
	(   classExpression(CEorDR) -> owl_rdf_assert(BNode,'owl:onClass',Tce); owl_rdf_assert(BNode,'owl:onDataRange',Tce)),!.


owl2_export_axiom(maxCardinality(N,OPE),main_triple(BNode,'rdf:type','owl:Restriction')) :-
	as2rdf_bnode(maxCardinality(N,OPE),BNode),
	owl_rdf_assert(BNode,'rdf:type','owl:Restriction'),
	owl_rdf_assert(BNode,'owl:maxCardinality',literal(type('http://www.w3.org/2001/XMLSchema#nonNegativeInteger',N))),
	owl2_export_axiom(OPE,main_triple(Tope,_,_)),owl_rdf_assert(BNode,'owl:onProperty',Tope),!.

owl2_export_axiom(maxCardinality(N,OPE,CEorDR),main_triple(BNode,'rdf:type','owl:Restriction')) :-
	as2rdf_bnode(maxCardinality(N,OPE,CEorDR),BNode),
	owl_rdf_assert(BNode,'rdf:type','owl:Restriction'),
	owl_rdf_assert(BNode,'owl:maxQualifiedCardinality',literal(type('http://www.w3.org/2001/XMLSchema#nonNegativeInteger',N))),
	owl2_export_axiom(OPE,main_triple(Tope,_,_)),owl_rdf_assert(BNode,'owl:onProperty',Tope),
	owl2_export_axiom(CEorDR,main_triple(Tce,_,_)),	
	(   classExpression(CEorDR) -> owl_rdf_assert(BNode,'owl:onClass',Tce); owl_rdf_assert(BNode,'owl:onDataRange',Tce)),!.


owl2_export_axiom(exactCardinality(N,OPE),main_triple(BNode,'rdf:type','owl:Restriction')) :-
	as2rdf_bnode(exactCardinality(N,OPE),BNode),
	owl_rdf_assert(BNode,'rdf:type','owl:Restriction'),
	owl_rdf_assert(BNode,'owl:cardinality',literal(type('http://www.w3.org/2001/XMLSchema#nonNegativeInteger',N))),
	owl2_export_axiom(OPE,main_triple(Tope,_,_)),owl_rdf_assert(BNode,'owl:onProperty',Tope),!.

owl2_export_axiom(exactCardinality(N,OPE,CEorDR),main_triple(BNode,'rdf:type','owl:Restriction')) :-
	as2rdf_bnode(exactCardinality(N,OPE,CEorDR),BNode),
	owl_rdf_assert(BNode,'rdf:type','owl:Restriction'),
	owl_rdf_assert(BNode,'owl:qualifiedCardinality',literal(type('http://www.w3.org/2001/XMLSchema#nonNegativeInteger',N))),
	owl2_export_axiom(OPE,main_triple(Tope,_,_)),owl_rdf_assert(BNode,'owl:onProperty',Tope),
	owl2_export_axiom(CEorDR,main_triple(Tce,_,_)),
	(   classExpression(CEorDR) -> owl_rdf_assert(BNode,'owl:onClass',Tce); owl_rdf_assert(BNode,'owl:onDataRange',Tce)),!.

% 
% SWRL
% (this may eventually end up in a separate hooks file?)

owl2_export_axiom(implies(AL,CL),main_triple(RuleNode,'rdf:type','swrl:Imp')) :-
        % TODO: allow rules to have IDs in swrl.pl
        %   for now we force it to be a bnode
        !,
        %rdf_bnode(RuleNode),
        as2rdf_bnode(implies(AL,CL),RuleNode),
	owl_rdf_assert(RuleNode,'rdf:type','swrl:Imp'),
	swrl_export_atom_list(AL,ALNode),
	swrl_export_atom_list(CL,CLNode),
        owl_rdf_assert(RuleNode,'swrl:body', ALNode),
        owl_rdf_assert(RuleNode,'swrl:head', CLNode).


:- multifile translate_iri_hook/2.
owl2_export_axiom(IRI,main_triple(T,_,_)) :- translate_iri_hook(IRI,T), !. 

owl2_export_axiom(IRI,main_triple(IRI,_,_)) :- atom(IRI),!. % better iri(IRI).
owl2_export_axiom(X,main_triple(X,_,_)) :- X=literal(_),!. % better iri(IRI).
owl2_export_axiom(Any,main_triple(Any,_,_)) :- format(user_error,'unresolved: ~w~n',[Any]).

translate_iri(X,Y) :- translate_iri_hook(X,Y),!.
translate_iri(X,X).



/*
owl2_export_annotation(Parent)
 Parent can be an axiom or an annotation itself (nested annotations)
*/    


owl2_export_annotation(annotation(_,_,_),_,S,_,_) :-
        \+ atom(S), % do nothing with this for now - see issue #8
        !.
owl2_export_annotation(Parent,ParentType,S,ParentP,ParentO) :-
	(   Parent = annotation(_,ParentAP,ParentAV)
        ->  P = ParentAP,
            O = ParentAV,
	    owl_rdf_assert(S,P,O)
        ;   P = ParentP, O = ParentO),
	findall(AP-AV,annotation(Parent,AP,AV),ANNs),
	(   ANNs = [_|_]
        ->  as2rdf_bnode(Parent,BNode),
	    reify(BNode,'rdf:type',ParentType,S,P,O),
	    forall(member(ANN,ANNs),owl2_export_annotation(ANN,'owl:Annotation',BNode,_,_))
        ;   true).

reify(SNode,PTerm,OTerm,S,P,O) :-
	owl_rdf_assert(SNode,PTerm,OTerm),
	owl_rdf_assert(SNode,'owl:annotatedSource',S),
	owl_rdf_assert(SNode,'owl:annotatedProperty',P),
	owl_rdf_assert(SNode,'owl:annotatedTarget',O).



/*
owl2_export_list(+List, -Node).  
        Generates RDF triples for the List of construct based on
	Abstract Syntax list transformation rules. Node represents the 
	List in the resulting RDF graph
*/   

owl2_export_list([],'rdf:nil').

owl2_export_list([S|Rest],Node) :-
	as2rdf_bnode([S|Rest],Node),
	owl2_export_axiom(S,main_triple(Ts,_,_)),
        % this is to circumvent a bug in list writing:
        (   S=literal(_)
        ->  true
        ;   owl_rdf_assert(Node,'rdf:type', 'rdf:List')),	
	owl_rdf_assert(Node,'rdf:first', Ts),	
	owl2_export_list(Rest,Node2),
	owl_rdf_assert(Node,'rdf:rest',Node2).

swrl_export_atom_list([],'rdf:nil').

swrl_export_atom_list([S|Rest],Node) :-
	as2rdf_bnode([S|Rest],Node),
	swrl_export_atom(S,main_triple(Ts,_,_)),
	%owl_rdf_assert(Node,'rdf:type', 'swrl:AtomList'),	
	owl_rdf_assert(Node,'rdf:type', 'rdf:List'),	
	owl_rdf_assert(Node,'rdf:first', Ts),	
	swrl_export_atom_list(Rest,Node2),
	owl_rdf_assert(Node,'rdf:rest',Node2).

% in swrl.pl atom lists are weakly typed - we allow a swrl atom to
% stand in for an atom list for convenience
swrl_export_atom_list(X,Node) :-
        \+ atom(X),
        swrl_export_atom_list([X],Node).

swrl_export_argument_list([],'rdf:nil').

swrl_export_argument_list([S|Rest],Node) :-
	as2rdf_bnode([S|Rest],Node),
	swrl_export_argument(S,main_triple(Ts,_,_)),
	owl_rdf_assert(Node,'rdf:type', 'swrl:ArgumentList'),	
	owl_rdf_assert(Node,'rdf:first', Ts),	
	swrl_export_argument_list(Rest,Node2),
	owl_rdf_assert(Node,'rdf:rest',Node2).


swrl_export_atom(propertyAssertion(OPE,A1,A2),main_triple(BNode,'rdf:type','swrl:DatavaluedPropertyAtom')) :-
        dataProperty(OPE),
        !,
        rdf_bnode(BNode),
	owl_rdf_assert(BNode,'rdf:type','swrl:DatavaluedPropertyAtom'),
	owl2_export_axiom(OPE,main_triple(Tope,_,_)),owl_rdf_assert(BNode,'swrl:propertyPredicate',Tope),
        swrl_export_argument(A1,main_triple(TA1,_,_)),owl_rdf_assert(BNode,'swrl:argument1',TA1),
        swrl_export_argument(A2,main_triple(TA2,_,_)),owl_rdf_assert(BNode,'swrl:argument2',TA2).

swrl_export_atom(propertyAssertion(OPE,A1,A2),main_triple(BNode,'rdf:type','swrl:IndividualPropertyAtom')) :-
        !,
        rdf_bnode(BNode),
        %as2rdf_bnode(propertyAssertion(OPE,A1,A2),BNode),
	owl_rdf_assert(BNode,'rdf:type','swrl:IndividualPropertyAtom'),
	owl2_export_axiom(OPE,main_triple(Tope,_,_)),owl_rdf_assert(BNode,'swrl:propertyPredicate',Tope),
        swrl_export_argument(A1,main_triple(TA1,_,_)),owl_rdf_assert(BNode,'swrl:argument1',TA1),
        swrl_export_argument(A2,main_triple(TA2,_,_)),owl_rdf_assert(BNode,'swrl:argument2',TA2).

swrl_export_atom(builtin(OPE,Args),main_triple(BNode,'rdf:type','swrl:BuiltinAtom')) :-
        !,
        rdf_bnode(BNode),
	owl_rdf_assert(BNode,'rdf:type','swrl:BuiltinAtom'),
	%owl2_export_axiom(OPE,main_triple(Tope,_,_)),owl_rdf_assert(BNode,'swrl:builtin',Tope),
	atom_concat('swrlb:',OPE,Builtin),owl_rdf_assert(BNode,'swrl:builtin',Builtin),
        swrl_export_argument_list(Args,ListNode),
        owl_rdf_assert(BNode,'swrl:arguments',ListNode).

swrl_export_atom(description(C,A1),main_triple(BNode,'rdf:type','swrl:ClassAtom')) :-
        !,
        rdf_bnode(BNode),
        %as2rdf_bnode(description(C,A1),BNode),
	owl_rdf_assert(BNode,'rdf:type','swrl:ClassAtom'),
	owl2_export_axiom(C,main_triple(TC,_,_)),owl_rdf_assert(BNode,'swrl:classPredicate',TC),
        swrl_export_argument(A1,main_triple(TA1,_,_)),owl_rdf_assert(BNode,'swrl:argument1',TA1).

swrl_export_atom(differentFrom(A1,A2),main_triple(BNode,'rdf:type','swrl:DifferentIndividualsAtom')) :-
        !,
        rdf_bnode(BNode),
	%as2rdf_bnode(differentFrom(A1,A2),BNode),
	owl_rdf_assert(BNode,'rdf:type','swrl:DifferentIndividualsAtom'),
        swrl_export_argument(A1,main_triple(TA1,_,_)),owl_rdf_assert(BNode,'swrl:argument1',TA1),
        swrl_export_argument(A2,main_triple(TA2,_,_)),owl_rdf_assert(BNode,'swrl:argument2',TA2).

swrl_export_atom(sameAs(A1,A2),main_triple(BNode,'rdf:type','swrl:SameIndividualAtom')) :-
        !,
        rdf_bnode(BNode),
	%as2rdf_bnode(sameAs(A1,A2),BNode),
        owl_rdf_assert(BNode,'rdf:type','swrl:SameIndividualAtom'),
        swrl_export_argument(A1,main_triple(TA1,_,_)),owl_rdf_assert(BNode,'swrl:argument1',TA1),
        swrl_export_argument(A2,main_triple(TA2,_,_)),owl_rdf_assert(BNode,'swrl:argument2',TA2).

% allow convenient non-canonical form
swrl_export_atom(CA,T) :-
        CA=..[C,A],
        !,
        swrl_export_atom(class(C,A),T).
swrl_export_atom(PA,T) :-
        PA=..[P,A1,A2],
        !,
        swrl_export_atom(propertyAssertion(P,A1,A2),T).


swrl_export_argument(v(V),main_triple(V1,'rdf:type','swrl:Variable')) :-
        !,
        (   number(V)
        ->  atom_concat('#v',V,V1)
        ;   V1=V),
        owl_rdf_assert(V1,'rdf:type','swrl:Variable'),!.

swrl_export_argument(d(V),T) :- swrl_export_argument(v(V),T).
swrl_export_argument(i(V),T) :- swrl_export_argument(v(V),T).
swrl_export_argument(V,T) :- owl2_export_axiom(V,T).


/*
owl_rdf_assert(S,P,O).  
        Expands the NS the S, P, O terms and asserts into the RDF
	database
*/   
owl_rdf_assert(S,P,O) :-
	expand_ns(S,S1),expand_ns(P,P1),expand_ns(O,O1),
        debug(rdf_assert,'assert: ~w ~w ~w.',[S,P,O]),
	rdf_db:rdf_assert(S1,P1,O1), !.

/*
as2rdf_bnode(+X,-Node).  
        It generates a bnode Node for construct X in case it does not
	exist already as a blanknode/3 clause.
*/
% NO STRUCTURE SHARING IN OWL2
%as2rdf_bnode(X,Node) :-
%	blanknode_gen(Node,X),
%        debug(bnode,'bnode REUSE ~w ==> ~w',[X,Node]),
%        !.

as2rdf_bnode(X,Node) :-
	rdf_db:rdf_bnode(Node),
	assert(blanknode_gen(Node,X)),
        debug(bnode,'bnode NEW ~w ==> ~w',[X,Node]),
        !.

