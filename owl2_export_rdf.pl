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
	  [ 	    	    
	    owl_generate_rdf/2, % FileName, RDF_Load_Mode (complete/not)
	    owl_generate_rdf/3, % Ontology,FileName, RDF_Load_Mode (complete/not)
	    owl_rdf2n3/0 		    
	  ]).

:- use_module(owl2_model).
:- use_module(owl2_from_rdf).
:- use_module(library('semweb/rdf_db')).

:- multifile owl2_io:save_axioms_hook/3.
owl2_io:save_axioms_hook(File,owl,Opts) :-
        (   member(rdf_load_load(RDF_Load_Mode),Opts)
        ->  true
        ;   true),
        owl_generate_rdf(File,RDF_Load_Mode).

owl2_from_rdf:owl_repository('http://www.w3.org/TR/2003/PR-owl-guide-20031209/food','testfiles/food.owl').
t:- owl_parse('testfiles/wine.owl',complete,complete,true),
	owl_generate_rdf('http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine','testfiles/wine_g.owl',complete).

t1 :-
	owl_generate_rdf('http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine','testfiles/wine_g.owl',complete).

owl_generate_rdf(FileName,RDF_Load_Mode) :-
        ontology(Ontology),
        owl_generate_rdf(Ontology,FileName,RDF_Load_Mode).

owl_generate_rdf(Ontology,FileName,RDF_Load_Mode) :- 
	(   RDF_Load_Mode=complete -> rdf_retractall(_,_,_); true),
	retractall(blanknode_gen(_,_)),retractall(blanknode(_,_,_)),
	owl2_export_axiom(ontology(Ontology),_),
	forall(ontologyAxiom(Ontology,Axiom),
	       (owl2_export_axiom(Axiom,main_triple(S,P,O)),
		owl2_export_annotation(Axiom,'owl:Axiom',S,P,O))),
	%as2rdf_class,
	%as2rdf_subclass,
	%as2rdf_disjointSet,
	%as2rdf_equivalentSet,
	%as2rdf_property,
	%as2rdf_individual,
	%as2rdf_ontology,
	%as2rdf_sameIndividuals,
	%as2rdf_differentIndividuals,
	rdf_db:rdf_save(FileName).
	


/*
owl_rdf2n3
     Prints out the RDF triples in N3 notation.   
*/
owl_rdf2n3 :-	
    rdf_db:rdf(S,P,O),
    collapse_ns(S,S1,':',[]),collapse_ns(P,P1,':',[]),collapse_ns(O,O1,':',[]),
    write(S1), write(' '), write(P1), write(' '), write(O1), write(' .'),nl,
    fail.

/*
owl2_export_axiom(X,main_triple(Node,_,_)) :-
	blanknode_gen(Node,X),!.
*/

owl2_export_axiom(ontology(O),main_triple(O,'rdf:type','owl:Ontology')) :-
	owl_rdf_assert(O,'rdf:type','owl:Ontology'),
	forall(ontologyImport(O,Import),owl_rdf_assert(O,'owl:imports',Import)),!.


owl2_export_axiom(class(C),main_triple(C,'rdf:type','owl:Class')) :-
	owl_rdf_assert(C,'rdf:type','owl:Class'),!.

owl2_export_axiom(subClassOf(C1,C2),main_triple(TC1,'rdfs:subClassOff',TC2)) :-
	owl2_export_axiom(C1,main_triple(TC1,_,_)),
	(   C1 = 'http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#WhiteLoire' -> 
	true 
	;   
	true),
	owl2_export_axiom(C2,main_triple(TC2,_,_)),
	owl_rdf_assert(TC1,'rdfs:subClassOf',TC2),!.

owl2_export_axiom(equivalentClasses([X,Y]),main_triple(Tx,'owl:equivalentClass',Ty)) :-
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

owl2_export_axiom(disjointUnion([C|Rest]),main_triple(Tc,'owl:disjointUnionOf',LNode)) :-
	owl2_export_axiom(C,main_triple(Tc,_,_)),
	owl2_export_list(Rest,LNode),
	owl_rdf_assert(Tc,'owl:disjointUnionOf',LNode),!.

owl2_export_axiom(subPropertyOf(P1,P2),main_triple(Tp1,'owl:subPropertyOf',Tp2)) :-
	owl2_export_axiom(P1,main_triple(Tp1,_,_)),
	owl2_export_axiom(P2,main_triple(Tp2,_,_)),
	owl_rdf_assert(Tp1,'owl:subPropertyOf',Tp2),!.

% TODO: subPropertyOf(propertyChain(Chain),P2)
% 

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
	owl_rdf_assert(Ta,'rdf:type',Tce),!.

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
	owl2_export_axiom(As,main_triple(TAs,_,_)),
	owl2_export_axiom(Av,main_triple(TAv,_,_)),
	owl_rdf_assert(TAs,AP,TAv),!.

%
% Class Expressions (Descriptions)
%

owl2_export_axiom(intersectionOf([E|Rest]),main_triple(BNode,'rdf:type',Type)) :-
	as2rdf_bnode(intersectionOf([E|Rest]),BNode),
	owl2_export_list([E|Rest],LNode),
	(   classExpression(E) -> Type = 'owl:Class'; Type = 'owl:Datatype'),
	owl_rdf_assert(BNode,'owl:intersectionOf', LNode),!.

owl2_export_axiom(unionOf([E|Rest]),main_triple(BNode,'rdf:type',Type)) :-
	as2rdf_bnode(unionOf([E|Rest]),BNode),
	owl2_export_list([E|Rest],LNode),
	(   classExpression(E) -> Type = 'owl:Class'; Type = 'owl:Datatype'),
	owl_rdf_assert(BNode,'owl:unionOf', LNode),!.	  

owl2_export_axiom(oneOf([E|Rest]),main_triple(BNode,'rdf:type',Type)) :-
	as2rdf_bnode(oneOf([E|Rest]),BNode),
	owl2_export_list([E|Rest],LNode),
	(   classExpression(E) -> Type = 'owl:Class'; Type = 'owl:Datatype'),
	owl_rdf_assert(BNode,'owl:oneOf', LNode),!.	  


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


owl2_export_axiom(IRI,main_triple(IRI,_,_)) :- atom(IRI),!. % better iri(IRI).



owl2_export_axiom(Any,main_triple(Any,_,_)) :- print('unresolved:'-Any),nl.

/*
owl2_export_annotation(Parent)
 Parent can be an axiom or an annotation itself (nested annotations)
*/    


owl2_export_annotation(Parent,ParentType,S,ParentP,ParentO) :-
	(   Parent = annotation(_,ParentAP,ParentAV) -> P = ParentAP, O = ParentAV,
	    owl_rdf_assert(S,P,O); 
	P = ParentP, O = ParentO),
	findall(AP-AV,annotation(Parent,AP,AV),ANNs),
	(   ANNs = [_|_] -> as2rdf_bnode(Parent,BNode),
	    reify(BNode,'rdf:type',ParentType,S,P,O),
	    forall(member(ANN,ANNs),owl2_export_annotation(ANN,'owl:Annotation',BNode,_,_)); 
	true).

reify(SNode,PTerm,OTerm,S,P,O) :-
	owl_rdf_assert(SNode,PTerm,OTerm),
	owl_rdf_assert(SNode,'owl:subject',S),
	owl_rdf_assert(SNode,'owl:predicate',P),
	owl_rdf_assert(SNode,'owl:object',O).




/*
as2rdf_class_2(+Class, +CP, +DescriptionList).
	Generates RDF triples for Class, depending on the contents 
	of the DescriptionList and the Complete/Partial (CP) indicator
*/    

as2rdf_class_2(C,complete,[unionOf(DL)]) :- !,
	as2rdf_list(DL,Node), 
	owl_rdf_assert(C,'owl:unionOf',Node).

as2rdf_class_2(C,complete,[complementOf(D)]) :- !,
	as2rdf(D,Node), 
	owl_rdf_assert(C,'owl:complementOf',Node).

as2rdf_class_2(C,complete,[intersectionOf(DL)]) :- !,
	as2rdf_list(DL,Node), 
	owl_rdf_assert(C,'owl:intersectionOf',Node).

as2rdf_class_2(C,complete,DL) :- !,
	as2rdf_list(DL,Node),
	owl_rdf_assert(C,'owl:intersectionOf',Node).	

as2rdf_class_2(C,complete,[Description]) :- !,
	as2rdf(Description,Node), 
	owl_rdf_assert(C,'owl:equivalentClass',Node).


as2rdf_class_2(C,partial,DL) :- !,
	as2rdf_triple_list(C,'rdfs:subClassOf',DL).


as2rdf_deprecated_class(C,Deprecated) :-
	( Deprecated = true, !, owl_rdf_assert(C, 'rdf:type','owl:DeprecatedClass'); true).


/*
------------------------------------------------------
as2rdf_subclass. 
	Generates RDF triples for SsubClass constructs.
*/    

as2rdf_subclass :-
	subclassOf(X,Y),
	as2rdf(X,NodeX),as2rdf(Y,NodeY),
	owl_rdf_assert(NodeX,'rdfs:subClassOf',NodeY), 
	fail.

as2rdf_subclass.


/*
------------------------------------------------------
as2rdf_equivalentSet. 
	Generates RDF triples for equivalentSet constructs.
*/    

as2rdf_equivalentSet :-
	equivalentSet(Set),
	as2rdf_equivalentSet_2(Set),
	fail.

as2rdf_equivalentSet.


/*
as2rdf_equivalentSet_2. 
	Generates RDF triples for equivalentClass and equivalentProperty constructs.
*/    
as2rdf_equivalentSet_2([H|T]):-
	(   property(H,_,_,_,_,_,_),!, 
	    as2rdf_set2pairs([H|T],'owl:equivalentProperty')
	    ;  
	    as2rdf_set2pairs([H|T],'owl:equivalentClass')
	).

/*
as2rdf_disjointSet. 
	Generates RDF triples for disjointSet constructs.
*/    

as2rdf_disjointSet :-
	disjointSet(Set),
	as2rdf_set2pairs(Set,'owl:disjointWith'),
	fail.


as2rdf_disjointSet.


/*
------------------------------------------------------
as2rdf_differentIndividuals  
	Generates RDF triples for differentIndividuals constructs.
*/    
as2rdf_differentIndividuals :- 
	differentIndividuals(Set),
	as2rdf_bnode(allDifferent(Set),Node),
	owl_rdf_assert(Node, 'rdf:type', 'owl:AllDifferent'),
        as2rdf_list(Set,Node1),
	owl_rdf_assert(Node,'owl:distinctMembers',Node1),	
	fail.

as2rdf_differentIndividuals.


/*
as2rdf_sameIndividuals  
	Generates RDF triples for sameIndividuals constructs.
*/    
as2rdf_sameIndividuals :- 
	sameIndividuals(Set),
	as2rdf_set2pairs(Set,'owl:sameAs'),	
	fail.

as2rdf_sameIndividuals.


/*
------------------------------------------------------
as2rdf_set2pairs(+Set,+Predicate).
        Given a list (Set) it generates (X Predicate Y) rdf triples
	for all X, Y elements in the list
*/    

as2rdf_set2pairs([],_) :- !.

as2rdf_set2pairs([A|Rest],Predicate) :- !,
	as2rdf_set2pairs(A,Rest,Predicate),
	as2rdf_set2pairs(Rest,Predicate).

/*
as2rdf_set2pairs(+X, +Set,+Predicate).
        It generates (X Predicate Y) rdf triples for all Y elements in
	the list Set. X and Y are converted to RDF Nodes first.
*/    
as2rdf_set2pairs(_,[],_) :- !.
	
as2rdf_set2pairs(A,[B|Rest],Predicate) :- 
	as2rdf(A,Node1),
	as2rdf(B,Node2),
	owl_rdf_assert(Node1,Predicate,Node2),
        as2rdf_set2pairs(A,Rest,Predicate).
	

/*
------------------------------------------------------------------------------------------
as2rdf_property. 
	Generates RDF triples for Property constructs.
*/    
as2rdf_property :- 
	property(PID,Deprecated,AnnotationList,PID_SuperList,
			      PTList,PID_DomainList,PID_RangeList),
	as2rdf_property_2(PID,Deprecated,PTList),
	as2rdf_triple_list(PID,_,AnnotationList),
	as2rdf_triple_list(PID,'rdfs:subPropertyOf',PID_SuperList),
	as2rdf_triple_list(PID,'rdfs:domain',PID_DomainList),
	as2rdf_triple_list(PID,'rdfs:range',PID_RangeList),
	fail.
	
as2rdf_property.

/*
as2rdf_property_2(+PId, +Deprecated, +PropertyTypeSet) 
	Generates RDF triples for property PID based on the type of property
	as defined in the PropertTypeSet options list
*/    
as2rdf_property_2(PID,Deprecated,[OT,F,IF,T,S,iof(Inv)]) :-
	(   Deprecated = true, !, owl_rdf_assert(PID, 'rdf:type','owl:DeprecatedProperty')
	;
	true),
	( nonvar(S),
	  owl_rdf_assert(PID,'rdf:type','owl:SymmetricProperty'),!,Defined = 'yes' ; true),	
	( nonvar(T),owl_rdf_assert(PID,'rdf:type','owl:TransitiveProperty'),
	  !,Defined = 'yes' ; true),
	( nonvar(IF),owl_rdf_assert(PID,'rdf:type','owl:InverseFunctionalProperty'),
	  !,Defined = 'yes' ; true),
	( var(Defined), !,
	  (   OT = objectProperty,owl_rdf_assert(PID,'rdf:type','owl:ObjectProperty'),!;
	      OT = datatypeProperty,owl_rdf_assert(PID,'rdf:type','owl:DatatypeProperty'),!)
	; true),
	(   nonvar(F), owl_rdf_assert(PID,'rdf:type','owl:FunctionalProperty'),!; true),
	(   nonvar(Inv), owl_rdf_assert(PID,'owl:inverseOf',Inv),!; true).

/*
------------------------------------------------------------------------------------------
as2rdf_individual. 
	Generates RDF triples for individual constructs.
*/    
as2rdf_individual :-
	individual(I),
        findall(type(C),classAssertion(C,I),Cs),
	as2rdf_triple_list(I,'rdf:type',Cs),
        findall(value(P,I2),propertyAssertion(P,I,I2),PAs),
	as2rdf_triple_list(I,_,PAs),
        findall(value(P,I2),annotationAssertion(P,I,I2),AAs),
	as2rdf_triple_list(I,_,AAs),
	fail.

as2rdf_individual.


/*
------------------------------------------------------------------------------------------
as2rdf_ontology. 
	Generates RDF triples for ontology constructs.
*/           
as2rdf_ontology :-
	ontology(O,OAL),
	owl_rdf_assert(O, 'rdf:type','owl:Ontology'),
	as2rdf_triple_list(O,_,OAL),
	fail.

as2rdf_ontology.


/*
-------------------------------------------------------
as2rdf_triple_list(+ID,+Predicate,+List). 
	Generates RDF triples of the form (ID, Predicate, Y) where Y is 
	each element in List. Nodes are generated for each element Y
	and specific cases for elements Y are handled. Eg Y=type(T) or 
	Y = value(P,V) or Y = annotation(P,V).
*/           

as2rdf_triple_list(_,_,[]).

as2rdf_triple_list(ID,Predicate,[type(T)|Rest]) :- !,
	as2rdf(T,Node),
	owl_rdf_assert(ID,'rdf:type',Node),
	as2rdf_triple_list(ID,Predicate, Rest).

as2rdf_triple_list(ID,Predicate,[value(P,V)|Rest]) :- !,
	as2rdf(P,PNode),
	as2rdf(V,VNode),
	owl_rdf_assert(ID,PNode,VNode),
	as2rdf_triple_list(ID,Predicate, Rest).


% Note: there is no distinction here between OntologyProperty and
% AnnotationProperty. Do we need this?
as2rdf_triple_list(ID,_,[annotation(P,V)|Rest]) :- !,
	owl_rdf_assert(P,'rdf:type','owl:AnnotationProperty'),
	as2rdf(V,VNode),
	owl_rdf_assert(ID,P,VNode), !,
	as2rdf_triple_list(ID,_,Rest).


as2rdf_triple_list(ID,Predicate,[D|Rest]) :-
	as2rdf(D,Node),
	owl_rdf_assert(ID,Predicate,Node),!,
	as2rdf_triple_list(ID,Predicate,Rest).


/*
as2rdf(+Construct, -Node).  
        Generates RDF triples for the Construct based on AS
	transformation rules. If not existing a blankNode is also 
	generated to represent the construct.
        A Construct can be any Description (incl Restrictions), URL, 
	blanknode or literal.
*/   

as2rdf(X,Node) :-
	blanknode_gen(Node,X),!.


as2rdf(unionOf(DL),Node) :- !,
	as2rdf_bnode(unionOf(DL),Node),
	owl_rdf_assert(Node,'rdf:type','owl:Class'),
	as2rdf_list(DL,DLNode),
	owl_rdf_assert(Node,'owl:unionOf',DLNode).
	
as2rdf(intersectionOf(DL),Node) :- !,
	as2rdf_bnode(intersectionOf(DL),Node),
	owl_rdf_assert(Node,'rdf:type','owl:Class'),
	as2rdf_list(DL,Node1),
	owl_rdf_assert(Node,'owl:intersectionOf',Node1).

as2rdf(complementOf(D),Node) :- !,
	as2rdf_bnode(complementOf(D),Node),
	owl_rdf_assert(Node,'rdf:type','owl:Class'),
	as2rdf(D,Node1),
	owl_rdf_assert(Node,'owl:complementOf',Node1).

as2rdf(oneOf(DL),Node) :- !,
	as2rdf_bnode(oneOf(DL),Node),
	owl_rdf_assert(Node,'rdf:type','owl:Class'),
	as2rdf_list(DL,Node1),
	owl_rdf_assert(Node,'owl:oneOf',Node1).

as2rdf(restriction(PropertyID,allValuesFrom(Descr)),Node) :-  !,
	as2rdf_bnode(restriction(PropertyID,allValuesFrom(Descr)),Node),
	owl_rdf_assert(Node,'rdf:type','owl:Restriction'),
	owl_rdf_assert(Node,'owl:onProperty',PropertyID),
	as2rdf(Descr,Node1),
	owl_rdf_assert(Node,'owl:allValuesFrom',Node1).

as2rdf(restriction(PropertyID,someValuesFrom(Descr)),Node) :- !,
	as2rdf_bnode(restriction(PropertyID,someValuesFrom(Descr)),Node),
	owl_rdf_assert(Node,'rdf:type','owl:Restriction'),
	owl_rdf_assert(Node,'owl:onProperty',PropertyID),
	as2rdf(Descr,Node1),
	owl_rdf_assert(Node,'owl:someValuesFrom',Node1).

as2rdf(restriction(PropertyID,value(Value)),Node) :- !,
	as2rdf_bnode(restriction(PropertyID,value(Value)),Node),
	owl_rdf_assert(Node,'rdf:type','owl:Restriction'),
	owl_rdf_assert(Node,'owl:onProperty',PropertyID),
	as2rdf(Value,Node1),
	owl_rdf_assert(Node,'owl:hasValue',Node1).

as2rdf(restriction(PropertyID,minCardinality(Min)),Node) :- !,
	as2rdf_bnode(restriction(PropertyID,minCardinality(Min)),Node),
	owl_rdf_assert(Node,'rdf:type','owl:Restriction'),
	owl_rdf_assert(Node,'owl:onProperty',PropertyID),
	owl_rdf_assert(Node,'owl:minCardinality',Min).

as2rdf(restriction(PropertyID,maxCardinality(Max)),Node) :- !,
	as2rdf_bnode(restriction(PropertyID,maxCardinality(Max)),Node),
	owl_rdf_assert(Node,'rdf:type','owl:Restriction'),
	owl_rdf_assert(Node,'owl:onProperty',PropertyID),
	owl_rdf_assert(Node,'owl:maxCardinality',Max).

as2rdf(restriction(PropertyID,cardinality(Card)),Node) :- !,
	as2rdf_bnode(restriction(PropertyID,cardinality(Card)),Node),
	owl_rdf_assert(Node,'rdf:type','owl:Restriction'),
	owl_rdf_assert(Node,'owl:onProperty',PropertyID),
	owl_rdf_assert(Node,'owl:cardinality',Card).

as2rdf('rdfs:Literal','rdfs:Literal') :-!.

as2rdf(literal(X),literal(X)):-!.

as2rdf(ID, ID) :- !.


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
	owl_rdf_assert(Node,'rdf:first', Ts),	
	owl2_export_list(Rest,Node2),
	owl_rdf_assert(Node,'rdf:rest',Node2).

/*
owl_rdf_assert(S,P,O).  
        Expands the NS the S, P, O terms and asserts into the RDF
	database
*/   
owl_rdf_assert(S,P,O) :- 
	expand_ns(S,S1),expand_ns(P,P1),expand_ns(O,O1),
	rdf_db:rdf_assert(S1,P1,O1), !.
/*
as2rdf_bnode(+X,-Node).  
        It generates a bnode Node for construct X in case it does not
	exist already as a blanknode/3 clause.
*/   
as2rdf_bnode(X,Node) :-
	blanknode(X,Node,_),
	blanknode_gen(Node,X),!.

as2rdf_bnode(X,Node) :-
	rdf_db:rdf_bnode(Node),
	assert(blanknode_gen(Node,X)),!.
