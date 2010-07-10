/* -*- Mode: Prolog -*- */

:- module(owl2_model,
	  [
           entity/1,
           declarationAxiom/1,
           class/1,
           datatype/1,
           property/1,
           objectProperty/1,
           dataProperty/1,
           annotationProperty/1,
           individual/1,
           namedIndividual/1,
           anonymousIndividual/1,
           construct/1,
           axiom/1,
           classAxiom/1,
           subClassOf/2,
           equivalentClasses/1,
           disjointClasses/1,
           disjointUnion/2,
           propertyAxiom/1,
           subPropertyOf/2,
           subObjectPropertyOf/2,
           subDataPropertyOf/2,
           subAnnotationPropertyOf/2,
           equivalentProperties/1,
           equivalentObjectProperties/1,
           equivalentDataProperties/1,
           disjointProperties/1,
           disjointObjectProperties/1,
           disjointDataProperties/1,
           inverseProperties/2,
           propertyDomain/2,
           objectPropertyDomain/2,
           dataPropertyDomain/2,
           annotationPropertyDomain/2,
           propertyRange/2,
           objectPropertyRange/2,
           dataPropertyRange/2,
           annotationPropertyRange/2,
           functionalProperty/1,
           functionalObjectProperty/1,
           functionalDataProperty/1,
           inverseFunctionalProperty/1,
           reflexiveProperty/1,
           irreflexiveProperty/1,
           symmetricProperty/1,
           asymmetricProperty/1,
           transitiveProperty/1,
           hasKey/2,
           fact/1,
           sameIndividual/1,
           differentIndividuals/1,
           classAssertion/2,
           propertyAssertion/3,
           objectPropertyAssertion/3,
           dataPropertyAssertion/3,
           negativePropertyAssertion/3,
           negativeObjectPropertyAssertion/3,
           negativeDataPropertyAssertion/3,
           annotationAssertion/3,
           annotation/1,
           annotation/3,
           ontologyAnnotation/3,
           axiomAnnotation/3,
           annotationAnnotation/3,
           ontology/1,
           ontologyDirective/2,
           ontologyAxiom/2,
           ontologyImport/2,
           ontologyVersionInfo/2,

           axiom_arguments/2,

           classExpression/1,
           objectIntersectionOf/1, objectUnionOf/1, objectComplementOf/1, objectOneOf/1,
           objectSomeValuesFrom/1, objectAllValuesFrom/1, objectHasValue/1, objectHasSelf/1,
           objectMinCardinality/1, objectMaxCardinality/1, objectExactCardinality/1,
           dataSomeValuesFrom/1, dataAllValuesFrom/1, dataHasValue/1,
           dataMinCardinality/1, dataMaxCardinality/1, dataExactCardinality/1,

           dataRange/1,
           dataIntersectionOf/1,
           dataUnionOf/1,
           dataComplementOf/1,
           dataOneOf/1,
           datatypeRestriction/1,

           axiompred/1,

           anyPropertyAssertion/3,
           equivalent_to/2,
           disjoint_with/2,
           labelAnnotation_value/2,

           axiom_directly_about/2,
           axiom_directly_references/2,
           axiom_about/2,
           axiom_references/2,

           assert_axiom/1,
           assert_axiom/2,
           retract_axiom/1,
           retract_all_axioms/0,
	   owl2_model_init/0,
           consult_axioms/1

	  ]).
%:- require([ is_list/1
%	   , current_prolog_flag/2
%	   , forall/2
%	   , debug/3
%	   ]).


:- use_module(library(lists),[member/2]).

%% axiompred(?PredSpec)
% @param PredSpec Predicate/Arity
% (metamodeling) true if PredSpec is a predicate that defines an axiom
:- multifile axiompred/1.

:- discontiguous(valid_axiom/1).
:- discontiguous(axiompred/1).
:- discontiguous(axiom_arguments/2).


% TODO: hasKey

/****************************************
  AXIOMS
  ****************************************/

%% entity(?IRI)
% the fundamental building blocks of owl 2 ontologies, and they define the vocabulary (the named terms) of an ontology
%
% @see individual/1, property/1, class/1, datatype/1
entity(A) :- individual(A).
entity(A) :- property(A).
entity(A) :- class(A).
entity(A) :- datatype(A).
axiom_arguments(entity,[iri]).
valid_axiom(entity(A)) :- subsumed_by([A],[iri]).

%declarationAxiom(individual(A)) :- individual(A). % TODO - check this
declarationAxiom(namedIndividual(A)) :- namedIndividual(A).
declarationAxiom(objectProperty(A)) :- objectProperty(A).
declarationAxiom(dataProperty(A)) :- dataProperty(A).
declarationAxiom(annotationProperty(A)) :- annotationProperty(A).  % VV added 9/3/2010
declarationAxiom(class(A)) :- class(A).
declarationAxiom(datatype(A)) :- datatype(A).
% TODO: check. here we treat the ontology declaration as an axiom;
% this liberal definition of axiom allows us to iterate over axiom/1
% to find every piece of information in the ontology.
declarationAxiom(ontology(A)) :- ontology(A).

%% class(?IRI)
% Classes can be understood as sets of individuals
:- dynamic(class/1).
:- multifile(class/1).
axiompred(class/1).
axiom_arguments(class,[iri]).
valid_axiom(class(A)) :- subsumed_by([A],[iri]).

%% datatype(?IRI)
% Datatypes are entities that refer to sets of values described by a datatype map
:- dynamic(datatype/1).
:- multifile(datatype/1).
axiompred(datatype/1).
axiom_arguments(datatype,[iri]).
valid_axiom(datatype(A)) :- subsumed_by([A],[iri]).

%% property(?IRI)
% Properties connect individuals with either other individuals or with literals
%
% @see dataProperty/1, objectProperty/1, annotationProperty/1
property(A) :- dataProperty(A).
property(A) :- objectProperty(A).
property(A) :- annotationProperty(A).
axiom_arguments(property,[iri]).
valid_axiom(property(A)) :- subsumed_by([A],[iri]).

%% objectProperty(?IRI)
% Object properties connect pairs of individuals
:- dynamic(objectProperty/1).
:- multifile(objectProperty/1).
axiompred(objectProperty/1).
axiom_arguments(objectProperty,[iri]).
valid_axiom(objectProperty(A)) :- subsumed_by([A],[iri]).

%% dataProperty(?IRI)
% Data properties connect individuals with literals. In some knowledge representation systems, functional data properties are called attributes.
:- dynamic(dataProperty/1).
:- multifile(dataProperty/1).
axiompred(dataProperty/1).
axiom_arguments(dataProperty,[iri]).
valid_axiom(dataProperty(A)) :- subsumed_by([A],[iri]).

%% annotationProperty(?IRI)
% Annotation properties can be used to provide an annotation for an ontology, axiom, or an IRI
:- dynamic(annotationProperty/1).
:- multifile(annotationProperty/1).
axiompred(annotationProperty/1).
axiom_arguments(annotationProperty,[iri]).
valid_axiom(annotationProperty(A)) :- subsumed_by([A],[iri]).


%% individual(?IRI)
% Individuals represent actual objects from the domain being modeled
% @see anonymousIndividual/1, namedIndividual/1
individual(A) :- anonymousIndividual(A).
individual(A) :- namedIndividual(A).
individual(A) :- nonvar(A),iri(A),\+property(A),\+class(A),\+ontology(A). % TODO: check: make individuals the default
axiom_arguments(individual,[iri]).
valid_axiom(individual(A)) :- subsumed_by([A],[iri]).

%% namedIndividual(?IRI)
% Named individuals are given an explicit name that can be used in any ontology in the import closure to refer to the same individual
:- dynamic(namedIndividual/1).
:- multifile(namedIndividual/1).
axiompred(namedIndividual/1).
axiom_arguments(namedIndividual,[iri]).
valid_axiom(namedIndividual(A)) :- subsumed_by([A],[iri]).

%% anonymousIndividual(?IRI)
% Anonymous individuals are local to the ontology they are contained in. Analagous to bnodes
% @see construct/1
:- dynamic(anonymousIndividual/1).
:- multifile(anonymousIndividual/1).
axiompred(anonymousIndividual/1).
axiom_arguments(anonymousIndividual,[iri]).
valid_axiom(anonymousIndividual(A)) :- subsumed_by([A],[iri]).

%% construct(?IRI)
% @see axiom/1, annotation/1, ontology/1
construct(A) :- axiom(A).
construct(A) :- annotation(A).
construct(A) :- ontology(A).
axiom_arguments(construct,[iri]).
valid_axiom(construct(A)) :- subsumed_by([A],[iri]).

%% axiom(?Axiom)
% The main component of an OWL 2 ontology is a set of axioms - statements that say what is true in the domain being modeled.
% @see classAxiom/1, propertyAxiom/1, fact/1
axiom(A) :- classAxiom(A).
axiom(A) :- propertyAxiom(A).
axiom(hasKey(A,B)) :- hasKey(A,B).
axiom(A) :- fact(A).
axiom(A) :- declarationAxiom(A).
%axiom(annotation(A,B,C)) :- annotation(A,B,C). % CJM - treat annotations as axioms
axiom_arguments(axiom,[axiom]).
valid_axiom(axiom(A)) :- subsumed_by([A],[axiom]).

%% classAxiom(?Axiom)
% OWL 2 provides axioms that allow relationships to be established between class expressions. This predicate reifies the actual axiom
% @see equivalentClasses/1, disjointClasses/1, subClassOf/2, disjointUnion/2
classAxiom(equivalentClasses(A)) :- equivalentClasses(A).
classAxiom(disjointClasses(A)) :- disjointClasses(A).
classAxiom(subClassOf(A, B)) :- subClassOf(A, B).
classAxiom(disjointUnion(A, B)) :- disjointUnion(A, B).
axiom_arguments(classAxiom,[axiom]).
valid_axiom(classAxiom(A)) :- subsumed_by([A],[axiom]).

%% subClassOf(?SubClass:ClassExpression, ?SuperClass:ClassExpression)
% A subclass axiom SubClassOf( CE1 CE2 ) states that the class expression CE1 is a subclass of the class expression CE2
%
%   @param SubClass a classExpression/1 representing the more specific class
%   @param SuperClass a classExpression/1 representing the more general class
:- dynamic(subClassOf/2).
:- multifile(subClassOf/2).
axiompred(subClassOf/2).
axiom_arguments(subClassOf,[classExpression, classExpression]).
valid_axiom(subClassOf(A, B)) :- subsumed_by([A, B],[classExpression, classExpression]).


%% equivalentClasses(?ClassExpressions:set(ClassExpression))
% An equivalent classes axiom EquivalentClasses( CE1 ... CEn ) states that all of the class expressions CEi, 1 <= i <= n, are semantically equivalent to each other.
:- dynamic(equivalentClasses/1).
:- multifile(equivalentClasses/1).
axiompred(equivalentClasses/1).
axiom_arguments(equivalentClasses,[set(classExpression)]).
valid_axiom(equivalentClasses(A)) :- subsumed_by([A],[set(classExpression)]).

%% disjointClasses(?ClassExpressions:set(ClassExpression))
% A disjoint classes axiom DisjointClasses( CE1 ... CEn ) states that all of the class expressions CEi, 1 <= i <= n, are pairwise disjoint; that is, no individual can be at the same time an instance of both CEi and CEj for i != j
:- dynamic(disjointClasses/1).
:- multifile(disjointClasses/1).
axiompred(disjointClasses/1).
axiom_arguments(disjointClasses,[set(classExpression)]).
valid_axiom(disjointClasses(A)) :- subsumed_by([A],[set(classExpression)]).

%% disjointUnion(?ClassExpression, ?ClassExpressions:set(ClassExpression))
% A disjoint union axiom DisjointUnion( C CE1 ... CEn ) states that a class C is a disjoint union of the class expressions CEi, 1 <= i <= n, all of which are pairwise disjoint.
:- dynamic(disjointUnion/2).
:- multifile(disjointUnion/2).
axiompred(disjointUnion/2).
axiom_arguments(disjointUnion,[classExpression,set(classExpression)]).
valid_axiom(disjointUnion(A,B)) :- subsumed_by([A,B],[classExpression,set(classExpression)]).

%% propertyAxiom(?Axiom)
% OWL 2 provides axioms that can be used to characterize and establish relationships between object property expressions. This predicate reifies the actual axiom
%
% @see symmetricProperty/1, inverseFunctionalProperty/1, transitiveProperty/1, asymmetricProperty/1, subPropertyOf/2, functionalProperty/1, irreflexiveProperty/1, disjointProperties/1, propertyDomain/2, reflexiveProperty/1, propertyRange/2, equivalentProperties/1, inverseProperties/2
propertyAxiom(symmetricProperty(A)) :- symmetricProperty(A).
propertyAxiom(inverseFunctionalProperty(A)) :- inverseFunctionalProperty(A).
propertyAxiom(transitiveProperty(A)) :- transitiveProperty(A).
propertyAxiom(asymmetricProperty(A)) :- asymmetricProperty(A).
propertyAxiom(subPropertyOf(A, B)) :- subPropertyOf(A, B).
propertyAxiom(functionalProperty(A)) :- functionalProperty(A).
propertyAxiom(irreflexiveProperty(A)) :- irreflexiveProperty(A).
propertyAxiom(disjointProperties(A)) :- disjointProperties(A).
propertyAxiom(propertyDomain(A, B)) :- propertyDomain(A, B).
propertyAxiom(reflexiveProperty(A)) :- reflexiveProperty(A).
propertyAxiom(propertyRange(A, B)) :- propertyRange(A, B).
propertyAxiom(equivalentProperties(A)) :- equivalentProperties(A).
propertyAxiom(inverseProperties(A, B)) :- inverseProperties(A, B).
axiom_arguments(propertyAxiom,[axiom]).
valid_axiom(propertyAxiom(A)) :- subsumed_by([A],[axiom]).

%% subPropertyOf(?Sub:PropertyExpression, ?Super:ObjectPropertyExpressions)
% subproperty axioms are analogous to subclass axioms
% (extensional predicate - can be asserted)
:- dynamic(subPropertyOf/2).
:- multifile(subPropertyOf/2).
axiompred(subPropertyOf/2).
axiom_arguments(subPropertyOf,[propertyExpression, objectPropertyExpressions]).
valid_axiom(subPropertyOf(A, B)) :- subsumed_by([A, B],[propertyExpression, objectPropertyExpressions]).

%% subObjectPropertyOf(?Sub:ObjectPropertyExpressionOrChain, ?Super:ObjectPropertyExpression)
% The basic form is SubPropertyOf( OPE1 OPE2 ). This axiom states that the object property expression OPE1 is a subproperty of the object property expression OPE2 - that is, if an individual x is connected by OPE1 to an individual y, then x is also connected by OPE2 to y. The more complex form is SubPropertyOf( PropertyChain( OPE1 ... OPEn ) OPE ). This axiom states that, if an individual x is connected by a sequence of object property expressions OPE1, ..., OPEn with an individual y, then x is also connected with y by the object property expression OPE
subObjectPropertyOf(A, B) :- subPropertyOf(A, B),subsumed_by([A, B],[objectPropertyExpressionOrChain, objectPropertyExpression]).
axiom_arguments(subObjectPropertyOf,[objectPropertyExpressionOrChain, objectPropertyExpression]).
valid_axiom(subObjectPropertyOf(A, B)) :- subsumed_by([A, B],[objectPropertyExpressionOrChain, objectPropertyExpression]).

%% subDataPropertyOf(?Sub:DataPropertyExpression, ?Super:DataPropertyExpression)
% A data subproperty axiom SubPropertyOf( DPE1 DPE2 ) states that the data property expression DPE1 is a subproperty of the data property expression DPE2 - that is, if an individual x is connected by OPE1 to a literal y, then x is connected by OPE2 to y as well.
subDataPropertyOf(A, B) :- subPropertyOf(A, B),subsumed_by([A, B],[dataPropertyExpression, dataPropertyExpression]).
axiom_arguments(subDataPropertyOf,[dataPropertyExpression, dataPropertyExpression]).
valid_axiom(subDataPropertyOf(A, B)) :- subsumed_by([A, B],[dataPropertyExpression, dataPropertyExpression]).

%% subAnnotationPropertyOf(?Sub:AnnotationProperty, ?Super:AnnotationProperty)
% An annotation subproperty axiom SubPropertyOf( AP1 AP2 ) states that the annotation property AP1 is a subproperty of the annotation property AP2
subAnnotationPropertyOf(A, B) :- subPropertyOf(A, B),subsumed_by([A, B],[annotationProperty, annotationProperty]).
axiom_arguments(subAnnotationPropertyOf,[annotationProperty, annotationProperty]).
valid_axiom(subAnnotationPropertyOf(A, B)) :- subsumed_by([A, B],[annotationProperty, annotationProperty]).

%% equivalentProperties(?PropertyExpressions:set(PropertyExpression))
% An equivalent object properties axiom EquivalentProperties( OPE1 ... OPEn ) states that all of the object property expressions OPEi, 1 <= i <= n, are semantically equivalent to each other
% (extensional predicate - can be asserted)
:- dynamic(equivalentProperties/1).
:- multifile(equivalentProperties/1).
axiompred(equivalentProperties/1).
axiom_arguments(equivalentProperties,[set(propertyExpression)]).
valid_axiom(equivalentProperties(A)) :- subsumed_by([A],[set(propertyExpression)]).

%% equivalentObjectProperties(?PropertyExpressions:set(ObjectPropertyExpression))
% An equivalent object properties axiom EquivalentObjectProperties( OPE1 ... OPEn ) states that all of the object property expressions OPEi, 1 <= i <= n, are semantically equivalent to each other
equivalentObjectProperties(A) :- equivalentProperties(A),subsumed_by([A],[set(objectPropertyExpression)]).
axiom_arguments(equivalentObjectProperties,[set(objectPropertyExpression)]).
valid_axiom(equivalentObjectProperties(A)) :- subsumed_by([A],[set(objectPropertyExpression)]).

%% equivalentDataProperties(?PropertyExpressions:set(DataPropertyExpression))
% An equivalent data properties axiom EquivalentProperties( DPE1 ... DPEn ) states that all the data property expressions DPEi, 1 <= i <= n, are semantically equivalent to each other. This axiom allows one to use each DPEi as a synonym for each DPEj - that is, in any expression in the ontology containing such an axiom, DPEi can be replaced with DPEj without affecting the meaning of the ontology
equivalentDataProperties(A) :- equivalentProperties(A),subsumed_by([A],[set(dataPropertyExpression)]).
axiom_arguments(equivalentDataProperties,[set(dataPropertyExpression)]).
valid_axiom(equivalentDataProperties(A)) :- subsumed_by([A],[set(dataPropertyExpression)]).

%% disjointProperties(?PropertyExpressions:set(PropertyExpression))
% A disjoint properties axiom DisjointProperties( PE1 ... PEn ) states that all of the property expressions PEi, 1 <= i <= n, are pairwise disjoint
% (extensional predicate - can be asserted)
:- dynamic(disjointProperties/1).
:- multifile(disjointProperties/1).
axiompred(disjointProperties/1).
axiom_arguments(disjointProperties,[set(propertyExpression)]).
valid_axiom(disjointProperties(A)) :- subsumed_by([A],[set(propertyExpression)]).

%% disjointObjectProperties(?PropertyExpressions:set(ObjectPropertyExpression))
% A disjoint object properties axiom DisjointProperties( OPE1 ... OPEn ) states that all of the object property expressions OPEi, 1 <= i <= n, are pairwise disjoint; that is, no individual x can be connected to an individual y by both OPEi and OPEj for i != j.
disjointObjectProperties(A) :- disjointProperties(A),subsumed_by([A],[set(objectPropertyExpression)]).
axiom_arguments(disjointObjectProperties,[set(objectPropertyExpression)]).
valid_axiom(disjointObjectProperties(A)) :- subsumed_by([A],[set(objectPropertyExpression)]).

%% disjointDataProperties(?PropertyExpressions:set(DataPropertyExpression))
% A disjoint data properties axiom DisjointProperties( DPE1 ... DPEn ) states that all of the data property expressions DPEi, 1 <= i <= n, are pairwise disjoint; that is, no individual x can be connected to a literal y by both DPEi and DPEj for i !- j.
disjointDataProperties(A) :- disjointProperties(A),subsumed_by([A],[set(dataPropertyExpression)]).
axiom_arguments(disjointDataProperties,[set(dataPropertyExpression)]).
valid_axiom(disjointDataProperties(A)) :- subsumed_by([A],[set(dataPropertyExpression)]).

%% inverseProperties(?ObjectPropertyExpression1:ObjectPropertyExpression, ?ObjectPropertyExpression2:ObjectPropertyExpression)
% An inverse object properties axiom InverseProperties( OPE1 OPE2 ) states that the object property expression OPE1 is an inverse of the object property expression OPE2
% (note there are no inverse data properties, as literals are not connected to individuals)
% Example:
% =|inverseProperties(partOf,hasPart)|=
% (extensional predicate - can be asserted)
:- dynamic(inverseProperties/2).
:- multifile(inverseProperties/2).
axiompred(inverseProperties/2).
axiom_arguments(inverseProperties,[objectPropertyExpression, objectPropertyExpression]).
valid_axiom(inverseProperties(A, B)) :- subsumed_by([A, B],[objectPropertyExpression, objectPropertyExpression]).

%% propertyDomain(?PropertyExpression, ?CE)
%  A property domain axiom PropertyDomain( PE CE ) states that the
%  domain of the property expression PE is CE
% (extensional predicate - can be asserted)

:- dynamic(propertyDomain/2).
:- multifile(propertyDomain/2).
axiompred(propertyDomain/2).
axiom_arguments(propertyDomain,[propertyExpression, classExpression]).
valid_axiom(propertyDomain(A, B)) :- subsumed_by([A, B],[propertyExpression, classExpression]).

%% objectPropertyDomain(?ObjectPropertyExpression, ?ClassExpression)
% An object property domain axiom PropertyDomain( OPE CE ) states that the domain of the object property expression OPE is the class expression CE - that is, if an individual x is connected by OPE with some other individual, then x is an instance of CE
objectPropertyDomain(A, B) :- propertyDomain(A, B),subsumed_by([A, B],[objectPropertyExpression, classExpression]).
axiom_arguments(objectPropertyDomain,[objectPropertyExpression, classExpression]).
valid_axiom(objectPropertyDomain(A, B)) :- subsumed_by([A, B],[objectPropertyExpression, classExpression]).

%% dataPropertyDomain(?DataPropertyExpression, ?ClassExpression)
% A data property domain axiom PropertyDomain( DPE CE ) states that the domain of the data property expression DPE is the class expression CE - that is, if an individual x is connected by DPE with some literal, then x is an instance of CE
dataPropertyDomain(A, B) :- propertyDomain(A, B),subsumed_by([A, B],[dataPropertyExpression, classExpression]).
axiom_arguments(dataPropertyDomain,[dataPropertyExpression, classExpression]).
valid_axiom(dataPropertyDomain(A, B)) :- subsumed_by([A, B],[dataPropertyExpression, classExpression]).

%% annotationPropertyDomain(?AnnotationProperty, ?IRI)
% An annotation property domain axiom PropertyDomain( AP U ) states that the domain of the annotation property AP is the IRI U. Such axioms have no effect on the Direct Semantics of OWL 2
annotationPropertyDomain(A, B) :- propertyDomain(A, B),subsumed_by([A, B],[annotationProperty, iri]).
axiom_arguments(annotationPropertyDomain,[annotationProperty, iri]).
valid_axiom(annotationPropertyDomain(A, B)) :- subsumed_by([A, B],[annotationProperty, iri]).

%% propertyRange(?PropertyExpression, ?ClassExpression)
% An object property domain axiom PropertyRange( OPE CE ) states that the domain of the object property expression OPE is the class expression CE - that is, if an individual x is connected by OPE with some other individual, then x is an instance of CE
% (extensional predicate - can be asserted)
:- dynamic(propertyRange/2).
:- multifile(propertyRange/2).
axiompred(propertyRange/2).
axiom_arguments(propertyRange,[propertyExpression, classExpression]).
valid_axiom(propertyRange(A, B)) :- subsumed_by([A, B],[propertyExpression, classExpression]).

%% objectPropertyRange(?ObjectPropertyExpression, ?ClassExpression)
% An object property domain axiom PropertyRange( OPE CE ) states that the domain of the object property expression OPE is the class expression CE - that is, if an individual x is connected by OPE with some other individual, then x is an instance of CE
objectPropertyRange(A, B) :- propertyRange(A, B),subsumed_by([A, B],[objectPropertyExpression, classExpression]).
axiom_arguments(objectPropertyRange,[objectPropertyExpression, classExpression]).
valid_axiom(objectPropertyRange(A, B)) :- subsumed_by([A, B],[objectPropertyExpression, classExpression]).

%% dataPropertyRange(?ObjectPropertyExpression, ?DataRange)
% A data property range axiom PropertyRange( DPE DR ) states that the range of the data property expression DPE is the data range DR - that is, if some individual is connected by DPE with a literal x, then x is in DR. The arity of DR MUST be one
dataPropertyRange(A, B) :- propertyRange(A, B),subsumed_by([A, B],[dataPropertyExpression, dataRange]).
axiom_arguments(dataPropertyRange,[objectPropertyExpression, dataRange]).
valid_axiom(dataPropertyRange(A, B)) :- subsumed_by([A, B],[objectPropertyExpression, dataRange]).

%% annotationPropertyRange(?AnnotationProperty, ?IRI)
% An annotation property range axiom PropertyRange( AP U ) states that the range of the annotation property AP is the IRI U. Such axioms have no effect on the Direct Semantics of OWL 2
annotationPropertyRange(A, B) :- propertyRange(A, B),subsumed_by([A, B],[annotationProperty, iri]).
axiom_arguments(annotationPropertyRange,[annotationProperty, iri]).
valid_axiom(annotationPropertyRange(A, B)) :- subsumed_by([A, B],[annotationProperty, iri]).

%% functionalProperty(?PropertyExpression)
% An object property functionality axiom FunctionalProperty( OPE ) states that the object property expression OPE is functional - that is, for each individual x, there can be at most one distinct individual y such that x is connected by OPE to y
% (extensional predicate - can be asserted)
:- dynamic(functionalProperty/1).
:- multifile(functionalProperty/1).
axiompred(functionalProperty/1).
axiom_arguments(functionalProperty,[propertyExpression]).
valid_axiom(functionalProperty(A)) :- subsumed_by([A],[propertyExpression]).

%% functionalObjectProperty(?ObjectPropertyExpression)
% An object property functionality axiom FunctionalProperty( OPE ) states that the object property expression OPE is functional - that is, for each individual x, there can be at most one distinct individual y such that x is connected by OPE to y
functionalObjectProperty(A) :- functionalProperty(A),subsumed_by([A],[objectPropertyExpression]).
axiom_arguments(functionalObjectProperty,[objectPropertyExpression]).
valid_axiom(functionalObjectProperty(A)) :- subsumed_by([A],[objectPropertyExpression]).

%% functionalDataProperty(?DataPropertyExpression)
% A data property functionality axiom FunctionalProperty( DPE ) states that the data property expression DPE is functional - that is, for each individual x, there can be at most one distinct literal y such that x is connected by DPE with y
functionalDataProperty(A) :- functionalProperty(A),subsumed_by([A],[dataPropertyExpression]).
axiom_arguments(functionalDataProperty,[dataPropertyExpression]).
valid_axiom(functionalDataProperty(A)) :- subsumed_by([A],[dataPropertyExpression]).

%% inverseFunctionalProperty(?ObjectPropertyExpression)
% An object property inverse functionality axiom InverseFunctionalProperty( OPE ) states that the object property expression OPE is inverse-functional - that is, for each individual x, there can be at most one individual y such that y is connected by OPE with x. Note there are no InverseFunctional DataProperties
:- dynamic(inverseFunctionalProperty/1).
:- multifile(inverseFunctionalProperty/1).
axiompred(inverseFunctionalProperty/1).
axiom_arguments(inverseFunctionalProperty,[objectPropertyExpression]).
valid_axiom(inverseFunctionalProperty(A)) :- subsumed_by([A],[objectPropertyExpression]).

%% reflexiveProperty(?ObjectPropertyExpression)
% An object property reflexivity axiom ReflexiveProperty( OPE ) states that the object property expression OPE is reflexive - that is, each individual is connected by OPE to itself
:- dynamic(reflexiveProperty/1).
:- multifile(reflexiveProperty/1).
axiompred(reflexiveProperty/1).
axiom_arguments(reflexiveProperty,[objectPropertyExpression]).
valid_axiom(reflexiveProperty(A)) :- subsumed_by([A],[objectPropertyExpression]).

%% irreflexiveProperty(?ObjectPropertyExpression)
% An object property reflexivity axiom ReflexiveProperty( OPE ) states that the object property expression OPE is reflexive - that is, no individual is connected by OPE to itsel
:- dynamic(irreflexiveProperty/1).
:- multifile(irreflexiveProperty/1).
axiompred(irreflexiveProperty/1).
axiom_arguments(irreflexiveProperty,[objectPropertyExpression]).
valid_axiom(irreflexiveProperty(A)) :- subsumed_by([A],[objectPropertyExpression]).

%% symmetricProperty(?ObjectPropertyExpression)
% An object property symmetry axiom SymmetricProperty( OPE ) states that the object property expression OPE is symmetric - that is, if an individual x is connected by OPE to an individual y, then y is also connected by OPE to x
:- dynamic(symmetricProperty/1).
:- multifile(symmetricProperty/1).
axiompred(symmetricProperty/1).
axiom_arguments(symmetricProperty,[objectPropertyExpression]).
valid_axiom(symmetricProperty(A)) :- subsumed_by([A],[objectPropertyExpression]).

%% asymmetricProperty(?ObjectPropertyExpression)
% An object property asymmetry axiom AsymmetricProperty( OPE ) states that the object property expression OPE is asymmetric - that is, if an individual x is connected by OPE to an individual y, then y cannot be connected by OPE to x
:- dynamic(asymmetricProperty/1).
:- multifile(asymmetricProperty/1).
axiompred(asymmetricProperty/1).
axiom_arguments(asymmetricProperty,[objectPropertyExpression]).
valid_axiom(asymmetricProperty(A)) :- subsumed_by([A],[objectPropertyExpression]).

%% transitiveProperty(?ObjectPropertyExpression)
% An object property transitivity axiom TransitiveProperty( OPE ) states that the object property expression OPE is transitive - that is, if an individual x is connected by OPE to an individual y that is connected by OPE to an individual z, then x is also connected by OPE to z
:- dynamic(transitiveProperty/1).
:- multifile(transitiveProperty/1).
axiompred(transitiveProperty/1).
axiom_arguments(transitiveProperty,[objectPropertyExpression]).
valid_axiom(transitiveProperty(A)) :- subsumed_by([A],[objectPropertyExpression]).

%% hasKey(?ClassExpression,?PropertyExpression)
% A key axiom HasKey( CE PE1 ... PEn ) states that each (named) instance of the class expression CE is uniquely identified by the (data or object) property expressions PEi - that is, no two distinct (named) instances of CE can coincide on the values of all property expressions PEi
:- dynamic(hasKey/2).
:- multifile(hasKey/2).
axiompred(hasKey/2).
axiom_arguments(hasKey,[classExpression,propertyExpression]).
valid_axiom(hasKey(CE,PE)) :- subsumed_by([CE,PE],[classExpression,propertyExpression]).


%% fact(?Axiom)
% OWL 2 supports a rich set of axioms for stating assertions - axioms about individuals that are often also called facts. The fact/1 predicate reifies the fact predicate
%
% @see annotationAssertion/3, differentIndividuals/1, negativePropertyAssertion/3, propertyAssertion/3, sameIndividual/1, classAssertion/2
fact(annotationAssertion(A, B, C)) :- annotationAssertion(A, B, C).
fact(differentIndividuals(A)) :- differentIndividuals(A).
fact(negativePropertyAssertion(A, B, C)) :- negativePropertyAssertion(A, B, C).
fact(propertyAssertion(A, B, C)) :- propertyAssertion(A, B, C).
fact(sameIndividual(A)) :- sameIndividual(A).
fact(classAssertion(A, B)) :- classAssertion(A, B).
axiom_arguments(fact,[axiom]).
valid_axiom(fact(A)) :- subsumed_by([A],[axiom]).

%% sameIndividual(?Individuals:set(Individual))
% An individual equality axiom SameIndividual( a1 ... an ) states that all of the individuals ai, 1 <= i <= n, are equal to each other.
% note that despite the name of this predicate, it accepts a list of individuals as argument
:- dynamic(sameIndividual/1).
:- multifile(sameIndividual/1).
axiompred(sameIndividual/1).
axiom_arguments(sameIndividual,[set(individual)]).
valid_axiom(sameIndividual(A)) :- subsumed_by([A],[set(individual)]).

%% differentIndividuals(?Individuals:set(Individual))
% An individual inequality axiom DifferentIndividuals( a1 ... an ) states that all of the individuals ai, 1 <= i <= n, are different from each other
:- dynamic(differentIndividuals/1).
:- multifile(differentIndividuals/1).
axiompred(differentIndividuals/1).
axiom_arguments(differentIndividuals,[set(individual)]).
valid_axiom(differentIndividuals(A)) :- subsumed_by([A],[set(individual)]).

%% classAssertion(?ClassExpression, ?Individual)
% A class assertion ClassAssertion( CE a ) states that the individual a is an instance of the class expression CE
:- dynamic(classAssertion/2).
:- multifile(classAssertion/2).
axiompred(classAssertion/2).
axiom_arguments(classAssertion,[classExpression, individual]).
valid_axiom(classAssertion(A, B)) :- subsumed_by([A, B],[classExpression, individual]).

%% propertyAssertion(?PropertyExpression, ?SourceIndividual:Individual, ?TargetIndividual:Individual)
% A positive object property assertion PropertyAssertion( OPE a1 a2 ) states that the individual a1 is connected by the object property expression OPE to the individual a2
% (extensional predicate - can be asserted)
:- dynamic(propertyAssertion/3).
:- multifile(propertyAssertion/3).
axiompred(propertyAssertion/3).
axiom_arguments(propertyAssertion,[propertyExpression, individual, individual]).
valid_axiom(propertyAssertion(A, B, C)) :- subsumed_by([A, B, C],[propertyExpression, individual, individual]).

%% objectPropertyAssertion(?ObjectPropertyExpression, ?SourceIndividual:Individual, ?TargetIndividual:Individual)
% A positive object property assertion PropertyAssertion( OPE a1 a2 ) states that the individual a1 is connected by the object property expression OPE to the individual a2
objectPropertyAssertion(A, B, C) :- propertyAssertion(A, B, C),subsumed_by([A, B, C],[objectPropertyExpression, individual, individual]).
axiom_arguments(objectPropertyAssertion,[objectPropertyExpression, individual, individual]).
valid_axiom(objectPropertyAssertion(A, B, C)) :- subsumed_by([A, B, C],[objectPropertyExpression, individual, individual]).

%% dataPropertyAssertion(?ObjectPropertyExpression, ?SourceIndividual:Individual, ?TargetValue:Literal)
% A positive data property assertion PropertyAssertion( DPE a lt ) states that the individual a is connected by the data property expression DPE to the literal lt
dataPropertyAssertion(A, B, C) :- propertyAssertion(A, B, C),subsumed_by([A, B, C],[dataPropertyExpression, individual, literal]).
axiom_arguments(dataPropertyAssertion,[objectPropertyExpression, individual, literal]).
valid_axiom(dataPropertyAssertion(A, B, C)) :- subsumed_by([A, B, C],[dataPropertyExpression, individual, literal]).

%% negativePropertyAssertion(?PropertyExpression, ?SourceIndividual:Individual, ?TargetIndividual:Individual)
% A negative object property assertion NegativePropertyAssertion( OPE a1 a2 ) states that the individual a1 is not connected by the object property expression OPE to the individual a2
% (extensional predicate - can be asserted)
:- dynamic(negativePropertyAssertion/3).
:- multifile(negativePropertyAssertion/3).
axiompred(negativePropertyAssertion/3).
axiom_arguments(negativePropertyAssertion,[propertyExpression, individual, individual]).
valid_axiom(negativePropertyAssertion(A, B, C)) :- subsumed_by([A, B, C],[propertyExpression, individual, individual]).

%% negativeObjectPropertyAssertion(?ObjectPropertyExpression, ?SourceIndividual:Individual, ?TargetIndividual:Individual)
% A negative object property assertion NegativePropertyAssertion( OPE a1 a2 ) states that the individual a1 is not connected by the object property expression OPE to the individual a2
negativeObjectPropertyAssertion(A, B, C) :- negativePropertyAssertion(A, B, C),subsumed_by([A, B, C],[objectPropertyExpression, individual, individual]).
axiom_arguments(negativeObjectPropertyAssertion,[objectPropertyExpression, individual, individual]).
valid_axiom(negativeObjectPropertyAssertion(A, B, C)) :- subsumed_by([A, B, C],[objectPropertyExpression, individual, individual]).

%% negativeDataPropertyAssertion(?DataPropertyExpression, ?SourceIndividual:Individual, ?TargetValue:Literal)
% A negative data property assertion NegativePropertyAssertion( DPE a lt ) states that the individual a is not connected by the data property expression DPE to the literal lt
negativeDataPropertyAssertion(A, B, C) :- negativePropertyAssertion(A, B, C),subsumed_by([A, B, C],[dataPropertyExpression, individual, literal]).
axiom_arguments(negativeDataPropertyAssertion,[dataPropertyExpression, individual, literal]).
valid_axiom(negativeDataPropertyAssertion(A, B, C)) :- subsumed_by([A, B, C],[dataPropertyExpression, individual, literal]).

%% annotationAssertion(?AnnotationProperty, ?AnnotationSubject, ?AnnotationValue)
% An annotation assertion AnnotationAssertion( AP as at ) states that the annotation subject as - an IRI or an anonymous individual - is annotated with the annotation property AP and the annotation value av
:- dynamic(annotationAssertion/3).
:- multifile(annotationAssertion/3).
axiompred(annotationAssertion/3).
axiom_arguments(annotationAssertion,[annotationProperty, annotationSubject, annotationValue]).
valid_axiom(annotationAssertion(A, B, C)) :- subsumed_by([A, B, C],[annotationProperty, annotationSubject, annotationValue]).

%% annotation(?IRI,?AnnotationProperty,?AnnotationValue)
%
% @see annotationAnnotation/3, ontologyAnnotation/3, axiomAnnotation/3
:- dynamic(annotation/3).
:- multifile(annotation/3).
axiompred(annotation/3).

annotation(annotationAnnotation(A, B, C)) :- annotationAnnotation(A, B, C).
annotation(axiomAnnotation(A, B, C)) :- axiomAnnotation(A, B, C).
axiom_arguments(annotation,[iri,annotationProperty,annotationValue]).
valid_axiom(annotation(A,B,C)) :- subsumed_by([A,B,C],[iri,annotationProperty,annotationValue]).

%% ontologyAnnotation(?Ontology, ?AnnotationProperty, ?AnnotationValue)
ontologyAnnotation(Ontology,AP,AV) :-
	annotation(Ontology,AP,AV),
	ontology(Ontology).
axiom_arguments(ontologyAnnotation,[ontology, annotationProperty, annotationValue]).
valid_axiom(ontologyAnnotation(A, B, C)) :- subsumed_by([A, B, C],[ontology, annotationProperty, annotationValue]).

%% axiomAnnotation(?Axiom, ?AnnotationProperty, ?AnnotationValue)
axiomAnnotation(Axiom,AP,AV) :-
	annotation(Axiom,AP,AV),
	axiom(Axiom).
axiom_arguments(axiomAnnotation,[axiom, annotationProperty, annotationValue]).
valid_axiom(axiomAnnotation(A, B, C)) :- subsumed_by([A, B, C],[axiom, annotationProperty, annotationValue]).

%% annotationAnnotation(?Annotation, ?AnnotationProperty, ?AnnotationValue)
annotationAnnotation(Annotation,AP,AV) :-
	annotation(Annotation,AP,AV),
	annotation(Annotation).
axiom_arguments(annotationAnnotation,[annotation, annotationProperty, annotationValue]).
valid_axiom(annotationAnnotation(A, B, C)) :- subsumed_by([A, B, C],[annotation, annotationProperty, annotationValue]).

%% ontology(?IRI)
% An ontology in OWL2 is a collection of OWL Axioms
:- dynamic(ontology/1).
:- multifile(ontology/1).
axiompred(ontology/1).
axiom_arguments(ontology,[iri]).
valid_axiom(ontology(A)) :- subsumed_by([A],[iri]).

%% ontologyDirective(?OntologyIRI,?IRI)
% @see ontologyImport/2, ontologyAxiom/2
ontologyDirective(A, B) :- ontologyImport(A, B).
ontologyDirective(A, B) :- ontologyAxiom(A, B).
ontologyDirective(A, B) :- ontologyVersionInfo(A, B).
axiom_arguments(ontologyDirective,[ontology, iri]).
valid_axiom(ontologyDirective(A, B)) :- subsumed_by([A, B],[ontology, iri]).

%% ontologyAxiom(?Ontology, ?Axiom)
% True if Ontology contains Axiom.
% Axiom is a prolog term that is typically asserted and separately and can thus can be executed as a goal.
% For example, an ontology http://example.org# will contain redundant assertions:
% ==
% subClassOf('http://example.org#a', 'http://example.org#b').
% ontologyAxiom('http://example.org#', subClassOf('http://example.org#a','http://example.org#b')).
% ==
:- dynamic(ontologyAxiom/2).
:- multifile(ontologyAxiom/2).
axiompred(ontologyAxiom/2).
axiom_arguments(ontologyAxiom,[ontology, axiom]).
valid_axiom(ontologyAxiom(A, B)) :- subsumed_by([A, B],[ontology, axiom]).

%% ontologyImport(?Ontology, ?IRI)
% True of Ontology imports document IRI
:- dynamic(ontologyImport/2).
:- multifile(ontologyImport/2).
axiompred(ontologyImport/2).
axiom_arguments(ontologyImport,[ontology, iri]).
valid_axiom(ontologyImport(A, B)) :- subsumed_by([A, B],[ontology, iri]).

%% ontologyVersionInfo(?Ontology, ?IRI)
:- dynamic(ontologyVersionInfo/2).
:- multifile(ontologyVersionInfo/2).
axiompred(ontologyVersionInfo/2).
axiom_arguments(ontologyVersionInfo,[ontology, iri]).
valid_axiom(ontologyVersionInfo(A, B)) :- subsumed_by([A, B],[ontology, iri]).

/****************************************
  RESTRICTIONS ON AXIOMS
  ****************************************/

% 11.1
% An object property expression OPE is simple in Ax if, for each object property expression OPE' such that OPE' ->* OPE holds, OPE' is not composite.
% (The property hierarchy relation ->* is the reflexive-transitive closure of ->)
%simpleObjectPropertyExpresion(OPE) :-
%        objectPropertyExpression(OPE),


/****************************************
  EXPRESSIONS
  ****************************************/

subsumed_by([],[]) :- !.
subsumed_by([I|IL],[T|TL]) :-
	!,
	subsumed_by(I,T),
	subsumed_by(IL,TL).
subsumed_by(L,set(T)):-
        !,
        forall(member(I,L),
               subsumed_by(I,T)).
subsumed_by(I,T):-
        !,
	G=..[T,I],
	G.


%% iri(?IRI)
% true if IRI is an IRI. TODO: currently underconstrained, any atomic term can be an IRI
iri(IRI) :- atomic(IRI).	%

%% literal(?Lit)
% true if Lit is an rdf literal
%literal(_).			% TODO
literal(literal(_)).			% TODO


%% objectPropertyExpression(?OPE)
% true if OPE is an ObjectPropertyExpression
% ObjectPropertyExpression := ObjectProperty | InverseObjectProperty
objectPropertyExpression(E) :- objectProperty(E) ; inverseObjectProperty(E).

% give benefit of doubt; e.g. rdfs:label
% in the OWL2 spec we have DataProperty := IRI
% here dataProperty/1 is an asserted fact
objectPropertyExpression(E) :- nonvar(E),iri(E).

objectPropertyExpressionOrChain(propertyChain(PL)) :- forall(member(P,PL),objectPropertyExpression(P)).
objectPropertyExpressionOrChain(PE) :- objectPropertyExpression(PE).


inverseObjectProperty(inverseOf(OP)) :- objectProperty(OP).

dataPropertyExpression(E) :- dataProperty(E).

% give benefit of doubt; e.g. rdfs:label
% in the OWL2 spec we have DataProperty := IRI
% here dataProperty/1 is an asserted fact
dataPropertyExpression(E) :- nonvar(E),iri(E).

%already declared as entity
%datatype(IRI) :- iri(IRI).

%% dataRange(+DR) is semidet
dataRange(DR) :-
    datatype(DR) ;
    dataIntersectionOf(DR );
    dataUnionOf(DR) ;
    dataComplementOf(DR) ;
    dataOneOf(DR) ;
    datatypeRestriction(DR).

%% classExpression(+CE) is semidet
%
% true if CE is a class expression term, as defined in OWL2
%
% Example: =|classExpression(intersectionOf([car,someValuesFrom(hasColor,blue)])))|=
%
% Union of:
%
%    class/1 | objectIntersectionOf/1 | objectUnionOf/1 |
%    objectComplementOf/1 | objectOneOf/1 | objectSomeValuesFrom/1 |
%    objectAllValuesFrom/1 | objectHasValue/1 | objectHasSelf/1 |
%    objectMinCardinality/1 | objectMaxCardinality/1 |
%    objectExactCardinality/1 | dataSomeValuesFrom/1 |
%    dataAllValuesFrom/1 | dataHasValue/1 | dataMinCardinality/1 |
%    dataMaxCardinality/1 | dataExactCardinality/1
classExpression(CE):-
        iri(CE) ;               % NOTE: added to allow cases where class is not imported
    class(CE) ;
    objectIntersectionOf(CE) ; objectUnionOf(CE) ; objectComplementOf(CE) ; objectOneOf(CE) ;
    objectSomeValuesFrom(CE) ; objectAllValuesFrom(CE) ; objectHasValue(CE) ; objectHasSelf(CE) ;
    objectMinCardinality(CE) ; objectMaxCardinality(CE) ; objectExactCardinality(CE) ;
    dataSomeValuesFrom(CE) ; dataAllValuesFrom(CE) ; dataHasValue(CE) ;
    dataMinCardinality(CE) ; dataMaxCardinality(CE) ; dataExactCardinality(CE).

%% objectIntersectionOf(+CE) is semidet
% true if CE is a term intersectionOf(ClassExpression:list)
%
% An intersection class expression IntersectionOf( CE1 ... CEn ) contains all individuals that are instances of all class expressions CEi for 1 <= i <= n.
objectIntersectionOf(intersectionOf(CEs)) :-
	forall(member(CE,CEs),
	       classExpression(CE)).

%% objectUnionOf(+CE) is semidet
% A union class expression UnionOf( CE1 ... CEn ) contains all individuals that are instances of at least one class expression CEi for 1 <= i <= n
objectUnionOf(unionOf(CEs)) :-
	forall(member(CE,CEs),
	       classExpression(CE)).

%% objectComplementOf(+CE) is semidet
%
objectComplementOf(complementOf(CE)) :-
	classExpression(CE).

%% objectOneOf(+CE) is semidet
% An enumeration of individuals OneOf( a1 ... an ) contains exactly the individuals ai with 1 <= i <= n.
objectOneOf(oneOf(Is)) :-
        is_list(Is). % TODO: check if we need to strengthen this check
%objectOneOf(oneOf(Is)) :-
%	forall(member(I,Is),
%	       individual(I)).

%% objectSomeValuesFrom(+R) is semidet
% An existential class expression SomeValuesFrom( OPE CE ) consists of an object property expression OPE and a class expression CE, and it contains all those individuals that are connected by OPE to an individual that is an instance of CE
objectSomeValuesFrom(someValuesFrom(OPE,CE)) :-
	objectPropertyExpression(OPE),
	classExpression(CE).

%% objectAllValuesFrom(+R) is semidet
% A universal class expression AllValuesFrom( OPE CE ) consists of an object property expression OPE and a class expression CE, and it contains all those individuals that are connected by OPE only to individuals that are instances of CE
objectAllValuesFrom(allValuesFrom(OPE,CE)) :-
	objectPropertyExpression(OPE),
	classExpression(CE).

%% objectHasValue(+R) is semidet
% A has-value class expression HasValue( OPE a ) consists of an object property expression OPE and an individual a, and it contains all those individuals that are connected by OPE to a
objectHasValue(hasValue(OPE,I)) :-
	objectPropertyExpression(OPE),
	individual(I).

%% objectHasSelf(+R) is semidet
% A self-restriction HasSelf( OPE ) consists of an object property expression OPE, and it contains all those individuals that are connected by OPE to themselves
objectHasSelf(hasSelf(OPE)) :-
	objectPropertyExpression(OPE).

%% objectMinCardinality(+CR) is semidet
% A minimum cardinality expression MinCardinality( n OPE CE ) consists of a nonnegative integer n, an object property expression OPE, and a class expression CE, and it contains all those individuals that are connected by OPE to at least n different individuals that are instances of CE. If CE is missing, it is taken to be owl:Thing
objectMinCardinality(minCardinality(C,OPE,CE)):-
	number(C),
	C>=0,
	objectPropertyExpression(OPE),
	classExpression(CE).
objectMinCardinality(minCardinality(C,OPE)):-
	number(C),
	C>=0,
	objectPropertyExpression(OPE).


%% objectMaxCardinality(+CR) is semidet
% A maximum cardinality expression MaxCardinality( n OPE CE ) consists of a nonnegative integer n, an object property expression OPE, and a class expression CE, and it contains all those individuals that are connected by OPE to at most n different individuals that are instances of CE. If CE is missing, it is taken to be owl:Thing
objectMaxCardinality(maxCardinality(C,OPE,CE)):-
	number(C),
	C>=0,
	objectPropertyExpression(OPE),
	classExpression(CE).
objectMaxCardinality(maxCardinality(C,OPE)):-
	number(C),
	C>=0,
	objectPropertyExpression(OPE).

%% objectExactCardinality(+CR) is semidet
% An exact cardinality expression ExactCardinality( n OPE CE ) consists of a nonnegative integer n, an object property expression OPE, and a class expression CE, and it contains all those individuals that are connected by OPE to exactly n different individuals that are instances of CE. If CE is missing, it is taken to be owl:Thing
objectExactCardinality(exactCardinality(C,OPE,CE)):-
	number(C),
	C>=0,
	objectPropertyExpression(OPE),
	classExpression(CE).
objectExactCardinality(exactCardinality(C,OPE)):-
	number(C),
	C>=0,
	objectPropertyExpression(OPE).
% NON-NORMATIVE: we accept this in order to maximize compatibility with Thea1
objectExactCardinality(cardinality(C,OPE)):-
	number(C),
	C>=0,
	objectPropertyExpression(OPE).


%% dataIntersectionOf(+DR:dataIntersectionOf) is semidet
% An intersection data range IntersectionOf( DR1 ... DRn ) contains all data values that are contained in the value space of every data range DRi for 1 <= i <= n. All data ranges DRi must be of the same arity
dataIntersectionOf(intersectionOf(DRs)) :-
	forall(member(DR,DRs),
	       dataRange(DR)).

%% dataUnionOf(+DR:dataUnionOf) is semidet
% A union data range UnionOf( DR1 ... DRn ) contains all data values that are contained in the value space of at least one data range DRi for 1 <= i <= n. All data ranges DRi must be of the same arity
dataUnionOf(unionOf(DRs)) :-
	forall(member(DR,DRs),
	       dataRange(DR)).

%% dataComplementOf(+DR:dataComplementOf) is semidet
% A complement data range ComplementOf( DR ) contains all literals that are not contained in the data range DR
dataComplementOf(complementOf(DR)) :-
	dataRange(DR).

%% dataOneOf(+DR:dataOneOf) is semidet
% An enumeration of literals OneOf( lt1 ... ltn ) contains exactly the explicitly specified literals lti with 1 <= i <= n
dataOneOf(oneOf(DRs)) :-
	forall(member(DR,DRs),
	       dataRange(DR)).

%% datatypeRestriction(+DR) is semidet
%
% TODO: multiple args
datatypeRestriction(datatypeRestriction(DR,FacetValues)):-
	datatype(DR),
	FacetValues=[_|_].

%% dataSomeValuesFrom(+DR) is semidet
dataSomeValuesFrom(someValuesFrom(DPE,DR)):-
	dataPropertyExpressions(DPE),
	dataRange(DR).

%% dataAllValuesFrom(+DR) is semidet
dataAllValuesFrom(allValuesFrom(DPE,DR)):-
	dataPropertyExpressions(DPE),
	dataRange(DR).

%% dataHasValue(+DR) is semidet
% A has-value class expression HasValue( DPE lt ) consists of a data property expression DPE and a literal lt, and it contains all those individuals that are connected by DPE to lt. Each such class expression can be seen as a syntactic shortcut for the class expression SomeValuesFrom( DPE OneOf( lt ) )
dataHasValue(hasValue(DPE,L)):-
	dataPropertyExpressions(DPE),
	literal(L).

%% dataMinCardinality(+DR) is semidet
% A minimum cardinality expression MinCardinality( n DPE DR ) consists of a nonnegative integer n, a data property expression DPE, and a unary data range DR, and it contains all those individuals that are connected by DPE to at least n different literals in DR. If DR is not present, it is taken to be rdfs:Literal
dataMinCardinality(minCardinality(C,DPE,DR)):-
	number(C),
	C>=0,
	dataPropertyExpression(DPE),
	dataRange(DR).
dataMinCardinality(minCardinality(C,DPE)):-
	number(C),
	C>=0,
	dataPropertyExpression(DPE).



%% dataMaxCardinality(+DR) is semidet
% A maximum cardinality expression MaxCardinality( n DPE DR ) consists of a nonnegative integer n, a data property expression DPE, and a unary data range DR, and it contains all those individuals that are connected by DPE to at most n different literals in DR. If DR is not present, it is taken to be rdfs:Literal.
dataMaxCardinality(maxCardinality(C,DPE,DR)):-
	number(C),
	C>=0,
	dataPropertyExpression(DPE),
	dataRange(DR).
dataMaxCardinality(maxCardinality(C,DPE)):-
	number(C),
	C>=0,
	dataPropertyExpression(DPE).


%% dataExactCardinality(+DR) is semidet
% An exact cardinality expression ExactCardinality( n DPE DR ) consists of a nonnegative integer n, a data property expression DPE, and a unary data range DR, and it contains all those individuals that are connected by DPE to exactly n different literals in DR. If DR is not present, it is taken to be rdfs:Literal
dataExactCardinality(exactCardinality(C,DPE,DR)):-
	number(C),
	C>=0,
	dataPropertyExpression(DPE),
	dataRange(DR).
dataExactCardinality(exactCardinality(C,DPE)):-
	number(C),
	C>=0,
	dataPropertyExpression(DPE).
% NON-NORMATIVE: we accept this in order to maximize compatibility with Thea1
dataExactCardinality(cardinality(C,OPE)):-
	number(C),
	C>=0,
	objectPropertyExpression(OPE).

dataPropertyExpressions(DPEs) :-
	(   is_list(DPEs)
	->  forall(member(DPE,DPEs),
		   dataPropertyExpression(DPE))
	;   dataPropertyExpression(DPEs)).

%% valid_axiom(?Axiom) is semidet
% true if Axiom passes typechecking

/****************************************
  VIEW PREDICATES
  ****************************************/

%% equivalent_to(?X,?Y)
% note: this is currently slow for bound values of X and Y
equivalent_to(X,Y) :- equivalentClasses(L),member(X,L),member(Y,L),X\=Y.
equivalent_to(X,Y) :- equivalentProperties(L),member(X,L),member(Y,L),X\=Y.

disjoint_with(X,Y) :- disjointClasses(L),member(X,L),member(Y,L),X\=Y.

%% anyPropertyAssertion(?Property,?Entity,?Value)
% subsumes propertyAssertion/3 and annotationAssertion/3
anyPropertyAssertion(P,E,V) :- propertyAssertion(P,E,V).
anyPropertyAssertion(P,E,V) :- annotationAssertion(P,E,V).


%% labelAnnotation_value(?X,?Val)
labelAnnotation_value(X,Val) :-
        anyPropertyAssertion('http://www.w3.org/2000/01/rdf-schema#label', X, literal(type(_,Val))),atom(Val).
labelAnnotation_value(X,Val) :-
        anyPropertyAssertion('http://www.w3.org/2000/01/rdf-schema#label', X, literal(lang(_,Val))),atom(Val).
labelAnnotation_value(X,Val) :-
        anyPropertyAssertion('http://www.w3.org/2000/01/rdf-schema#label', X, literal(Val)),atom(Val).

/****************************************
  META-PREDICATES
  ****************************************/


%% axiom_directly_about(?Ax,?About)
% true if Ax is an axiom whose first argument is equal to About.
%
% e.g. axiom_directly_about( subClassOf(X,_), X).
%
% also include property assertions whose second argument is equal to About.
%
% e.g. axiom_directly_about( propertyAssertion(P,X,_), X).
%
axiom_directly_about(Ax,About) :-
        axiom(Ax),
        Ax =.. [_,Arg1|_],
        (   is_list(Arg1)
        ->  member(About,Arg1)
        ;   About=Arg1).
axiom_directly_about(Ax,About) :-
	Ax=propertyAssertion(_,About,_),
        axiom(Ax).
axiom_directly_about(Ax,About) :-
	Ax=annotationAssertion(_,About,_),
        axiom(Ax).
axiom_directly_about(Ax,About) :-
	Ax=classAssertion(_,About),
        axiom(Ax).


%% axiom_directly_references(?Ax:axiom,?Ref)
%
% Ref may be
%  - an axiom
%  - a named entity
%  - an expression
axiom_directly_references(Ax,Ref) :-
        axiom(Ax),
        axiom_or_expression_references(Ax,Ref).

axiom_or_expression_references(X,Ref) :-
        X =.. [P|Args],
        P\=literal,
        member(Arg,Args),
        (   is_list(Arg)
        ->  member(Ref,Arg)
        ;   Ref=Arg).

axiom_about(Ax,About) :-
        axiom_directly_about(Ax,About).
axiom_about(Ax,About) :-
        axiom_directly_about(Ax,X),
        axiom_about(X,About).

axiom_references(Ax,Ref) :-
        axiom_directly_references(Ax,Ref).
axiom_references(Ax,Ref) :-
        axiom_directly_references(Ax,X),
        axiom_or_expression_references(X,Ref).



/****************************************
  UTILITY
  ****************************************/

:- multifile assert_axiom_hook/1.
:- dynamic assert_axiom_hook/1.

%% assert_axiom(+Axiom:axiom)
%
% writes an axiom to the prolog database.
% typically this will just be a matter of calling assert/1. However, in future we
% will have different backing stores (rdf_db, sql), and in these cases calls to
% this predicate will perform the appropriate actions.
%
% this also asserts ontologyAxiom/2, using nb_getval with current_ontology
assert_axiom(Axiom) :-
        assert_axiom_hook(Axiom),
        !.
assert_axiom(Axiom) :-
        assert(Axiom),
	(   nb_current(current_ontology,O)
        ->  assert(ontologyAxiom(O,Axiom))
        ;   true),
        !.

%% assert_axiom(+Axiom:axiom,+Ontology:ontology) is det
%
% as assert_axiom/1, but also asserts to ontologyAxiom/2
assert_axiom(Axiom,O) :-
        assert(Axiom),
	assert(ontologyAxiom(O,Axiom)),
        !.

:- multifile retract_axiom_hook/1.
:- dynamic retract_axiom_hook/1.

%% retract_axiom(+Axiom:axiom)
%
% removes an axiom from the prolog database.
% typically this will just be a matter of calling retract/1. However, in future we
% will have different backing stores (rdf_db, sql), and in these cases calls to
% this predicate will perform the appropriate actions.
%
% also removes ontologyAxiom/2 from ALL ontologies
retract_axiom(Axiom) :-
        retract_axiom_hook(Axiom),
        !.
retract_axiom(Axiom) :-
        retractall(Axiom),
	retractall(ontologyAxiom(_,Axiom)),
        !.

retract_all_axioms :-
        findall(A,axiom(A),Axioms),
        maplist(retract,Axioms),
        findall(ontologyAxiom(O,A),ontologyAxiom(O,A),OAxioms),
        maplist(retract,OAxioms),
	!.


owl2_model_init :-
	assert(annotationProperty('http://www.w3.org/2000/01/rdf-schema#label')),
	assert(annotationProperty('http://www.w3.org/2000/01/rdf-schema#comment')).

consult_axioms(File) :-
        consult(File).


/** <module> Ontology language axioms and expressions

---+ Synopsis

    Example OWL2 ontology as a prolog database:
==
class(organism).
class(animal).
class(carnivore).
class(herbivore).
objectProperty(eats).
subClassOf(animal,organism).
equivalentClasses([carnivore,intersectionOf([animal,someValuesFrom(eats,animal)])]).
disjointClasses([herbivore,carnivore]).
==

---+ Details

This module is a prolog model of the OWL2 language. It consists of predicates for both OWL2 axioms and expressions.

This model is intended to closely parallel Structural Specification and Functional-Style Syntax for OWL2 (http://www.w3.org/TR/owl2-syntax).

* Axioms and Declarations are modeled as prolog predicates (e.g. SubClassOf --> subClassOf/2)
* Class and Property Expressions are modeled as prolog terms. These can be checked via semi-deterministic predicates (e.g. objectIntersectionOf/1)
* Axiom Annotations are modeled as prolog predicates taking a reified axiom clause head as an argument (axiomAnnotation/3)
* The names should correspond exactly, with the leading uppercase character substituted for a lowercase (to avoid quoting in prolog) - the one exception is Import, which maps to the prolog predicate ontologyImport/2 (to avoid confusion with prolog import/1)
* Axioms with variable arguments are modeled as prolog predicates that take prolog lists as arguments (e.g. equivalentClasses/1)
* For programmatic convenience we provide additional abstract predicates that do not necessarily correspond to the OWL syntax (e.g. property/1,fact/1)


---++ Axioms

Extensional predicates are declared for all terminal axiom symbols in the functional syntax;  i.e. subPropertyOf/2, subClassOf/2.
These can be directly asserted, or compiled from a prolog file.

The terms from the OWL2 structural syntax are taken as primitive,
i.e. they are extensional / unit-clauses, and designed to be asserted
or compiled.

Some predicates such as property/1 are intensional - these generalize
over the OWL2 axioms are defined by prolog rules, and should not be
asserted.  In this case property/1 is defined as annotationProperty/1
or dataProperty/1 or objectProperty/1.

For the strong typed model, we also provide intensional predicates
such as subObjectPropertyOf/2 - satisfaction of this predicate is
determined at runtime based on type-checking, if subPropertyOf/2 holds.


---++ Expressions and Type checking

OWL Axioms can take either entities or expressions as arguments.
 Entities are simply prolog atoms corresponding to the IRI.
 Expressions are prolog terms;
 e.g. =|intersectionOf(a,someValuesFrom(inverseOf(p),b))|=

 (Class expressions are also known as Descriptions)

 Optional run-time checking of predicates using valid_axiom/1.

 For example =|subClassOf(intersectionOf(c1,c2),unionOf(c3,c4))|= is
 valid if c1,c2,c3,c4 are all class expressions, but
 =|subClassOf(p1,c1)|= is not valid if p1 is a property

 We can also make checks for specific types: e.g objectIntersectionOf/1

---++ Annotations

  In OWL Syntax, axiom annotations are written using an optional annotation list argument.
  We opt not to do this here; instead we use axiomAnnotation/3 where the first argument is the reified predicate head.
  E.g.

==
subClassOf(cat,mammal).
axiomAnnotation(SubClassOf(cat,mammal),author,linnaeus).
==

---+++ Punning

  OWL2 allows classes to act as individuals, so this is legal (TODO: check!):


==
class(polarBear).
class(endangered).
classAssertion(endangered,polarBear).
==

  ---++ Ontologies

  We use a similar scheme for annotations:

==
subClassOf(cat,mammal).
ontologyAxiom(linnaenTaxonomy,SubClassOf(cat,mammal)).
ontology(linnaenTaxonomy).
==

TODO: check edge cases, eg two ontologies have the same axioms but different annotations

---++ IRIs

By default there is no type checking of IRIs, so =|class(polarBear)|=
is allowed, even though "polarBear" is not an IRI - this makes for
convenience in working with example ontologies

---+ Open Issues

---++ Structure Sharing

Should we allow bnode IDs as arguments of predicate axioms for
expressions? I don't think this is necessary.

---++ Enumeration of expressions

We provide semi-deterministic predicates of the form
  ?type(+Expression).  Should the mode be extended to allow
  enumeration of all descriptions/expressions? This would probably
  require forcing all expressions to be bnodes OR possibly recursively
  analyzing the term Axiom in a call axiom(Axiom)

---++ Type checking

  Is Tom Schrijvers type checking system going to be integrated into SWI and Yap? Should we use that?

  I am attempting to put as much typing info in the pldoc comments,
  but unsure of the conventions for complex terms.

  LATEST: see owl2_metamodel.pl

---++ Ontologies

  continue using ontologyAxiom/2? Alternatively use builtin prolog module mechanism..?

---+ See Also

* owl2_from_rdf.pl
* swrl.pl

---+ Additional Information

@see     README
@license License

*/
