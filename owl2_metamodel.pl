/* -*- Mode: Prolog -*- */

:- module(owl2_metamodel,
	  [
           owlpredicate_typed/2,
           owlpredicate_arguments/2
	  ]).

:- use_module(library(lists)). % Yap



%% owlpredicate_typed(?UntypedPredicate,?TypedPredicate) is non det
%
% true if UntypedPredicate has a stringly typed for TypedPredicate
% Example: =|owlpredicate_typed(intersectionOf,objectIntersectionOf)|=

owlpredicate_typed(intersectionOf,objectIntersectionOf).
owlpredicate_typed(intersectionOf,dataIntersectionOf).
owlpredicate_typed(someValuesFrom,objectSomeValuesFrom).
owlpredicate_typed(someValuesFrom,dataSomeValuesFrom).
owlpredicate_typed(allValuesFrom,objectAllValuesFrom).
owlpredicate_typed(allValuesFrom,dataAllValuesFrom).
owlpredicate_typed(complementOf,objectComplementOf).
owlpredicate_typed(complementOf,dataComplementOf).
owlpredicate_typed(unionOf,objectUnionOf).
owlpredicate_typed(unionOf,dataUnionOf).
owlpredicate_typed(oneOf,objectOneOf).
owlpredicate_typed(oneOf,dataOneOf).
owlpredicate_typed(hasValue,objectHasValue).
owlpredicate_typed(hasValue,dataHasValue).
owlpredicate_typed(minCardinality,objectMinCardinality).
owlpredicate_typed(minCardinality,dataMinCardinality).
owlpredicate_typed(maxCardinality,objectMaxCardinality).
owlpredicate_typed(maxCardinality,dataMaxCardinality).
owlpredicate_typed(exactCardinality,objectExactCardinality).
owlpredicate_typed(exactCardinality,dataExactCardinality).

owlpredicate_typed(functionalProperty,functionalObjectProperty).
owlpredicate_typed(functionalProperty,functionalDataProperty).

owlpredicate_typed(subPropertyOf,subObjectPropertyOf).
owlpredicate_typed(subPropertyOf,subDataPropertyOf).
owlpredicate_typed(subPropertyOf, subAnnotationPropertyOf).

owlpredicate_typed(disjointProperties,disjointObjectProperties).
owlpredicate_typed(disjointProperties,disjointDataProperties).
owlpredicate_typed(equivalentProperties,equivalentObjectProperties).
owlpredicate_typed(equivalentProperties,equivalentDataProperties).

owlpredicate_typed(propertyDomain,objectPropertyDomain).
owlpredicate_typed(propertyDomain,dataPropertyDomain).
owlpredicate_typed(propertyRange,objectPropertyRange).
owlpredicate_typed(propertyRange,dataPropertyRange).

owlpredicate_typed(propertyAssertion,objectPropertyAssertion).
owlpredicate_typed(propertyAssertion,dataPropertyAssertion).

owlpredicate_typed(datatype, datatypeRestriction).
owlpredicate_typed(negativePropertyAssertion, negativeObjectPropertyAssertion).
owlpredicate_typed(propertyRange, annotationPropertyRange).
owlpredicate_typed(propertyDomain, annotationPropertyDomain).
owlpredicate_typed(negativePropertyAssertion, negativeDataPropertyAssertion).

%% owlpredicate_arguments(?TypedPredicate,?ArgumentExpression:list) is semidet
%
% ArgumentExpression = [ Datatype | set(CE) | CE ]
% where CE is a classExpression/1 and Datatype = int | ...
%
% This can be used for run-time checking (see valid_axiom/1) and
% to help write parsers/writers for strongly-typed formats like OWL-XML
%
% Example:
% =|owlpredicate_arguments(objectIntersectionOf,[set(classExpression)])|=

owlpredicate_arguments(objectIntersectionOf,[set(classExpression)]).
owlpredicate_arguments(dataIntersectionOf,[set(dataExpression)]).
owlpredicate_arguments(objectSomeValuesFrom,[objectPropertyExpression,classExpression]).
owlpredicate_arguments(dataSomeValuesFrom,[dataPropertyExpression,dataExpression]).
owlpredicate_arguments(objectAllValuesFrom,[objectPropertyExpression,classExpression]).
owlpredicate_arguments(dataAllValuesFrom,[dataPropertyExpression,dataExpression]).
owlpredicate_arguments(objectComplementOf,[classExpression]).
owlpredicate_arguments(dataComplementOf,[dataExpression]).
owlpredicate_arguments(objectUnionOf,[set(classExpression)]).
owlpredicate_arguments(dataUnionOf,[set(dataExpression)]).
owlpredicate_arguments(objectOneOf,[set(individual)]).
owlpredicate_arguments(dataOneOf,[set(dataRange)]).
owlpredicate_arguments(objectHasValue,[objectPropertyExpression,individual]). % TODO
owlpredicate_arguments(dataHasValue,[dataPropertyExpression,dataRange]).
owlpredicate_arguments(objectMinCardinality,[int,objectPropertyExpression,classExpression]). % TEST-ME: non-QCR
owlpredicate_arguments(objectMinCardinality,[int,objectPropertyExpression]). % TEST-ME: non-QCR
owlpredicate_arguments(dataMinCardinality,[int,dataPropertyExpression,dataRange]). % TEST-ME: non-QCR
owlpredicate_arguments(dataMinCardinality,[int,dataPropertyExpression]). % TEST-ME: non-QCR
owlpredicate_arguments(objectMaxCardinality,[int,objectPropertyExpression,classExpression]). % TEST-ME: non-QCR
owlpredicate_arguments(objectMaxCardinality,[int,objectPropertyExpression]). % TEST-ME: non-QCR
owlpredicate_arguments(dataMaxCardinality,[int,dataPropertyExpression,dataRange]). % TEST-ME: non-QCR
owlpredicate_arguments(dataMaxCardinality,[int,dataPropertyExpression]). % TEST-ME: non-QCR
owlpredicate_arguments(objectExactCardinality,[int,objectPropertyExpression,classExpression]). % TEST-ME: non-QCR
owlpredicate_arguments(objectExactCardinality,[int,objectPropertyExpression]). % TEST: non-QCR
owlpredicate_arguments(dataExactCardinality,[int,dataPropertyExpression,dataRange]). % TEST-ME: non-QCR
owlpredicate_arguments(dataExactCardinality,[int,dataPropertyExpression]). % TEST: non-QCR

owlpredicate_arguments(inverseOf,[objectProperty]).
owlpredicate_arguments(propertyChain,[list(objectProperty)]).

owlpredicate_arguments(functionalObjectProperty,[objectPropertyExpression]).
owlpredicate_arguments(functionalDataProperty,[dataPropertyExpression]).

owlpredicate_arguments(dataPropertyDomain,[dataPropertyExpression, classExpression]).
owlpredicate_arguments(objectPropertyDomain,[objectPropertyExpression, classExpression]).
owlpredicate_arguments(dataPropertyRange,[dataPropertyExpression, dataRange]).
owlpredicate_arguments(objectPropertyRange,[objectPropertyExpression, classExpression]).

owlpredicate_arguments(subObjectPropertyOf,[objectPropertyExpressionOrChain, objectPropertyExpression]).
owlpredicate_arguments(subDataPropertyOf,[dataPropertyExpression, dataPropertyExpression]).
owlpredicate_arguments(subAnnotationPropertyOf, [annotationProperty, annotationProperty]).

owlpredicate_arguments(disjointObjectProperties,[set(objectPropertyExpression)]).
owlpredicate_arguments(disjointDataProperties,[set(dataPropertyExpression)]).
owlpredicate_arguments(equivalentObjectProperties,[set(objectPropertyExpression)]).
owlpredicate_arguments(equivalentDataProperties,[set(dataPropertyExpression)]).

owlpredicate_arguments(annotationAssertion,[annotationProperty, iri, value]).
owlpredicate_arguments(objectPropertyAssertion,[objectPropertyExpression, individual, individual]).
owlpredicate_arguments(dataPropertyAssertion,[dataPropertyExpression, individual, literal]).

owlpredicate_arguments(negativeObjectPropertyAssertion, [objectPropertyExpression, individual, individual]).
owlpredicate_arguments(annotationPropertyRange, [annotationProperty, iri]).
owlpredicate_arguments(annotationPropertyDomain, [annotationProperty, iri]).
owlpredicate_arguments(negativeDataPropertyAssertion, [dataPropertyExpression, individual, literal]).

% for sql_compiler

:- discontiguous(relation/2,attribute/4).

relation('class',1).
attribute(1,'class','IRI',string).
relation('datatype',1).
attribute(1,'datatype','IRI',string).
relation('objectProperty',1).
attribute(1,'objectProperty','IRI',string).
relation('dataProperty',1).
attribute(1,'dataProperty','IRI',string).
relation('annotationProperty',1).
attribute(1,'annotationProperty','IRI',string).
relation('namedIndividual',1).
attribute(1,'namedIndividual','IRI',string).
relation('subClassOf',2).
attribute(1,'subClassOf','subClass:ClassExpression',string).
attribute(2,'subClassOf','superClass:ClassExpression',string).
relation('equivalentClasses',1).
attribute(1,'equivalentClasses','classExpressions:set(ClassExpression)',string).
relation('disjointClasses',1).
attribute(1,'disjointClasses','classExpressions:set(ClassExpression)',string).
relation('disjointUnion',2).
attribute(1,'disjointUnion','classExpression',string).
attribute(2,'disjointUnion','classExpressions:set(ClassExpression)',list).
relation('subPropertyOf',2).
attribute(1,'subPropertyOf','sub:PropertyExpression',string).
attribute(2,'subPropertyOf','super:ObjectPropertyExpressions',string).
relation('equivalentProperties',1).
attribute(1,'equivalentProperties','propertyExpressions:set(PropertyExpression)',string).
relation('disjointProperties',1).
attribute(1,'disjointProperties','propertyExpressions:set(PropertyExpression)',string).
relation('inverseProperties',2).
attribute(1,'inverseProperties','objectPropertyExpression1:ObjectPropertyExpression',string).
attribute(2,'inverseProperties','objectPropertyExpression2:ObjectPropertyExpression',string).
relation('propertyDomain',2).
attribute(1,'propertyDomain','propertyExpression',string).
attribute(2,'propertyDomain','classExpression',string).
relation('propertyRange',2).
attribute(1,'propertyRange','propertyExpression',string).
attribute(2,'propertyRange','classExpression',string).
relation('functionalProperty',1).
attribute(1,'functionalProperty','propertyExpression',string).
relation('inverseFunctionalProperty',1).
attribute(1,'inverseFunctionalProperty','objectPropertyExpression',string).
relation('reflexiveProperty',1).
attribute(1,'reflexiveProperty','objectPropertyExpression',string).
relation('irreflexiveProperty',1).
attribute(1,'irreflexiveProperty','objectPropertyExpression',string).
relation('symmetricProperty',1).
attribute(1,'symmetricProperty','objectPropertyExpression',string).
relation('asymmetricProperty',1).
attribute(1,'asymmetricProperty','objectPropertyExpression',string).
relation('transitiveProperty',1).
attribute(1,'transitiveProperty','objectPropertyExpression',string).
relation('hasKey',2).
attribute(1,'hasKey','classExpression').
attribute(2,'hasKey','propertyExpression').
relation('sameIndividual',1).
attribute(1,'sameIndividual','individuals:set(Individual)',string).
relation('differentIndividuals',1).
attribute(1,'differentIndividuals','individuals:set(Individual)',string).
relation('classAssertion',2).
attribute(1,'classAssertion','classExpression',string).
attribute(2,'classAssertion','individual',string).
relation('propertyAssertion',3).
attribute(1,'propertyAssertion','propertyExpression',string).
attribute(2,'propertyAssertion','sourceIndividual:Individual',string).
attribute(3,'propertyAssertion','targetIndividual:Individual',string).
relation('negativePropertyAssertion',3).
attribute(1,'negativePropertyAssertion','propertyExpression',string).
attribute(2,'negativePropertyAssertion','sourceIndividual:Individual',string).
attribute(3,'negativePropertyAssertion','targetIndividual:Individual',string).
relation('annotationAssertion',3).
attribute(1,'annotationAssertion','annotationProperty',string).
attribute(2,'annotationAssertion','annotationSubject',string).
attribute(3,'annotationAssertion','annotationValue',string).
relation('ontologyAnnotation',3).
attribute(1,'ontologyAnnotation','ontology',string).
attribute(2,'ontologyAnnotation','annotationProperty',string).
attribute(3,'ontologyAnnotation','annotationValue',string).
relation('axiomAnnotation',3).
attribute(1,'axiomAnnotation','axiom',string).
attribute(2,'axiomAnnotation','annotationProperty',string).
attribute(3,'axiomAnnotation','annotationValue',string).
relation('annotationAnnotation',3).
attribute(1,'annotationAnnotation','annotation',string).
attribute(2,'annotationAnnotation','annotationProperty',string).
attribute(3,'annotationAnnotation','annotationValue',string).
relation('ontology',1).
attribute(1,'ontology','IRI',string).
relation('ontologyAxiom',2).
attribute(1,'ontologyAxiom','ontology',string).
attribute(2,'ontologyAxiom','axiom',string).
relation('ontologyImport',2).
attribute(1,'ontologyImport','ontology',string).
attribute(2,'ontologyImport','IRI',string).
relation('ontologyVersionInfo',2).
attribute(1,'ontologyVersionInfo','ontology',string).
attribute(2,'ontologyVersionInfo','IRI',string).


/** <module> 

---+ Synopsis

---+ Details

This is the prolog metamodel for owl2_model. Meta-modeling adds some abstraction, but allows for reduced code.

Note that the typical Thea user need not use the module at all. It is primarily for use by modules within Thea

---+ Additional Information


@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/
