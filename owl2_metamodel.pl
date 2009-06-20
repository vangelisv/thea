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
owlpredicate_arguments(objectComplementOf,[set(classExpression)]).
owlpredicate_arguments(dataComplementOf,[set(dataExpression)]).
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
