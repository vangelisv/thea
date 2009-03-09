/* -*- Mode: Prolog -*- */

:- module(owl2_metamodel,
	  [
           owlpredicate_typed/2,
           owlpredicate_arguments/2
	  ]).

:- use_module(library(lists)). % Yap

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

owlpredicate_arguments(objectIntersectionOf,[list(classExpression)]).
owlpredicate_arguments(dataIntersectionOf,[list(dataExpression)]).
owlpredicate_arguments(objectSomeValuesFrom,[objectPropertyExpression,classExpression]).
owlpredicate_arguments(dataSomeValuesFrom,[dataPropertyExpression,dataExpression]).
owlpredicate_arguments(objectAllValuesFrom,[objectPropertyExpression,classExpression]).
owlpredicate_arguments(dataAllValuesFrom,[dataPropertyExpression,dataExpression]).
owlpredicate_arguments(objectComplementOf,[list(classExpression)]).
owlpredicate_arguments(dataComplementOf,[list(dataExpression)]).
owlpredicate_arguments(objectUnionOf,[list(classExpression)]).
owlpredicate_arguments(dataUnionOf,[list(dataExpression)]).
owlpredicate_arguments(objectOneOf,[list(individual)]).
owlpredicate_arguments(dataOneOf,[list(dataRange)]).


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
