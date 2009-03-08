/* -*- Mode: Prolog -*- */

:- module(owl2_metamodel,
	  [
	  ]).

:- use_module(library(lists)). % Yap

owlpredicate_typed(intersectionOf,objectIntersectionOf).
owlpredicate_typed(intersectionOf,dataIntersectionOf).
owlpredicate_typed(someValuesFrom,objectSomeValuesFrom).
owlpredicate_typed(someValuesFrom,dataSomeValuesFrom).
owlpredicate_typed(complementOf,objectComplementOf).
owlpredicate_typed(complementOf,dataComplementOf).
owlpredicate_typed(unionOf,objectUnionOf).
owlpredicate_typed(unionOf,dataUnionOf).

owlpredicate_arguments(objectIntersectionOf,[list(classExpression)]).
owlpredicate_arguments(dataIntersectionOf,[list(dataExpression)]).
owlpredicate_arguments(objectSomeValuesFrom,[objectPropertyExpression,list(classExpression)]).
owlpredicate_arguments(dataSomeValuesFrom,[dataPropertyExpression,list(dataExpression)]).
owlpredicate_arguments(objectComplementOf,[list(classExpression)]).
owlpredicate_arguments(dataComplementOf,[list(dataExpression)]).
owlpredicate_arguments(objectUnionOf,[list(classExpression)]).
owlpredicate_arguments(dataUnionOf,[list(dataExpression)]).

owlpredicate_arguments(oneOf,[list(individual)]).


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
