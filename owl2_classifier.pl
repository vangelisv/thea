/* -*- Mode: Prolog -*- */

:- module(owl2_classifier,
          [
           generate_class_expressions/0,
           optimal_description/1
          ]).

:- use_module(owl2_model).
:- use_module(owl2_basic_reasoner).

% failure-driven loop to avoid stack overflow
generate_class_expressions :-
        generate_class_expression(C),
        \+ generated(C),
        format('~q.~n',[C]),
        assert(generated(C)),
        fail.
generate_class_expressions :-
        format('%% done.~n').


generate_class_expression( intersectionOf(XC,someValuesFrom(P,YC)) ):-
        property_assertion_least_common_ancestor(P,_,_,XC,YC).

        

%% EVERYTHING BELOW HERE IS OLD

description_instance(D,I) :-
        entailed(classAssertion(D,I)).

description_instances(D,IL) :-
        setof(I,description_instance(D,I),IL).


candidate_description(intersectionOf([X,someValuesFrom(P,Y)])) :-
        propertyAssertion(P,XI,YI),
        classAssertion(XC,XI),
        classAssertion(YC,YI),
        entailed(subClassOf(XC,X)),
        class(X),
        entailed(subClassOf(YC,Y)), % TODO : subProp
        class(Y).

optimal_description(D) :-
        specific_description(D),
        \+ ((entailed(subClassOf(D,Z)), % TODO: add to reasoner
             entailed(subClassOf(Z,D)))).

specific_description(D) :-
        candidate_description(D),
        description_instances(D,DIL),
        \+ ((candidate_description(D2),
             entailed(subClassOf(D2,D)),
             D2\=D,
             description_instances(D2,DIL))),
        writeln(D-DIL).



/** <module> Various classifierity predicates for OWL ontologies

  ---+ Synopsis

==
:- use_module(bio(owl2_classifier)).

% 
demo:-
  nl.
  

==

---+ Details



---+ Additional Information



*/
