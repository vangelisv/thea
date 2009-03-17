/* -*- Mode: Prolog -*- */

:- module(owl2_owlapi_swrl_hooks,
          [
           ]).

:- use_module(library(jpl)).
:- use_module(swrl).
:- use_module(owl2_model).
:- use_module(owl2_metamodel).


prefix('org.semanticweb.owl.model').

swrl_java(Fac,V,Ob) :-
        i_variable(V),
        !,
        atom_javaURI(V,VU),
        jpl_call(Fac,getSWRLAtomIVariable,[VU],Ob). % TODO: dvariable
swrl_java(Fac,implies(A,C),Ob) :-
        !,
        swrl_java(Fac,A,JA),
        swrl_java(Fac,C,JC),
        list_jset(JA,SJA),
        list_jset(JC,SJC),
        jpl_call(Fac,getSWRLRule,[SJA,SJB],Ob).
swrl_java(Fac,description(CE,I),Ob) :-
        !,
        swrl_java(Fac,CE,JCE),
        swrl_java(Fac,I,JI),
        jpl_call(Fac,getSWRLClassAtom,[JCE,JI],Ob).
swrl_java(Fac,sameAs(X,Y),Ob) :-
        !,
        swrl_java(Fac,X,JX),
        swrl_java(Fac,Y,JY),
        jpl_call(Fac,getSWRLSameAsAtom,[JX,JY],Ob).
swrl_java(Fac,differentFrom(X,Y),Ob) :-
        !,
        swrl_java(Fac,X,JX),
        swrl_java(Fac,Y,JY),
        jpl_call(Fac,getSWRLdifferentFromAtom,[JX,JY],Ob).

swrl_java(Fac,A,Ob) :-
        A=..[F,X],              % e.g. artist(v(x))
        !,
        swrl_java(description(F,X)).
swrl_java(Fac,A,Ob) :-
        A=..[F,X,Y],              % TODO: datavalue
        !,
        swrl_java(Fac,X,JX),
        swrl_java(Fac,Y,JY),
        jpl_call(Fac,getSWRLObjectPropertyAtom,[JX,JY],Ob).

getSWRLSameAsAtom
