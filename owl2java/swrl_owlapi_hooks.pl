/* -*- Mode: Prolog -*- */

:- module(owlapi_swrl_hooks,
          [
           ]).

:- use_module(library(jpl)).
:- use_module(swrl).
:- use_module(owl2_model).
:- use_module(owl2_metamodel).

prefix('org.semanticweb.owl.model').

:- multifile owl2_java_owlapi:owlterm_java/4.

owl2_java_owlapi:owlterm_java(Fac,rule,Pl,Obj) :-
        swrl_java(Fac,Pl,Obj).

swrl_java(Fac,i(V),Ob) :-
        !,
        owl2_java_owlapi:atom_javaURI(V,VU),
        jpl_call(Fac,getSWRLAtomIVariable,[VU],Ob). % TODO: dvariable
swrl_java(Fac,implies(A,C),Ob) :-
        !,
        maplist(swrl_java(Fac),A,JAL),
        list_jset(JAL,SJA),
        maplist(swrl_java(Fac),C,JCL),
        list_jset(JCL,SJC),
        jpl_call(Fac,getSWRLRule,[SJA,SJC],Ob).
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
        swrl_java(Fac,description(F,X),Ob).
swrl_java(Fac,A,Ob) :-
        A=..[P,X,Y],              % TODO: datavalue
        !,
        owl2_java_owlapi:owlterm_java(Fac,_,objectProperty(P),JP),
        swrl_java(Fac,X,JX),
        swrl_java(Fac,Y,JY),
        jpl_call(Fac,getSWRLObjectPropertyAtom,[JP,JX,JY],Ob).
swrl_java(_,A,Ob) :-
        owl2_java_owlapi:atom_javaURI(A,Ob).

list_jset(L,JSet) :-
        jpl_new('java.util.HashSet',[],JSet),
        forall(member(Obj,L),
               jpl_call(JSet,add,[Obj],_)).


/** <module> includes SWRL in translation to OWLAPI

  ---+ Synopsis

==
[owl2_from_rdf].
[swrl].
[swrl_rdf_hooks].
owl_parse_rdf('testfiles/dl-safe-ancestor.owl').
['owl2java/swrl_owlapi_hooks'].
[owl2_java_owlapi].
create_factory(Man,Fac),
build_ontology(Man,Fac,Ont),
save_ontology(Man,Ont,'file:///tmp/foo').
==

---+ Details





*/
