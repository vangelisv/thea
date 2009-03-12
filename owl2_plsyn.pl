/* -*- Mode: Prolog -*- */

:- module(owl2_plsyn,[
                      op(1200,xfy,(--)),
                      op(1150,fx,class),
 op(1150,fx,individual),
 op(1150,fx,transitive),
 op(1150,fx,symmetric),
 op(1150,fx,asymmetric),
 op(1150,fx,reflexive),
 op(1150,fx,irreflexive),
% 700 <
% 700 =
 op(600,xfy,not),
 op(500,xfy,or),
 op(200,xfy,and),
 op(200,xfy,that),
 op(150,xfy,some),
 op(150,xfy,only),
 op(150,xfy,value)

                      ]).

:- op(1200,xfy,(--)).
:- op(1150,fx,class).
:- op(1150,fx,individual).

:- op(1150,xfy,disjointUnion).

:- op(1150,fx,transitive).
:- op(1150,fx,symmetric).
:- op(1150,fx,asymmetric).
:- op(1150,fx,reflexive).
:- op(1150,fx,irreflexive).
% 700 <
% 700 =
:- op(600,xfy,not).
:- op(500,xfy,or).
:- op(200,xfy,and).
:- op(200,xfy,that).
:- op(150,xfy,some).
:- op(150,xfy,only).
:- op(150,xfy,value).

plsyn_owl(Pl,Owl) :-
        nonvar(Pl),
        plsyn2owl(Pl,Owl).
plsyn_owl(Pl,Owl) :-
        nonvar(Owl),
        owl2plsyn(Owl,Pl).

plsyn2owl(Pl,Owl) :-
        Pl=..[PlPred|Args],
        plpred2owlpred(PlPred,OwlPred),
        !,
        maplist(plsyn2owl,Args,Args2),
        Owl=..[OwlPred|Args2].
plsyn2owl(Pl,Owl) :-
        Pl=..[PlPred|Args],
        plpred2owlpred_list(PlPred,OwlPred),
        !,
        maplist(plsyn2owl,Args,Args2),
        Owl=..[OwlPred,[Args2]].

% TODO: entity annotations
plsyn2owl(Ax--Comments,[PlAx,axiomAnnotation('rdfs:comment',literal(Comments))]) :-
        !,
        plsyn2owl(Ax,PlAx).

plsyn2owl(R < R1*R2,subPropertyOf(R,propertyChain(Chain))) :-
        plsyn2owl_ec(R1*R2,(*),Chain).

% we can chain over a=b=c=d as equivalent/sameAs is transitive
% (note we cannot do this for different/disjoint)
plsyn2owl(A=B,sameIndividual(ECs)) :-
        !,
        plsyn2owl_ec(A=B,(=),ECs).
plsyn2owl(A==B,equivalentClasses(ECs)) :-
        !,
        plsyn2owl_ec(A==B,(==),ECs).
plsyn2owl(X,X) :- !.


plsyn2owl_ec(T,Op,L) :-
        T=..[Op,A,B],
        !,
        plsyn2owl_ec(A,Op,LA),
        plsyn2owl_ec(B,Op,LB),
        append(LA,LB,L).
plsyn2owl_ec(A,_,[A]).
        


owlsyn2pl(Owl,Pl) :-
        Owl=..[OwlPred|Args],
        plpred2owlpred(PlPred,OwlPred),
        !,
        maplist(owlsyn2pl,Args,Args2),
        Pl=..[PlPred|Args2].
owlsyn2pl(X,X) :- !.

plpred2owlpred(transitive,transitiveProperty).

plpred2owlpred(<,subClassOf). % TODO -- use for subpropertyof too

plpred2owlpred_list(\=,differentIndividuals). 
plpred2owlpred_list(\=,disjointClasses). 





/** <module> prolog-style syntactic sugar for OWL

  ---+ Synopsis

==
:- use_module(bio(owl2_plsyn)).

% 
demo:-
  nl.
  

==

---+ Details



---+ Additional Information

This module is part of blip. For more details, see http://www.blipkit.org

@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/
