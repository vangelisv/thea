/* -*- Mode: Prolog -*- */

:- module(owl2_plsyn,[
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

:- op(1150,fx,class).
:- op(1150,fx,individual).
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
        maplist(plsyn2owl,Args,Args2),
        Owl=..[OwlPred|Args2].
plsyn2owl(X,X) :- !.

owlsyn2pl(Owl,Pl) :-
        Owl=..[OwlPred|Args],
        plpred2owlpred(PlPred,OwlPred),
        !,
        maowlist(owlsyn2pl,Args,Args2),
        maowlist(owlsyn2pl,Args,Args2),
        Pl=..[PlPred|Args2].
owlsyn2pl(X,X) :- !.

plpred2owlpred(transitive,transitiveProperty).

plpred2owlpred(<,subClassOf). % TODO -- use for subpropertyof too




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
