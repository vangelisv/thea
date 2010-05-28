/* -*- Mode: Prolog -*- */

:- module(owl2_tbox_reasoner,
          [
	   subClassOfRT/2,
	   subClassOfT/2,
	   subClassOfT/3
          ]).

:- use_module(owl2_model).

subClassOfRT(A,A) :- class(A).
subClassOfRT(A,B) :- subClassOfT(A,B).

subClassOfT(A,B) :- subClassOfT(A,B,x).

subClassOfT(A,B,x) :- subClassOf(A,B).


% potential cycles?
subClassOfT(A,B,x) :- equivalent_to(A,B). 


subClassOfT(A,B,Depth) :-
    subClassOf(A,Z),
    subClassOfT(Z,B,Depth).

subClassOfT(A,B,x(Depth)) :-
    nonvar(A),
    A=someValuesFrom(RA,AX),
    nonvar(RA),
    nonvar(AX),
    B=someValuesFrom(RB,BX),
    subPropertyOfRT(RA,RB),
    subClassOfT(AX,BX,Depth),
    depth_check(Depth).

subClassOfT(A,B,x(Depth)) :-
    nonvar(B),
    B=someValuesFrom(RA,BX),
    nonvar(RA),
    nonvar(BX),
    A=someValuesFrom(RB,AX),
    subPropertyOfRT(RA,RB),
    subClassOfT(AX,BX,Depth),
    depth_check(Depth).    

subClassOfT(A,B,x(Depth)) :-
    var(B),
    A=someValuesFrom(R,AX),
    nonvar(R),
    nonvar(AX),
    transitiveProperty(R),
    subClassOfT(AX,someValuesFrom(R,B),Depth),
    depth_check(Depth).


subPropertyOfRT(A,B) :-
      subPropertyOfT(A,B).
subPropertyOfRT(A,A) :- objectProperty(A).


subPropertyOfT(A,B) :-
	subPropertyOf(A,B).

subPropertyOfT(A,B) :-
	subPropertyOf(A,Z),
	subPropertyOfT(Z,B).


depth_check(Depth) :-
	Depth\=x(x(_)).
