/* -*- Mode: Prolog -*- */

:- module(owl2_tbox_reasoner,
          [
	   subClassOfRT/2,
	   subClassOfT/2,
	   subClassOfT/3
          ]).

:- use_module(owl2_model).
:- use_module(owl2_reasoner).

:- multifile owl2_reasoner:reasoner_ask_hook/2.

reasoner_ask_hook(_,subClassOf(A,B)) :-
	subClassOfRT(A,B).

subClassOfRT(A,A) :- class(A).
subClassOfRT(A,B) :- subClassOfT(A,B).

subClassOfT(A,B) :- subClassOfT(A,B,x).

%subClassOfT(A,B,x) :- subClassOf(A,B).


% --------------------
% BASIC SUBCLASS
% --------------------
% asserted
subClassOfX(A,B) :- subClassOf(A,B).

% treat
%    A = B and ... as A < B
%
% potential cycles?
subClassOfX(A,B) :-
	equivalent_to(A,intersectionOf(L)),
	member(B,L).

% auto-classification - requires tabling
%subClassOfX(A,B) :-
%	equivalent_to(B,intersectionOf(L)),
%	forall(member(X,L),
%	       subClassOfRT(A,L)).

% --------------------
% SUBCLASS TRANSITIVITY
% --------------------
subClassOfT(A,B,Depth) :-
    subClassOfX(A,Z),
    subClassOfT(Z,B,Depth).
subClassOfT(A,B,_) :-
    subClassOfX(A,B).



% --------------------
% EXISTENTIAL RESTRICTIONS
% --------------------
% someValueFrom(RA,AX) =< someValuesFrom(RB,BX) if
%   RA =< RB and AX =< BX
subClassOfT(A,B,Depth) :-
    nonvar(A),
    A=someValuesFrom(RA,AX),
    nonvar(RA),
    nonvar(AX),
    B=someValuesFrom(RB,BX),
    subPropertyOfRT(RA,RB),
    subClassOfT(AX,BX,x(Depth)),
    depth_check(Depth).

subClassOfT(A,B,Depth) :-
    nonvar(B),
    B=someValuesFrom(RA,BX),
    nonvar(RA),
    nonvar(BX),
    A=someValuesFrom(RB,AX),
    subPropertyOfRT(RA,RB),
    subClassOfT(AX,BX,x(Depth)),
    depth_check(Depth).    

subClassOfT(A,B,Depth) :-
    var(B),
    A=someValuesFrom(R,AX),
    nonvar(R),
    nonvar(AX),
    transitiveProperty(R),
    subClassOfT(AX,someValuesFrom(R,B),x(Depth)),
    depth_check(Depth).


% --------------------
% SUBPROPERTY TRANSITIVITY
% --------------------
subPropertyOfRT(A,B) :-
      subPropertyOfT(A,B).
subPropertyOfRT(A,A) :- nonvar(A).
%subPropertyOfRT(A,A) :- objectProperty(A).


subPropertyOfT(A,B) :-
	subPropertyOf(A,B).

subPropertyOfT(A,B) :-
	subPropertyOf(A,Z),
	subPropertyOfT(Z,B).


depth_check(Depth) :-
	Depth\=x(x(x(_))).
