/* -*- Mode: Prolog -*- */

:- module(owl2_lcs,
          [
           class_pair_common_subsumer/4,
           class_pair_common_subsumer_ext/4,
           class_pair_least_common_subsumer_ext/4
          ]).

:- use_module(owl2_model).
:- use_module(owl2_reasoner).

class_pair_common_subsumers(R,A,B,CSs) :-
        class_pair_common_subsumers(R,A,B,_,_,CSs).
class_pair_common_subsumers(R,A,B,APs,BPs,CSs) :-
        setof(X,reasoner_ask(R,subClassOf(A,X)),APs),
        setof(X,reasoner_ask(R,subClassOf(B,X)),BPs),
        ord_intersection(APs,BPs,CSs).

class_pair_common_subsumer(R,A,B,CS) :-
        class_pair_common_subsumers(R,A,B,CSs),
        member(CS,CSs).


class_pair_least_common_subsumer(R,A,B,LCS) :-
        class_pair_common_subsumers(R,A,B,CSs),
        member(LCS,CSs),
        % TODO - equivalence
        \+ ((member(X,CSs),
             X\=LCS,
             reasoner_ask(R,subClassOf(X,LCS)))).

% ----------------------------------------
% EXTENDED - experimental
% ----------------------------------------
% finds class expressions

class_pair_common_subsumer_ext(R,A,B,CS) :-
        class_pair_common_subsumers(R,A,B,CSs),
        member(C1,CSs), % eg r1 some (r2 some a)
        member(C2,CSs), % eg r1 some (r2 some b)
        \+ subsumes_or_subsumed_by(R,C1,C2),
        % now make r1 some (r2 some a and b)
        combine_expr_pair(C1,C2,CS),
        is_subsumed_by(R,A,CS),
        is_subsumed_by(R,B,CS).

class_pair_least_common_subsumer_ext(R,A,B,CS) :-
        class_pair_common_subsumer_ext(R,A,B,CS),
        debug(foo,'TEST: is_subsumer_of(~q,???).',[CS]),
        \+ ((class_pair_common_subsumer_ext(R,A,B,X),
             \+ is_equivalent(R,X,CS),
             is_subsumed_by(R,X,CS),
             debug(foo,'  fail: is_subsumed_by(~w,~q,~q).',[R,X,CS]))).



% takes two linear chain expressions and makes an intersection
% e.g. likes some (directs some (about some foo)) +
%      likes some (directs some (about some bar))
%  =>  likes some (directs some (about some foo and bar))
%      likes some (directs some about some foo) and (directs some about some bar)
%      (likes some directs some about some foo) and (likes some directs some about some bar) 
combine_expr_pair(C1,C2,intersectionOf([C1,C2])).
combine_expr_pair(C1x,C2x,someValuesFrom(R,CE) ) :-
        C1x=someValuesFrom(R,C1),
        C2x=someValuesFrom(R,C2),
        \+ subsumes_or_subsumed_by(R,C1,C2),
        combine_expr_pair(C1,C2,CE).

is_equivalent(_,C,C) :- !.
is_equivalent(R,C1,C2) :-
        is_subsumed_by(R,C1,C2),
        is_subsumed_by(R,C2,C1).
subsumes_or_subsumed_by(R,C1,C2) :-       is_subsumed_by(R,C1,C2).
subsumes_or_subsumed_by(R,C1,C2) :-       is_subsumed_by(R,C2,C1).


is_subsumed_by(_,X,X).
is_subsumed_by(R,A,intersectionOf(L)) :-
        forall(member(X,L),
               is_subsumed_by(R,A,X)).
is_subsumed_by(R,intersectionOf(L),B) :-
        member(X,L),
        is_subsumed_by(R,X,B).
is_subsumed_by(R,someValuesFrom(P,X),someValuesFrom(P,Y)) :-
        is_subsumed_by(R,X,Y).
is_subsumed_by(R,someValuesFrom(P,X),someValuesFrom(P,Y)) :-
        transitiveProperty(P),
        is_subsumed_by(R,X,someValuesFrom(P,Y)).
is_subsumed_by(R,A,X) :-
        reasoner_ask(R,subClassOf(A,X)).


        

        
