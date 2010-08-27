/* -*- Mode: Prolog -*- */

:- module(owl2_lcs,
          [
           prepare_optimizations/1,
           class_pair_common_subsumer/4,
           class_pair_least_common_subsumer/4,
           class_pair_common_subsumer_with_union/4,
           class_pair_common_subsumer_ext/4,
           class_pair_least_common_subsumer_ext/4
          ]).

:- use_module(owl2_model).
:- use_module(owl2_reasoner).

prepare_optimizations(_) :-
        ensure_loaded(library(thea2/tabling)),
        table_pred(reasoner_get_subsumer/2).

% TODO - make this a hook
exclude('http://ontology.neuinfo.org/NIF/Backend/BIRNLex_annotation_properties.owl#_birnlex_limbo_class').
exclude('http://ontology.neuinfo.org/NIF/DigitalEntities/NIF-Investigation.owl#birnlex_2087').
exclude('http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-GrossAnatomy.owl#birnlex_6'). % anatomical entity
exclude('http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-GrossAnatomy.owl#birnlex_4'). % organ
exclude('http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-GrossAnatomy.owl#birnlex_16'). % regional part of organ
exclude('http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-GrossAnatomy.owl#birnlex_1167'). % Regional part of brain
exclude('http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-Molecule.owl#CHEBI_23367'). % molecular entity
exclude('http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-Molecule.owl#nlx_mol_20090303'). % molecular role
%%%%%%exclude(X) :- ontologyAxiom(O,class(X)),\+((subClassOf(X,Y),ontologyAxiom(O,class(Y)))). % exclude root classes DO NOT USE - multiple declarations
exclude(someValuesFrom('http://www.obofoundry.org/ro/ro.owl#has_proper_part',_)).
exclude(Class) :-
	atom(Class),
	sub_atom(Class,0,_,_,'http://www.ifomis.org').

exclude_tr(someValuesFrom(_,X)) :- exclude_tr(X).
exclude_tr(X) :- exclude(X).

reasoner_get_subsumer(C,P) :- 
        reasoner_ask(_,subClassOf(C,P)),
        \+ exclude_tr(P).

class_pair_common_subsumers(R,A,B,CSs) :-
        class_pair_common_subsumers(R,A,B,_,_,CSs).
class_pair_common_subsumers(_R,A,B,APs,BPs,CSs) :-
        debug(owlsim,'finding cs(~w,~w)',[A,B]),
        setof(X,reasoner_get_subsumer(A,X),APs),
        setof(X,reasoner_get_subsumer(B,X),BPs),
        debug(owlsim_detail,'   finding intersection',[]),
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
% UNION
% ----------------------------------------

class_pair_common_subsumers_with_union(R,A,B,CSs) :-
        setof(CS,class_pair_common_subsumer_with_union(R,A,B,CS),CSs).

class_pair_common_subsumer_with_union(R,A,B,CS) :-
        debug(owlsim_detail,'finding cs+u(~w,~w)',[A,B]),
        class_pair_common_subsumers(R,A,B,APs,BPs,_),
        member(AP,APs),
        member(BP,BPs),
        mk_union(AP,BP,CS).
class_pair_common_subsumer_with_union(R,A,B,CS) :-
        class_pair_common_subsumer(R,A,B,CS).

mk_union(X,someValuesFrom(R,X),unionOf([X,someValuesFrom(R,X)])).
mk_union(someValuesFrom(R,X),X,unionOf([X,someValuesFrom(R,X)])).

% ----------------------------------------
% EXTENDED - experimental
% ----------------------------------------
% finds class expressions

%% class_pair_common_subsumer_ext(+Reasoner,+ClassA,+ClassB,?CommonSubsumerExpression)
%
% expression is either intersectionOf(...) | someValuesFrom(Prop,Expr)
%
% if both C1 and C2 are in the set of common subsumers for A and B,
% then C1^C2 is also a common subsumer. There may be other common subsumers
% that can be obtained by "threading" the class expressions together.
class_pair_common_subsumer_ext(R,A,B,CS_Out) :-
        class_pair_common_subsumers_with_union(R,A,B,CSs),
        member(C1,CSs),         % eg r1 some (r2 some a)
        member(C2,CSs), % eg r1 some (r2 some b)
        C1 @< C2, % arbitrary direction
        debug(owlsim_detail,'   candidate intersection: ~w ^ ~w',[C1,C2]),
        \+ subsumes_or_subsumed_by(R,C1,C2),
        % now make r1 some (r2 some a and b)
        combine_expr_pair(C1,C2,CS),
        debug(owlsim_detail,'   candidate CS: ~w',[CS]),
        is_subsumed_by_chk(R,A,CS),
        is_subsumed_by_chk(R,B,CS),
        CS_Out=CS.
        %class_pair_common_subsumer_ext_chain(R,A,B,CSs,[C1,C2],CS,CS_Out).

% 3-way; not very generic
class_pair_common_subsumer_ext(R,A,B,CS_Out) :-
        class_pair_common_subsumers_with_union(R,A,B,CSs),
        select(C1,CSs,CSs_r1),         % eg r1 some (r2 some a)
        select(C2,CSs_r1,CSs_r2),      % eg r1 some (r2 some b)
        member(C3,CSs_r2),                % eg r1 some (r2 some b)
        C1 @< C2, % arbitrary direction
        C2 @< C3,
        debug(owlsim_detail,'   candidate 3-way intersection: ~w ^ ~w ^ ~w',[C1,C2,C3]),
        \+ subsumes_or_subsumed_by(R,C1,C2),
        \+ subsumes_or_subsumed_by(R,C1,C3),
        \+ subsumes_or_subsumed_by(R,C2,C3),
        % now make r1 some (r2 some a and b)
        combine_expr_pair(C1,C2,CS_x),
        combine_expr_pair(CS_x,C3,CS),
        debug(owlsim_detail,'   candidate 3-way CS: ~w',[CS]),
        is_subsumed_by_chk(R,A,CS),
        is_subsumed_by_chk(R,B,CS),
        CS_Out=CS.
        %class_pair_common_subsumer_ext_chain(R,A,B,CSs,[C1,C2],CS,CS_Out).


% DOES NOT WORK YET
class_pair_common_subsumer_ext_chain(R,A,B,CSs,Used,CS_In,CS_Out) :-
        member(C3,CSs),
        \+ member(C3,Used),
        \+ subsumes_or_subsumed_by(R,C3,CS_In),
        combine_expr_pair(C3,CS_In,CS_Next),
        is_subsumed_by_chk(R,A,CS_Next),
        is_subsumed_by_chk(R,B,CS_Next),
        class_pair_common_subsumer_ext_chain(R,A,B,CSs,[C3|Used],CS_Next,CS_Out).
class_pair_common_subsumer_ext_chain(_,_,_,_,_,CS,CS).

        

%% class_pair_least_common_subsumer_ext(+Reasoner,+ClassA,+ClassB,?CommonSubsumerExpression)
class_pair_least_common_subsumer_ext(R,A,B,CS) :-
        setof(CS,class_pair_common_subsumer_ext(R,A,B,CS),CS_Set),
        member(CS,CS_Set),
        debug(owlsim_detail,'   candidate LCS: ~w',[CS]),
        \+ ((member(X,CS_Set),
             \+ is_equivalent(R,X,CS),
             is_subsumed_by_chk(R,X,CS),
             debug(foo,'  fail: is_subsumed_by_chk(~w,~q,~q).',[R,X,CS]))).



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

/*
combine_expr_pair(C1x,C2x,CE) :- combine_as_union(C1x,C2x,CE).
combine_expr_pair(C1x,C2x,CE) :- combine_as_union(C2x,C1x,CE).


combine_as_union(C1x,C2,unionOf(C1x,C2)) :-
        C1x = someValuesFrom(R,C2).
*/



% ----------------------------------------
% REASONING
% ----------------------------------------
% may be partially redundant with reasoner modules.
% however, we may want to use cached reasoner results,
% and we may have novel class expressions

is_equivalent(_,C,C) :- !.
is_equivalent(R,C1,C2) :-
        is_subsumed_by_chk(R,C1,C2),
        is_subsumed_by_chk(R,C2,C1).
subsumes_or_subsumed_by(R,C1,C2) :-       is_subsumed_by_chk(R,C1,C2).
subsumes_or_subsumed_by(R,C1,C2) :-       is_subsumed_by_chk(R,C2,C1).


is_subsumed_by_chk(R,X,Y) :-
        !,
        is_subsumed_by(R,X,Y).

is_subsumed_by(_,X,X).
is_subsumed_by(R,A,unionOf(L)) :-
        member(X,L),
        is_subsumed_by(R,A,X).
is_subsumed_by(R,unionOf(L),B) :-
        forall(member(X,L),
               is_subsumed_by(R,X,B)).
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


        

        
