/* -*- Mode: Prolog -*- */

:- module(owl2_lcs,
          [
           prepare_optimizations/1,
           class_pair_common_subsumer/4,
           class_pair_least_common_subsumer/4,
           class_pair_common_subsumer_with_union/4,
           class_pair_common_subsumer_ext/4,
           class_pair_least_common_subsumer_ext/4,
           class_pair_least_common_subsumer_ext_combined/4
          ]).

:- use_module(owl2_model).
:- use_module(owl2_reasoner).
:- use_module(owl2_graph_reasoner). % force this for now

%% prepare_optimizations(Opts)
% uses blip tabling module to cache reasoner calls
% (replace with reasoner caching?)
prepare_optimizations(_) :-
        ensure_loaded(library(thea2/tabling)),
        table_pred(is_subsumed_by/3),
        table_pred(is_equivalent/3),
        table_pred(reasoner_get_subsumer/2),
        graph_reasoner_memoize.

% HARCODE ALERT!!
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
exclude(allValuesFrom('http://www.obofoundry.org/ro/ro.owl#has_proper_part',_)).
exclude(Class) :-
	atom(Class),
	sub_atom(Class,0,_,_,'http://www.ifomis.org').

exclude_tr(someValuesFrom(_,X)) :- exclude_tr(X).
exclude_tr(allValuesFrom(_,X)) :- exclude_tr(X).
exclude_tr(X) :- exclude(X).

%% reasoner_get_subsumer(+Class,?SuperClass)
% wrapper for reasoner_ask/2
reasoner_get_subsumer(C,P) :- 
        reasoner_ask(_,subClassOf(C,P)),
        \+ exclude_tr(P).

% TODO - consider renaming all preds class_pair ==> entity_pair (also works for individuals)

%% class_pair_common_subsumers(+Reasoner,+ClassA,+ClassB,?CommonSubsumers:set) is det
% CommonSubsumers is the set of classes and class expressions that subsume A and B
class_pair_common_subsumers(R,A,B,CSs) :-
        class_pair_common_subsumers(R,A,B,_,_,CSs).
class_pair_common_subsumers(R,A,B,APs,BPs,CSs) :-
        debug(owlsim,'finding cs(~w,~w) via ~w',[A,B,R]),
        setof(X,reasoner_get_subsumer(A,X),APs),
        setof(X,reasoner_get_subsumer(B,X),BPs),
        debug(owlsim_detail,'   finding intersection',[]),
        ord_intersection(APs,BPs,CSs).

%% class_pair_common_subsumer(+Reasoner,+ClassA,+ClassB,?CommonSubsumer) is nondet
% see class_pair_common_subsumer/4
class_pair_common_subsumer(R,A,B,CS) :-
        class_pair_common_subsumers(R,A,B,CSs),
        member(CS,CSs).

%% class_pair_least_common_subsumer(+Reasoner,+ClassA,+ClassB,?LeastCommonSubsumer) is nondet
% true if LCS subsumes A and B, and there is no more specific class X that also subsumes A and B
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

%% class_pair_common_subsumers_with_union(+Reasoner,+ClassA,+ClassB,?CommonSubsumers:set) is det
%
% generally not called directly - instead use class_pair_common_subsumer_ext/3, which uses this
%
% as class_pair_common_subsumers/4, but also includes union constructs in the set of expressions.
% this is only done in certain circumstances (we do not want "trivial" unions).
% currently, if the set of common subsumers include:
%  * X
%  * someValuesFrom(R,X)
% then the union of these will be returned in the set.
%
% the rationale is that these represent something in common.
%
% for example,
% ==
% LCS(pizza and has_part some jalapeno, habanero) = spicy_pepper or (has_part some spicy_pepper)
% ==
% if one person likes pizza with jalapeno and another person likes habanero then it seems
% reasonable to include "spicy pepper or has part some spicy pepper" in the set of things that
% these people like in common.
% (another way to handle this particular example is with a property chain, but we may not
%  have property chains defined in all situations)
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
mk_union(someValuesFrom(R,X),someValuesFrom(R,Y),someValuesFrom(R,U)) :- mk_union(X,Y,U).

% ----------------------------------------
% EXTENDED - experimental
% ----------------------------------------
% finds class expressions

%% class_pair_common_subsumer_ext(+Reasoner,+ClassA,+ClassB,?CommonSubsumerExpression)
%
% expression is either intersectionOf(...) | someValuesFrom(Prop,Expr) | unionOf(...)
%
% if both C1 and C2 are in the set of common subsumers for A and B,
% then C1^C2 is also a common subsumer. There may be other common subsumers
% that can be obtained by "threading" the class expressions together.
class_pair_common_subsumer_ext(R,A,B,CS_Out) :-
        % first enumerate standard common subsumers
        class_pair_common_subsumers_with_union(R,A,B,CSs),
        debug(owlsim_detail,'   union cs(~w, ~w) = ~w',[A,B,CSs]),

        % choose candidate pair of classes
        member(C1,CSs),         % eg r1 some (r2 some a)
        member(C2,CSs), % eg r1 some (r2 some b)
        C1 @< C2, % arbitrary direction
        debug(owlsim_detail,'   candidate intersection: ~w ^ ~w',[C1,C2]),

        \+ subsumes_or_subsumed_by(R,C1,C2),

        debug(owlsim_detail,'     NR - now try combining',[]),
        % now make r1 some (r2 some a and b)
        combine_expr_pair(C1,C2,CS),
        debug(owlsim_detail,'   candidate combined CS: ~w',[CS]),

        is_subsumed_by_chk(R,A,CS),
        is_subsumed_by_chk(R,B,CS),
        CS_Out=CS.
        %class_pair_common_subsumer_ext_chain(R,A,B,CSs,[C1,C2],CS,CS_Out).

/*
% 3-way; not very generic
xxxclass_pair_common_subsumer_ext(R,A,B,CS_Out) :-
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
        length(Used,NumUsed),
        NumUsed < 4,
        member(C3,CSs),
        \+ member(C3,Used),
        \+ subsumes_or_subsumed_by(R,C3,CS_In),
        (   NumUsed=3
        ->  trace
        ;   true),
        combine_expr_pair(C3,CS_In,CS_Next),
        is_subsumed_by_chk(R,A,CS_Next),
        is_subsumed_by_chk(R,B,CS_Next),
        class_pair_common_subsumer_ext_chain(R,A,B,CSs,[C3|Used],CS_Next,CS_Out).
class_pair_common_subsumer_ext_chain(_,_,_,_,_,CS,CS).
*/

%% class_pair_least_common_subsumer_ext(+Reasoner,+ClassA,+ClassB,?CommonSubsumerExpression)
class_pair_least_common_subsumer_ext(R,A,B,CS_Simple) :-
        setof(CS,class_pair_common_subsumer_ext(R,A,B,CS),CS_Set),
        debug(owlsim_detail,'   calculated set of extended subsumers.',[]),
        member(CS,CS_Set),
        debug(owlsim_detail,'   candidate LCS: ~w',[CS]),
        % todo - include equivsets? just exclude structurally identical?
        \+ ((member(X,CS_Set),
             \+ is_equivalent(R,X,CS),
             is_subsumed_by_chk(R,X,CS),
             debug(foo,'  fail: is_subsumed_by_chk(~w,~q,~q).',[R,X,CS]))),
        simplify_expr(CS,CS_Simple).


%% class_pair_least_common_subsumer_ext_combined(+Reasoner,+ClassA,+ClassB,?CommonSubsumerExpression)
%
% as class_pair_common_subsumer_ext/4, but combines all subsumers into a single
% intersectionOf expression
class_pair_least_common_subsumer_ext_combined(R,A,B,CS_Combined) :-
        setof(CS,class_pair_least_common_subsumer_ext(R,A,B,CS),CS_Set),
        normalize_expr(intersectionOf(CS_Set),CS_Combined).

%% normalize_expr(+CE,?CE_Norm)
%
% generated CEs may have redundant or inconsistent structure
% TODO: full CNF?
normalize_expr(intersectionOf([X]),Y) :-
        !,
        normalize_expr(X,Y).
normalize_expr(intersectionOf(L1),Y) :-
        % example: (R some (A and B)) and (R some A)
        %  ==> (R some (A and B))
        select(X1,L1,L2),
        select(X2,L2,L3),
        reasoner_get_subsumer(X1,X2),
        !,
        normalize_expr(intersectionOf([X1|L3]),Y).
normalize_expr(intersectionOf(OuterL),Y) :-
        setof(X,intersection_member(X,OuterL),Xs),
        Xs\=OuterL,
        !,
        normalize_expr(intersectionOf(Xs),Y).
normalize_expr(X,X).

% e.g. X=car L=[..., (.. and X and ..), ...]
intersection_member(X,L) :-
        member(E,L),
        E=intersectionOf(IL),
        member(X,IL).
intersection_member(E,L) :-
        member(E,L),
        E\=intersectionOf(_).

simplify_expr(C,C) :- atom(C),!.
simplify_expr(CE,C) :- equivalent_to(CE,C),atom(C),!.
simplify_expr(CE,CE2) :-
        CE =.. [F|Args],
        Args\=[],
        !,
        maplist(simplify_expr,Args,Args2),
        CE2 =.. [F|Args2].
simplify_expr(C,C).


%% combine_expr_pair(+CE1,+CE2,?CE_Subsumer)
%
% takes two linear chain expressions C1, C2 and generates class expressions
% that are the superclass of both C1 and C2.
%
% this includes not only the trivial intersectionOf expression "C1 and C2" but also
% non-trivial expressions obtained by "weaving" C1 and C2 together
%
% for example, imagine comparing people based on which movie directors they like.
% we have 3 object properties:
% * likes - fan to director
% * directs - director to film
% * about - film to subject matter
%
% e.g. likes some (directs some (about some foo)) +
%      likes some (directs some (about some bar))
%  =>  1. likes some (directs some (about some foo and bar))
%      2. likes some (directs some about some foo) and (directs some about some bar)
%      3. (likes some directs some about some foo) and (likes some directs some about some bar)
%
% The 3rd expression is the trivial expression, but 1. is obtained by intersecting the innermost
% parts of the input expressions.
%
% note this only works for input expressions that are "linear chains" - these can be obtained
% by following any class up the hierarchy (see the owl2_graph_reasoner algorithm)
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

% particular checks are made for union, intersection and
% restrictions - these are not implemented in the owl2_graph_reasoner
% TODO: stratify these

is_equivalent(_,C,C) :- !.
is_equivalent(Rsnr,C1,C2) :-
        is_subsumed_by_chk(Rsnr,C1,C2),
        is_subsumed_by_chk(Rsnr,C2,C1).

subsumes_or_subsumed_by(Rsnr,C1,C2) :-       is_subsumed_by_chk(Rsnr,C1,C2).
subsumes_or_subsumed_by(Rsnr,C1,C2) :-       is_subsumed_by_chk(Rsnr,C2,C1).

%% is_subsumed_by_chk(+Reasoner,+ClassX,+ClassY) :- is semidet
% semideterministic version of is_subsumed_by/3
is_subsumed_by_chk(Rsnr,X,Y) :-
        !,
        is_subsumed_by(Rsnr,X,Y).

is_subsumed_by(_,X,X).
is_subsumed_by(Rsnr,X,Y) :-
        atom(X),
        equivalent_to(X,Expr),
        \+ atom(Expr), % avoid cycles - rewrite named classes as expressions only
        is_subsumed_by(Rsnr,Expr,Y).
is_subsumed_by(Rsnr,A,unionOf(L)) :-
        member(X,L),
        is_subsumed_by(Rsnr,A,X).
is_subsumed_by(Rsnr,unionOf(L),B) :- % todo (A or B) < (A or B or C)
        forall(member(X,L),
               is_subsumed_by(Rsnr,X,B)).
is_subsumed_by(Rsnr,A,intersectionOf(L)) :-
        forall(member(X,L),
               is_subsumed_by(Rsnr,A,X)).
is_subsumed_by(Rsnr,intersectionOf(L),B) :-
        member(X,L),
        is_subsumed_by(Rsnr,X,B).
is_subsumed_by(Rsnr,someValuesFrom(P,X),someValuesFrom(P,Y)) :-
        is_subsumed_by(Rsnr,X,Y).
is_subsumed_by(Rsnr,someValuesFrom(P,X),someValuesFrom(P,Y)) :-
        transitiveProperty(P),
        is_subsumed_by(Rsnr,X,someValuesFrom(P,Y)).
is_subsumed_by(Rsnr,A,X) :-
        reasoner_ask(Rsnr,subClassOf(A,X1)),
        X1\=A, % non-reflexive
        X=X1.




        
