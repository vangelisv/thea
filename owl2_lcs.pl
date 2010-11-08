/* -*- Mode: Prolog -*- */

:- module(owl2_lcs,
          [
           prepare_optimizations/1,
           class_pair_common_subsumer/3,
           class_pair_common_subsumer/4,
           class_pair_least_common_subsumer/3,
           class_pair_least_common_subsumer/4,
           individual_neighborhood_expression/3,
           individual_msc/2,
           individual_msc/3,
           description_pivot/2,
           simple_lcs/5,
           class_pair_gmatch/3
          ]).

:- use_module(owl2_model).
:- use_module(owl2_reasoner).
:- use_module(owl2_graph_reasoner). % force this for now

%% prepare_optimizations(Opts)
% uses blip tabling module to cache reasoner calls
% (replace with reasoner caching?)
prepare_optimizations(_) :-
        ensure_loaded(library(thea2/util/memoization)),
        table_pred(is_subsumed_by/3),
        table_pred(reasoner_get_subsumer/3),
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
% wrapper for reasoner_ask/2 over subClassOf/2 
reasoner_get_subsumer(C,P) :- 
        reasoner_get_subsumer(C,P,[]).
reasoner_get_subsumer(C,P,Opts) :-
        opts_reasoner(Opts,R),
        reasoner_ask(R,subClassOf(C,P)),
        \+ exclude_tr(P).

opts_reasoner(Opts,R) :- option(reasoner(R),Opts,graph_reasoner),!.

% TODO - consider renaming all preds class_pair ==> entity_pair (also works for individuals)

%% class_pair_common_subsumers(+ClassA,+ClassB,?CommonSubsumers:set,+Opts:list) is det
% CommonSubsumers is the set of classes and class expressions that subsume A and B
class_pair_common_subsumers(A,B,CSs,Opts) :-
        class_pair_common_subsumers(A,B,_,_,CSs,Opts).
class_pair_common_subsumers(A,B,APs,BPs,CSs,Opts) :-
        debug(owlsim,'finding cs(~w,~w) via ~w',[A,B,Opts]),
        setof(X,reasoner_get_subsumer(A,X,Opts),APs),
        setof(X,reasoner_get_subsumer(B,X,Opts),BPs),
        debug(owlsim_detail,'   finding intersection',[]),
        ord_intersection(APs,BPs,CSs).

%% class_pair_common_subsumer(+ClassA,+ClassB,?CommonSubsumer) is nondet
% see class_pair_common_subsumers/4
class_pair_common_subsumer(A,B,CS) :-
        class_pair_common_subsumer(A,B,CS,[]).

%% class_pair_common_subsumer(+ClassA,+ClassB,?CommonSubsumer,+Opts:list) is nondet
% see class_pair_common_subsumers/4
class_pair_common_subsumer(A,B,CS,Opts) :-
        class_pair_common_subsumers(A,B,CSs,Opts),
        member(CS,CSs).

%% class_pair_least_common_subsumer(+ClassA,+ClassB,?LeastCommonSubsumer) is nondet
% see class_pair_least_common_subsumer/4
class_pair_least_common_subsumer(A,B,LCS) :-
        class_pair_least_common_subsumer(A,B,LCS,[]).

%% class_pair_least_common_subsumer(+ClassA,+ClassB,?LeastCommonSubsumer,+Opts:list) is nondet
% true if LCS subsumes A and B, and there is no more specific class X that also subsumes A and B
class_pair_least_common_subsumer(A,B,LCS,Opts) :-
        member(basic(true),Opts),
        !,
        class_pair_least_common_subsumer_basic(A,B,LCS,Opts).
class_pair_least_common_subsumer(A,B,LCS,Opts) :-
        % default - use ext_combined algorithm
        class_pair_least_common_subsumer_ext_combined(A,B,LCS,Opts).

class_pair_least_common_subsumer_basic(A,B,LCS,Opts) :-
        class_pair_common_subsumers(A,B,CSs,Opts),
        member(LCS,CSs),
        opts_reasoner(Opts,R),
        % TODO - equivalence
        \+ ((member(X,CSs),
             X\=LCS,
             reasoner_ask(R,subClassOf(X,LCS)))).

% ----------------------------------------
% UNION
% ----------------------------------------

%% class_pair_common_subsumers_with_union(+ClassA,+ClassB,?CommonSubsumers:set,+Opts:list) is det
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
class_pair_common_subsumers_with_union(A,B,CSs,Opts) :-
        setof(CS,class_pair_common_subsumer_with_union(A,B,CS,Opts),CSs).

class_pair_common_subsumer_with_union(A,B,CS,Opts) :-
        debug(owlsim_detail,'finding cs+u(~w,~w)',[A,B]),
        class_pair_common_subsumers(A,B,APs,BPs,_,Opts),
        member(AP,APs),
        member(BP,BPs),
        mk_union(AP,BP,CS).
class_pair_common_subsumer_with_union(A,B,CS,Opts) :-
        class_pair_common_subsumer(A,B,CS,Opts).

mk_union(X,someValuesFrom(R,X),unionOf([X,someValuesFrom(R,X)])).
mk_union(someValuesFrom(R,X),X,unionOf([X,someValuesFrom(R,X)])).
mk_union(someValuesFrom(R,X),someValuesFrom(R,Y),someValuesFrom(R,U)) :- mk_union(X,Y,U).

% ----------------------------------------
% EXTENDED - experimental
% ----------------------------------------
% finds class expressions

%% class_pair_common_subsumer_ext(+ClassA,+ClassB,?CommonSubsumerExpression,Opts)
%
% expression is either intersectionOf(...) | someValuesFrom(Prop,Expr) | unionOf(...)
%
% if both C1 and C2 are in the set of common subsumers for A and B,
% then C1^C2 is also a common subsumer. There may be other common subsumers
% that can be obtained by "threading" the class expressions together.
class_pair_common_subsumer_ext(A,B,CS_Out,Opts) :-
        % first enumerate standard common subsumers
        class_pair_common_subsumers_with_union(A,B,CSs,Opts),
        debug(owlsim_detail,'   union cs(~w, ~w) = ~w',[A,B,CSs]),

        % choose candidate pair of classes
        member(C1,CSs),         % eg r1 some (r2 some a)
        member(C2,CSs), % eg r1 some (r2 some b)
        C1 @< C2, % arbitrary direction
        debug(owlsim_detail,'   candidate intersection: ~w ^ ~w',[C1,C2]),

        \+ subsumes_or_subsumed_by(C1,C2,Opts),

        debug(owlsim_detail,'     NR - now try combining',[]),
        % now make r1 some (r2 some a and b)
        combine_expr_pair(C1,C2,CS,Opts),
        debug(owlsim_detail,'   candidate combined CS: ~w',[CS]),

        is_subsumed_by_chk(A,CS,Opts),
        is_subsumed_by_chk(B,CS,Opts),
        CS_Out=CS.
        %class_pair_common_subsumer_ext_chain(A,B,CSs,[C1,C2],CS,CS_Out,Opts).

/*
% 3-way; not very generic
xxxclass_pair_common_subsumer_ext(A,B,CS_Out,Opts) :-
        class_pair_common_subsumers_with_union(A,B,CSs,Opts),
        select(C1,CSs,CSs_r1),         % eg r1 some (r2 some a)
        select(C2,CSs_r1,CSs_r2),      % eg r1 some (r2 some b)
        member(C3,CSs_r2),                % eg r1 some (r2 some b)
        C1 @< C2, % arbitrary direction
        C2 @< C3,
        debug(owlsim_detail,'   candidate 3-way intersection: ~w ^ ~w ^ ~w',[C1,C2,C3]),
        \+ subsumes_or_subsumed_by(C1,C2,Opts),
        \+ subsumes_or_subsumed_by(C1,C3,Opts),
        \+ subsumes_or_subsumed_by(C2,C3,Opts),
        % now make r1 some (r2 some a and b)
        combine_expr_pair(C1,C2,CS_x,Opts),
        combine_expr_pair(CS_x,C3,CS,Opts),
        debug(owlsim_detail,'   candidate 3-way CS: ~w',[CS]),
        is_subsumed_by_chk(A,CS,Opts),
        is_subsumed_by_chk(B,CS,Opts),
        CS_Out=CS.
        %class_pair_common_subsumer_ext_chain(A,B,CSs,[C1,C2],CS,CS_Out,Opts).


% DOES NOT WORK YET
class_pair_common_subsumer_ext_chain(A,B,CSs,Used,CS_In,CS_Out,Opts) :-
        length(Used,NumUsed),
        NumUsed < 4,
        member(C3,CSs),
        \+ member(C3,Used),
        \+ subsumes_or_subsumed_by(C3,CS_In),
        (   NumUsed=3
        ->  trace
        ;   true),
        combine_expr_pair(C3,CS_In,CS_Next,Opts),
        is_subsumed_by_chk(A,CS_Next,Opts),
        is_subsumed_by_chk(B,CS_Next,Opts),
        class_pair_common_subsumer_ext_chain(A,B,CSs,[C3|Used],CS_Next,CS_Out,Opts).
class_pair_common_subsumer_ext_chain(_,_,_,_,_,CS,CS,Opts).
*/

%% class_pair_least_common_subsumer_ext(+ClassA,+ClassB,?CommonSubsumerExpression,Opts)
class_pair_least_common_subsumer_ext(A,B,CS_Simple,Opts) :-
        setof(CS,class_pair_common_subsumer_ext(A,B,CS,Opts),CS_Set), % CONSIDER MEMOIZING THIS?
        debug(owlsim_detail,'   calculated set of extended subsumers.',[]),
        member(CS,CS_Set),
        debug(owlsim_detail,'   candidate LCS: ~w',[CS]),
        % todo - include equivsets? just exclude structurally identical?
        \+ ((member(X,CS_Set),
             \+ is_equivalent(X,CS,Opts),
             is_subsumed_by_chk(X,CS,Opts),
             debug(foo,'  fail: is_subsumed_by_chk(~q,~q).',[X,CS]))),
        simplify_expr(CS,CS_Simple).


%% class_pair_least_common_subsumer_ext_combined(+ClassA,+ClassB,?CommonSubsumerExpression,Opts)
%
% as class_pair_common_subsumer_ext/4, but combines all subsumers into a single
% intersectionOf expression
class_pair_least_common_subsumer_ext_combined(A,B,CS_Combined,Opts) :-
        setof(CS,class_pair_least_common_subsumer_ext(A,B,CS,Opts),CS_Set),
        normalize_expr(intersectionOf(CS_Set),CS_Combined,Opts).

%% normalize_expr(+CE,?CE_Norm,Opts)
%
% generated CEs may have redundant or inconsistent structure
% TODO: full CNF?
normalize_expr(intersectionOf([X]),Y,Opts) :-
        !,
        normalize_expr(X,Y,Opts).
normalize_expr(intersectionOf(L1),Y,Opts) :-
        % example: (R some (A and B)) and (R some A)
        %  ==> (R some (A and B))
        select(X1,L1,L2),
        select(X2,L2,L3),
        reasoner_get_subsumer(X1,X2,Opts),
        !,
        normalize_expr(intersectionOf([X1|L3]),Y,Opts).
normalize_expr(intersectionOf(OuterL),Y,Opts) :-
        setof(X,intersection_member(X,OuterL),Xs),
        Xs\=OuterL,
        !,
        normalize_expr(intersectionOf(Xs),Y,Opts).
normalize_expr(X,X,_Opts).

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


%% combine_expr_pair(+CE1,+CE2,?CE_Subsumer,Opts)
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
combine_expr_pair(C1,C2,intersectionOf([C1,C2]),_).
combine_expr_pair(C1x,C2x,someValuesFrom(R,CE) ,Opts) :-
        C1x=someValuesFrom(R,C1),
        C2x=someValuesFrom(R,C2),
        \+ subsumes_or_subsumed_by(C1,C2,Opts),
        combine_expr_pair(C1,C2,CE,Opts).

/*
combine_expr_pair(C1x,C2x,CE,Opts) :- combine_as_union(C1x,C2x,CE,Opts).
combine_expr_pair(C1x,C2x,CE,Opts) :- combine_as_union(C2x,C1x,CE,Opts).


combine_as_union(C1x,C2,unionOf(C1x,C2),Opts) :-
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

is_equivalent(C,C,_) :- !.
is_equivalent(C1,C2,Opts) :-
        is_subsumed_by_chk(C1,C2,Opts),
        is_subsumed_by_chk(C2,C1,Opts).

subsumes_or_subsumed_by(C1,C2,Opts) :-       is_subsumed_by_chk(C1,C2,Opts).
subsumes_or_subsumed_by(C1,C2,Opts) :-       is_subsumed_by_chk(C2,C1,Opts).

%% is_subsumed_by_chk(+ClassX,+ClassY,Opts) :- is semidet
% semideterministic version of is_subsumed_by/3
is_subsumed_by_chk(X,Y,Opts) :-
        !,
        is_subsumed_by(X,Y,Opts).

is_subsumed_by(X,X,_).
is_subsumed_by(X,Y,Opts) :-
        atom(X),
        equivalent_to(X,Expr),
        \+ atom(Expr), % avoid cycles - rewrite named classes as expressions only
        is_subsumed_by(Expr,Y,Opts).
is_subsumed_by(A,unionOf(L),Opts) :-
        member(X,L),
        is_subsumed_by(A,X,Opts).
is_subsumed_by(unionOf(L),B,Opts) :- % todo (A or B) < (A or B or C)
        forall(member(X,L),
               is_subsumed_by(X,B,Opts)).
is_subsumed_by(A,intersectionOf(L),Opts) :-
        forall(member(X,L),
               is_subsumed_by(A,X,Opts)).
is_subsumed_by(intersectionOf(L),B,Opts) :-
        member(X,L),
        is_subsumed_by(X,B,Opts).
is_subsumed_by(someValuesFrom(P,X),someValuesFrom(P,Y),Opts) :-
        is_subsumed_by(X,Y,Opts).
is_subsumed_by(someValuesFrom(P,X),someValuesFrom(P,Y),Opts) :-
        transitiveProperty(P),
        is_subsumed_by(X,someValuesFrom(P,Y),Opts).
is_subsumed_by(A,X,Opts) :-
        opts_reasoner(Opts,R),
        reasoner_ask(R,subClassOf(A,X1)),
        X1\=A, % non-reflexive
        X=X1.

% ----------------------------------------
% INSTANCE GRAPHS
% ----------------------------------------

individual_msc(Individual,ParentExpr) :-
        individual_msc(Individual,ParentExpr,[]).
individual_msc(Individual,ParentExpr,Opts) :-
        option(max_depth(MD),Opts,3),
        individual_neighborhood_expression(Individual,ParentExpr,MD,Opts).

individual_neighborhood_expression(ID,Expr,MaxDepth) :-
        individual_neighborhood_expression(ID,Expr,MaxDepth,[]).
individual_neighborhood_expression(ID,Expr,MaxDepth,Opts) :-
        setof(ID,is_individual(ID),IDs),
        member(ID,IDs),
	debug(mcs,'individual_nex(~w)',[ID]),
	individual_neighbor_graph([0/ID/Expr-Expr],[],MaxDepth,Opts).

individual_neighbor_graph([Depth/I/InnerExpr-_|ScheduledCCPairs],Visisted,MaxDepth,Opts) :-
        Depth < MaxDepth,
        classAssertion(C,I),
        debug(mcs,'C: ~w E: ~w',[ci(C,I),Expr]),
        DepthPlus1 is Depth+1,
	setof(Prop-Parent,
              (   individual_parent_over(I,Parent,Prop),
                  \+ exclude_entity(Parent,Opts),
                  \+ord_memberchk(Parent,Visisted)), % TODO; check for subpaths instead
              NextLinks),
	!,
        findall(DepthPlus1/Parent/PE-someValuesFrom(Prop,PE),member(Prop-Parent,NextLinks),PRPairs),
        prpairs_list(PRPairs,Restrictions),
        InnerExpr=intersectionOf([C|Restrictions]),
        debug(mcs,'    E: ~w',[Expr]),
        append(ScheduledCCPairs,PRPairs,NewScheduledCCPairs),
        debug(mcs,'    new: ~w',[NewScheduledCCPairs]),
	individual_neighbor_graph(NewScheduledCCPairs,[I|Visisted],MaxDepth,Opts).
individual_neighbor_graph([_/I/InnerExpr-_|ScheduledCCPairs],Visisted,MaxDepth,Opts) :-
	!,
        % I has no parents, or max depth is reached
        classAssertion(InnerExpr,I),
	individual_neighbor_graph(ScheduledCCPairs,[I|Visisted],MaxDepth,Opts).
individual_neighbor_graph([],_,_,_). % iterature until all scheduled nodes processed

prpairs_list([],[]).
prpairs_list([_-R|PL],[R|RL]) :-
        prpairs_list(PL,RL).

is_individual(ID) :-  namedIndividual(ID).
is_individual(ID) :-  classAssertion(_,ID).

individual_parent_over(Child,Parent,Prop) :-
        propertyAssertion(Prop,Child,Parent),
        \+ annotationProperty(Prop),
        Parent \= literal(_).
individual_parent_over(Child,Parent,InverseProp) :-
        propertyAssertion(Prop,Parent,Child),
        mk_inverse_prop(Prop,InverseProp),
        \+ annotationProperty(Prop),
        Parent \= literal(_).

mk_inverse_prop(Prop,InverseProp) :- inverseProperties(Prop,InverseProp),!.
mk_inverse_prop(Prop,InverseProp) :- inverseProperties(InverseProp,Prop),!.
mk_inverse_prop(Prop,inverseOf(Prop)).

exclude_entity(X,Opts) :-
        member(exclude_class(C),Opts),
        classAssertion(C,X).

% ----------------------------------------
% PIVOT
% ----------------------------------------

%% description_pivot(+InDesc,?OutDesc) is nondet
% 'rotate' a description around a separate pivot point.
% an owl description corresponds to a tree, we can re-root the tree at any node.
% Examples:
% ==
% R some X => [X, R' some thing]
% [A, R some X] => [X, R' some A]
% [A, B, R some X] = [X, R' some [A,B]]
% [A, Z, R some [B, S some X]] => [X, S' some [B, R' some [A,Z]]
% ==
description_pivot(In,Out) :-
        description_pivot(In,'owl:Thing',Out_1),
        remove_owl_thing(Out_1,Out).

%% description_pivot(+InDesc,+AccumDesc,?OutDesc) is nondet
%
% recursive descent: select an edge, invert it, point back to remaining
%  edges from node (this is Accum), traverse to target of selected edge,
%  passing Accum as argument
description_pivot(In,Accum,Out) :-
        d_select_edge(In,P,To,Rest),
        mk_inverse_prop(P,IP),
        d_mk_edge(IP,Accum,Rest,NewAccum),
        description_pivot(To,NewAccum,Out).

% base case: merge Accum (which points back up to remainder of graph)
% into current node
description_pivot(In,Accum,Out) :-
        d_intersect(Accum,In,Out).

%% d_select_edge(+Desc,?Prop,?Desc,?Remaining:list) is nondet
d_select_edge(intersectionOf(L),P,To,L2) :-
        select(someValuesFrom(P,To),L,L2).
d_select_edge(someValuesFrom(P,To),P,To,[]).

%% d_mk_edge(+Prop, +TgtDesc, +Descs:list, ?NewDesc)
d_mk_edge(Prop,TgtDesc,Descs,someValuesFrom(Prop,NewDesc) ) :-
        d_cons(TgtDesc,Descs,NewDesc).

% d_cons(+Desc, +DL:list, ?NewDesc)
d_cons(D,L,New) :-
        d_cons_1(D,L,NewL),
        (   NewL=[New]
        ->  true
        ;   NewL=[]
        ->  New='owl:Thing'
        ;   New=intersectionOf(NewL)).
d_cons_1('owl:Thing',L,L) :- !.
d_cons_1(intersectionOf(L1),L2,L3) :-
        !,
        append(L1,L2,L3).
d_cons_1(D,L,[D|L]).

d_intersect(A,B,intersectionOf([A,B])).

remove_owl_thing(intersectionOf(L),intersectionOf(L2)) :-
        select('owl:Thing',L,L2),
        !.
remove_owl_thing(X,X).

% ----------------------------------------
% SIMPLE LCS
% ----------------------------------------

simple_class_ancestor_over(X,A,RX) :-
        class_ancestor_over(X,A,RX),
        atom(A).

class_ancestors(X,AL) :-
        setof(A,R^simple_class_ancestor_over(X,A,R),AL).

simple_cs(X,Y,A,RX,RY) :-
        class_ancestors(X,XAL),
        class_ancestors(Y,YAL),
        ord_intersection(XAL,YAL,AL),
        member(A,AL),
        simple_class_ancestor_over(X,A,RX),
        simple_class_ancestor_over(Y,A,RY).

simple_lcs(X,Y,A) :-
        simple_lcs(X,Y,A,_,_).

simple_lcs(X,Y,A,RA) :-
        simple_lcs(X,Y,A,RX,RY),
        relation_union(RX,RY,RA).

simple_lcs(X,Y,A,RX,RY) :-
        simple_cs(X,Y,A,RX,RY),
        \+ ((simple_cs(X,Y,A2,RX2,RY2),
             A2-RX2-RY2 \= A-RX-RY,
             class_ancestor_over(A2,A,_))). % TODO

relation_union(R,R,R) :- !.
relation_union(RX,RY,or(RX,RY)) :- !.

simple_lcs_dist(X,Y,A,RA,D) :-
        simple_lcs(X,Y,A,RA),
        calc_lcs_dist(X,Y,A,D).

% TODO
calc_lcs_dist(A,A,A,0) :- !.
calc_lcs_dist(A,_,A,1) :- !.
calc_lcs_dist(_,A,A,1) :- !.
calc_lcs_dist(_,_,_,5).

/*
simple_lcs(X,Y,A,RX,RY) :-
        simple_cs(X,Y,A,RX,RY),
        \+ ((simple_cs(X,Y,A2,RX2,RY2),
             A2-RX2-RY2 \= A-RX-RY,
             class_ancestor_over(A2,A,R2),
             %writeln('**test'(A<A2,R2,RX2,RY2)),
             (   path_contains(RX2,R2)
             ;   path_contains(RY2,R2)))).

path_contains(R,R) :- !.
path_contains([],[_|_]) :- fail,!.
path_contains(_,[sub]) :- !.
path_contains(R,Sub) :- append(Sub,_,R),!.
*/        

% ----------------------------------------
% GMATCH
% ----------------------------------------

class_pair_gmatch(L1,L2,M) :-
        desc_edgeset(L1,S1),
        desc_edgeset(L2,S2),
        ord_intersection(S1,S2,M).

%% desc_edgeset(+Desc,?Edges:set) is semidet
% TODO: singletons
desc_edgeset(D,EL) :-
        setof(E,d_edge_tr(D,E),EL).

%% d_edge_tr(+Desc,?Edge) is nondet
% Edge is an edge in the description, or subpart of the description
d_edge_tr(SD,E) :-
        d_edge(SD,E,_).
d_edge_tr(SD,E) :-
        d_edge(SD,_,X),
        d_edge_tr(X,E).

d_edge(SD,e(S,T,R),TD) :-
        d_named_parent(SD,S),
        d_conn(SD,R,TD_1),
        d_extend(TD_1,T,TD).

% d_extent(+In,?NextObj,?NextDesc)
d_extend(A,B,A) :-
        d_named_parent(A,B),
        !.
d_extend(A,B,X) :-
        d_conn(A,_P,Z), % TODO
        d_extend(Z,B,X).

d_conn(someValuesFrom(Prop,Tgt),Prop,Tgt).
d_conn(intersectionOf(L),Prop,Tgt) :-
        member(X,L),
        d_conn(X,Prop,Tgt).
d_conn(D,Prop,Tgt) :-
        equivalent_to(D,intersectionOf(L)),
        member(X,L),
        d_conn(X,Prop,Tgt).


d_named_parent(D,P) :-
        d_named_parent(D,P,[]).

d_named_parent(intersectionOf(L),P,VL) :-
        !,
        member(X,L),
        d_named_parent(X,P,VL).
d_named_parent(D,P,VL) :-
        \+ member(D,VL),
        equivalent_to(D,EC),
        !,
        d_named_parent(EC,P,[D|VL]).
d_named_parent(D,D,_) :- atom(D).

edge_pair_subsumer_diff(E,E,E,0) :- !.
edge_pair_subsumer_diff(E1,E2,E3,Dist) :-
        E1=e(S1,T1,R1),
        E2=e(S2,T2,R2),
        E3=e(S3,T3,R3),
        simple_lcs_dist(S1,S2,S3,_RS3,DS),
        simple_lcs_dist(T1,T2,T3,_RT3,DT),
        relation_union(R1,R2,R3), % TODO
        Dist is DS+DT.

        
d_pair_matching_edges(D1,D2,ML1,ML2) :-
        desc_edgeset(D1,EL1),
        desc_edgeset(D2,EL2),
        e_pairs_scores(EL1,EL2,MEL),
        findall(m(E1,E2,E3,Diff),
                (   member(E1,EL1),
                    best_match1(E1,MEL,E2,E3,Diff)),
                ML1),
        findall(m(E1,E2,E3,Diff),
                (   member(E2,EL2),
                    best_match1(E2,MEL,E1,E3,Diff)),
                ML2).

compare_individuals(I1,I2,ML1,ML2) :-
        individual_msc(I1,D1),
        individual_msc(I2,D2),
        d_pair_matching_edges(D1,D2,ML1,ML2).

e_pairs_scores(EL1,EL2,MEL) :-
        setof(M,e_pairs_member_match(EL1,EL2,M),MEL).

e_pairs_member_match(L1,L2,m(E1,E2,E3,Diff)) :-
        member(E1,L1),
        member(E2,L2),
        debug(gm,'testing: ~w vs ~w',[E1,E2]),
        edge_pair_subsumer_diff(E1,E2,E3,Diff).

best_match1(E1,MEL,E2,E3,Diff) :-
        setof(Diff-m(E2,E3),
              member(m(E1,E2,E3,Diff),MEL),
              [Diff-m(E2,E3)|_]).
best_match2(E2,MEL,E1,E3,Diff) :-
        setof(Diff-m(E1,E3),
              member(m(E1,E2,E3,Diff),MEL),
              [Diff-m(E1,E3)|_]).



/*
d_parent(D,D).
d_parent(intersectionOf(L),P) :-
        member(X,L),
        d_parent(X,P).
d_parent(D,P) :-
        equivalent_to(D,intersectionOf(L)),
        member(X,L),
        d_parent(X,P).
*/

% ----------------------------------------
% GRAPHS
% ----------------------------------------

:- multifile user:parse_arg_hook/3.
user:parse_arg_hook(['--display-object',Ob|L],L,goal(owl2_lcs:display_object(Ob))) :-
        assume_entity_declarations.
user:parse_arg_hook(['--display-object-pair',X1,X2|L],L,goal(owl2_lcs:display_object_pair(X1,X2,[]))) :-
        assume_entity_declarations.

:- use_module(util/dot).

% TODO: Opts
edge_gterm(e(S,_,_),node(S,[label=N])) :- node_label(S,N).
edge_gterm(e(_,T,_),node(T,[label=N])) :- node_label(T,N).
edge_gterm(e(S,T,R),edge(S,T,[label=RL])) :- node_label(R,RL).


edge_to_gterm(e(S,T,invis),edge(S,T,[weight=100]),_) :- !.
edge_to_gterm(e(S,T,R),GT,Opts) :-
        GT=edge(S,T,[label=RL|Opts]),
        node_label(R,RL).
node_to_gterm(N,GT,Opts) :-
        GT=node(N,[label=NL|Opts]),
        node_label(N,NL).

node_label(N,NL) :- labelAnnotation_value(N,NL),!.
node_label(N,N) :- atom(N),!.
node_label(N,A) :- term_to_atom(N,A).



edges_to_gterms(EL,GTerms,Opts) :-
        findall(GT,(member(E,EL),
                    edge_to_gterm(E,GT,Opts)),
                GTerms).
nodes_to_gterms(NL,GTerms,Opts) :-
        findall(GT,(member(N,NL),
                    node_to_gterm(N,GT,Opts)),
                GTerms).


desc_gterm(D,graph(g,[],GTerms)) :-
        desc_edgeset(D,EL),
        findall(GTerm,
                (   member(E,EL),
                    edge_gterm(E,GTerm)),
                GTerms).

split_set(L1,L2,L3,L1_uniq,L2_uniq) :-
        ord_intersection(L1,L2,L3),
        ord_subtract(L1,L3,L1_uniq),
        ord_subtract(L2,L3,L2_uniq).


d_pair_gterm(D1,D2,G,Opts) :-
        desc_edgeset(D1,EL1),
        desc_edgeset(D2,EL2),
        append(EL1,EL2,EL_Union),
        split_set(EL1,EL2,EL_Intersection,EL1_Uniq,EL2_Uniq),
        edges_to_nodes(EL1,NL1),
        edges_to_nodes(EL2,NL2),
        append(NL1,NL2,NL_Union),
        fill_edges(NL_Union,EL_Union,EL_Ont,Opts),
        split_set(NL1,NL2,NL_Intersection,NL1_Uniq,NL2_Uniq),
        edges_to_gterms(EL1_Uniq,EGTerms1,[color=red]),
        edges_to_gterms(EL2_Uniq,EGTerms2,[color=blue]),
        edges_to_gterms(EL_Intersection,EGTerms_Intersection,[color=green,penwidth=5,weight=50]),
        edges_to_gterms(EL_Ont,EGTerms_Ont,[color=grey,style=dashed,weight=100]),
        nodes_to_gterms(NL1_Uniq,NGTerms1,[color=red]),
        nodes_to_gterms(NL2_Uniq,NGTerms2,[color=blue]),
        nodes_to_gterms(NL_Intersection,NGTerms_Intersection,[fillcolor=green,style=filled]),
        flatten([EGTerms1,EGTerms2,EGTerms_Intersection,EGTerms_Ont,
                 NGTerms1,NGTerms2,NGTerms_Intersection],GTerms),
        gterms_add_ontol_links(NL_Union,graph(g,[],GTerms),G).

gterms_add_ontol_links(Nodes,graph(GN,GProps,GTermsIn),GOut) :-
        findall(e(N,T,declaredIn),(member(N,Nodes),
                                   node_ont(N,T)),
                EL),
        setof(O,N^member(e(N,O,declaredIn),EL),Onts),
        nodes_to_gterms(Onts,NTerms,[]),
        edges_to_gterms(EL,ETerms,[]),
        flatten([NTerms,ETerms,GTermsIn],GTermsNew),
        graph_nest(graph(GN,GProps,GTermsNew),GOut,[declaredIn]).

node_ont(N,O) :- ontologyAxiom(O,class(N)),!.
node_ont(_,'x').




fill_edges(_,_,[],Opts) :-
        \+ member(fill_edges(true),Opts),
        !.
fill_edges(Nodes,Edges,NewEdgesNR,_) :-
        findall(e(S,T,R),
                (   member(S,Nodes),
                    member(T,Nodes),
                    S\=T,
                    \+ member(e(S,T,_),Edges),
                    \+ member(e(T,S,_),Edges),
                    class_ancestor_over(S,T,RL),
                    collapse_composite_edge_label(RL,R)),
                NewEdges_1),
        sort(NewEdges_1,NewEdges), % uniqify
        append(Edges,NewEdges,AllEdges),
        maplist(invert_edge,AllEdges,AllEdgesInv),
        append(AllEdges,AllEdgesInv,AllEdgesSymm),
        % TODO - more sophisticated redundancy checking
        findall(E,
                (   member(E,NewEdges),
                    E=e(S,T,R),
                    \+ ((member(e(S,Z,R),AllEdgesSymm),
                         member(e(Z,T,R),AllEdgesSymm)
                        ))),
                NewEdgesNR).

invert_edge(e(S,T,R),e(T,S,inverseOf(R))).


collapse_composite_edge_label(RL,RC) :-
        findall(Tok,(member(_-R,RL),sformat('~q',[R],Tok)),Toks),
        reverse(Toks,RToks),
        concat_atom(RToks,'->',RC).


/*
objs_gterm(OPairs,graph(g,[],GTerms)) :-
        desc_edgeset(D,EL),
        findall(GTerm,
                (   member(E,EL),
                    edge_gterm(E,GTerm)),
                GTerms).
*/

edge_to_node(e(S,_,_),S).
edge_to_node(e(_,T,_),T).

edges_to_nodes(EL,Nodes) :- setof(N,E^(member(E,EL),edge_to_node(E,N)),Nodes).


display_object(N) :-
        labelAnnotation_value(Ob,N),
        !,
        display_object(Ob).
display_object(Ob) :-       display_individual(Ob), !.
display_object(Ob) :-       display_desc(Ob), !.

object_gterm(I,G) :-
        individual_msc(I,D),
        !,
        desc_gterm(D,G).
object_gterm(D,G) :- desc_gterm(D,G).

display_individual(I) :-
        object_gterm(I,G),
        graph_display(G,open).

display_desc(D) :-
        object_gterm(D,G),
        graph_display(G,open).

obj_desc(X,D) :- individual_msc(X,D),!.
obj_desc(D,D2) :-
        % TEMP: this is for NDPO diseases
        setof(P,subClassOf(D,P),PL),
        !,
        D2=intersectionOf([D|PL]).
obj_desc(D,D).

display_object_pair(N1,N2,Opts) :-
        labelAnnotation_value(X1,N1),
        labelAnnotation_value(X2,N2),
        !,
        display_object_pair(X1,X2,Opts).
display_object_pair(X1,X2,Opts) :-
        obj_desc(X1,D1),
        obj_desc(X2,D2),
        d_pair_gterm(D1,D2,G,Opts),
        graph_display(G,open).







