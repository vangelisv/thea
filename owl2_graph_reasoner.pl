/* -*- Mode: Prolog -*- */

:- module(owl2_graph_reasoner,
          [
           entity_parent_over/3,
           class_ancestor/2,
           class_descendant/2,
           class_ancestor_over/3,
           individual_ancestor/2,
           individual_ancestor_over/3,
           graph_reasoner_memoize/0
           ]).

:- use_module(owl2_model).
:- use_module(owl2_reasoner).

% ----------------------------------------
% Hooks into owl2_reasoner
% ----------------------------------------

:- multifile owl2_reasoner:reasoner_ask_hook/2.
:- multifile owl2_reasoner:initialize_reasoner_hook/3.

:- multifile exclude_chain_hook/2.

owl2_reasoner:initialize_reasoner_hook(graph_reasoner,graph_reasoner,_).

% TODO: we can use more efficient procedures for finding subclasses between two named classes
owl2_reasoner:reasoner_ask_hook(graph_reasoner,subClassOf(A,B)) :-
        nonvar(B),
        var(A),
	class_descendant(B,A).
owl2_reasoner:reasoner_ask_hook(graph_reasoner,subClassOf(A,B)) :-
        \+((nonvar(B),
            var(A))),
	class_ancestor(A,B).
owl2_reasoner:reasoner_ask_hook(graph_reasoner,classAssertion(C,I)) :-
        nonvar(C),
        var(I),
	class_descendant_over(C,I,[inst]).
owl2_reasoner:reasoner_ask_hook(graph_reasoner,classAssertion(C,I)) :-
        \+ ((nonvar(C),
             var(I))),
        individual_ancestor(I,C).
owl2_reasoner:reasoner_ask_hook(graph_reasoner,propertyAssertion(P,I,J)) :-
        nonvar(J),
        var(I),
        individual_ancestor_over(I,J,[irel-P]). % TODO - desc
owl2_reasoner:reasoner_ask_hook(graph_reasoner,propertyAssertion(P,I,J)) :-
        \+ ((nonvar(J),
             var(I))),
        individual_ancestor_over(I,J,[irel-P]). % TODO - chain
owl2_reasoner:reasoner_ask_hook(graph_reasoner,individual_cs(I,J,CS)) :-
	individual_pair_common_subsumer(I,J,CS).

graph_reasoner_memoize :-
        ensure_loaded(library(thea2/util/memoization)),
        table_pred(class_descendant/2),
        table_pred(class_descendant_over/3),
        table_pred(class_ancestor/2),
        table_pred(class_ancestor_over/3),
        table_pred(individual_ancestor/2),
        table_pred(individual_ancestor_over/3),
        !.

% ----------------------------------------
% SubProperties
% ----------------------------------------
% use standard backward chaining
% assumes no cycles

subPropertyOfT(A,B) :- subPropertyOf(A,B).
subPropertyOfT(A,B) :- subPropertyOf(A,Z),subPropertyOfT(Z,B).

subPropertyOfRT(A,A) :- objectProperty(A).
subPropertyOfRT(A,B) :- subPropertyOfT(A,B).

property_composition(A,B,P) :-
        transitiveProperty(P),
        subPropertyOfRT(A,P),
        subPropertyOfRT(B,P).
property_composition(A,B,P) :-
        subPropertyOf(propertyChain([A1,B1]),P), % TODO - longer chains
        subPropertyOfRT(A,A1),
        subPropertyOfRT(B,B1).
calc_property_compositions :-
        findall(property_composition(A,B,C),
                assert(cached_property_composition(A,B,C))).


% ----------------------------------------
% DIRECT CONNECTION
% ----------------------------------------

entity_parent_over(Class,Parent,sub) :-
        subClassOf(Class,Parent).
entity_parent_over(Class,Parent,sub) :-
        equivalent_to(Class,Parent).
entity_parent_over(someValuesFrom(Prop,Parent),Parent,some-Prop).
entity_parent_over(allValuesFrom(Prop,Parent),Parent,all-Prop).
entity_parent_over(hasValue(Prop,Parent),Parent,value-Prop).
entity_parent_over(intersectionOf(CL),Parent,sub) :-
        ground(CL),
        member(Parent,CL).
entity_parent_over(I,C,inst) :-
        classAssertion(C,I).
entity_parent_over(Child,Parent,irel-Prop) :-
        propertyAssertion(Prop,Child,Parent),
        \+ annotationProperty(Prop),
        Parent \= literal(_).
entity_parent_over(Child,Parent,irel-Prop) :-
        propertyAssertion(InverseProp,Parent,Child),
        inverse_of_symm(InverseProp,Prop),
        \+ annotationProperty(Prop),
        Parent \= literal(_).

inverse_of_symm(Prop,InverseProp) :- inverseProperties(Prop,InverseProp).
inverse_of_symm(InverseProp,Prop) :- inverseProperties(Prop,InverseProp).

/*
% EXPERIMENTAL:
entity_parent_over(intersectionOf(L1),intersectionOf(L2),sub) :-
        maplist(subClassOf_or_same,L1,L2),
        L1\=L2.

  % not sufficient - need to traverse over someValuesFrom too..
subClassOf_or_same(X,X).
subClassOf_or_same(X,Y) :- subClassOf(X,Y).
subClassOf_or_same(someValuesFrom(Prop,X),someValuesFrom(Prop,Y))) :- subClassOf(X,Y).
*/

% TODO - subPropertyOf
combine_prop_pair(P,Q,_) :-
        exclude_chain_hook(P,Q), % TODO - this doesn't have desired effect, want to eliminate altogether
        !,
        fail.
combine_prop_pair(inst,sub,inst).
combine_prop_pair(sub,sub,sub).
combine_prop_pair(sub,Q-P,Q-P).
combine_prop_pair(Q-P,sub,Q-P).
combine_prop_pair(some-Prop,some-Prop,some-Prop) :-
        transitiveProperty(Prop).
combine_prop_pair(all-Prop,all-Prop,all-Prop) :-
        transitiveProperty(Prop).
%combine_prop_pair(irel-Prop,irel-Prop,irel-Prop) :-
%        transitiveProperty(Prop).
combine_prop_pair(some-Prop1,some-Prop2,some-Prop3) :-
        subPropertyOf(propertyChain([Prop1,Prop2]),Prop3).
combine_prop_pair(all-Prop1,all-Prop2,all-Prop3) :-
        subPropertyOf(propertyChain([Prop1,Prop2]),Prop3).
combine_prop_pair(irel-Prop1,irel-Prop2,irel-Prop3) :-
        property_composition(Prop1,Prop2,Prop3).
combine_prop_pair(irel-Prop,inst,some-Prop).


% ----------------------------------------
% COMBINATORIAL LOGIC
% ----------------------------------------

% we can collapse certain chains of connections

% UP
entity_parent_chain(Class,Parent,InConns,NewConns) :-
        entity_parent_over(Class,Parent,ConnNext),
        combine_props(InConns,ConnNext,NewConns).

% DOWN
entity_child_chain(Class,Child,InConns,NewConns) :-
        %debug(foo,'testing ~w',[Class]),
        entity_parent_over(Child,Class,ConnNext),
        ground(Child),          %  TODO - e.g. x = y and b, b ---> x
        %debug(foo,'  ~w < ~w',[Child,Class]),
        % TODO - inverse
        combine_props_rev(InConns,ConnNext,NewConns).

% use combine_prop_pair/2 to collapse an edge list
% (note that connection list maintained in reverse order)
combine_props([ConnPrev|InConns],ConnNext,NewConns) :-
        combine_prop_pair(ConnPrev,ConnNext,NewConn),
        !,
        combine_props(InConns,NewConn,NewConns).
combine_props([ConnPrev|_],ConnNext,_) :- % NEW, experimental
        exclude_chain_hook(ConnPrev,ConnNext),
        !,
        fail.
combine_props(InConns,ConnNext,[ConnNext|InConns]).

combine_props_rev([ConnPrev|InConns],ConnNext,NewConns) :-
        combine_prop_pair(ConnNext,ConnPrev,NewConn),
        !,
        combine_props_rev(InConns,NewConn,NewConns).
combine_props_rev(InConns,ConnNext,[ConnNext|InConns]).


% ----------------------------------------
% TRAVERSING UP GRAPH
% ----------------------------------------

not_excluded(Parent) :- atom(Parent).
not_excluded(intersectionOf(_)).
not_excluded(unionOf(_)).

%% class_ancestor(+Class,?ParentExpr)
% true if ParentExpr is an inferred superclass of Class.
% ParentExpr can be a named class or a linear class expression.
class_ancestor(Class,ParentExpr) :-
        class_ancestor_over(Class,Parent,Conns),
        % we exclude class expressions here; there will be an alternate path to the named class
        % over different connections
        not_excluded(Parent),
        % build the class expression from the connections
        translate_conns_to_class_expression(Conns,Parent,ParentExpr).

%% class_ancestor_over(+Class,?ParentClass,?Path)
% true if Path is a path between Class and ParentClass.
% Path is a list of Quantifier-Property pairs (ordered from Parent to Class)
% Path is reduced to the most compact form using OWL semantics.
class_ancestor_over(ID,PID,Over) :-
	class_or_expr(ID),
	debug(graph_reasoner,'class_ancestor_over(~w)',[ID]),
	entities_ancestors([ID-[]],[],[],L),
	member(PID-Over,L).
class_ancestor_over(ID,ID,[]) :-
        ground(ID).             % Reflexive
class_ancestor_over(ID,ID,[]) :-
        \+ ground(ID),             % Reflexive
        class(ID).

% an edge list can be trabslated to a "linear" class expression.
% e.g. [some-part_of,some-develops_from] X ==> part_of some develops_from some X
% remembers, the head of the connection list will refer to the parent
translate_conns_to_class_expression([Conn|Conns],Parent,ParentExpr) :-
        translate_conn_to_class_expression(Conn,Parent,ParentExpr_1),
        translate_conns_to_class_expression(Conns,ParentExpr_1,ParentExpr).
translate_conns_to_class_expression([],P,P) :- !.

translate_conn_to_class_expression(inst,Parent,Parent) :- !.
translate_conn_to_class_expression(sub,Parent,Parent) :- !.
translate_conn_to_class_expression(some-Prop,Parent,someValuesFrom(Prop,Parent)) :- !.
translate_conn_to_class_expression(irel-Prop,Parent,someValuesFrom(Prop,Parent)) :- !. %
translate_conn_to_class_expression(all-Prop,Parent,allValuesFrom(Prop,Parent)) :- !.
translate_conn_to_class_expression(value-Prop,Parent,hasValue(Prop,Parent)) :- !.

%% entities_ancestors(+ScheduledCCPairs:list, +Visited:list, +AccumulatedResults:list ,?FinalResults:list)
%
% internal. traverses up graph, maintaining a list of scheduled nodes. this list is processed one at
% a time, finding the parents of this node, and putting the resulting edge in the list of accumulated results,
% and adding the parents to the list of scheduled nodes.
entities_ancestors([Class-Conns|ScheduledCCPairs],Visisted,ResultCCPairs,FinalCCPairs) :-
	setof(Parent-NewConns,
              (   entity_parent_chain(Class,Parent,Conns,NewConns),
                  \+ord_memberchk(Parent,Visisted)),
              NextCCPairs),
	!,
	ord_union(ResultCCPairs,NextCCPairs,ResultCCPairsNew),
        ord_union(ScheduledCCPairs,NextCCPairs,NewScheduledCCPairs),
	entities_ancestors(NewScheduledCCPairs,[Class|Visisted],ResultCCPairsNew,FinalCCPairs).
entities_ancestors([Class-Conns|ScheduledCCPairs],Visisted,ResultCCPairs,FinalCCPairs) :-
	!,
        % Class has no parents
	entities_ancestors(ScheduledCCPairs,[Class-Conns|Visisted],ResultCCPairs,FinalCCPairs).
entities_ancestors([],_,ResultCCPairs,ResultCCPairs). % iterature until all scheduled nodes processed

%% class_descendant(+Class,?ChildExpr)
% true if ChildExpr is an inferred subclass of Class.
% currently both arguments must be named classes.
class_descendant(Class,ChildExpr) :-
        class_descendant_over(Class,ChildExpr,[sub]).

%% class_descendant_over(+Class,?ChildClass,?Path)
% true if Path is a path between ChildClass and Class
class_descendant_over(ID,CID,Over) :-
        class_or_expr(ID),
	debug(graph_reasoner,'class_descendant_over(~w)',[ID]),
	entities_descendants([ID-[]],[],[],L),
	member(CID-Over,L).
class_descendant_over(ID,ID,[]). % Reflexive


%% entities_descendants(+ScheduledCCPairs,+Visited,+AccumulatedResults,?FinalResults)
%
% internal. See entities_ancestors/4 for oppsite predicate.
entities_descendants([Class-Conns|ScheduledCCPairs],Visisted,ResultCCPairs,FinalCCPairs) :-
	setof(Child-NewConns,
              (   entity_child_chain(Class,Child,Conns,NewConns),
                  \+ord_memberchk(Child,Visisted)),
              NextCCPairs),
	!,
	ord_union(ResultCCPairs,NextCCPairs,ResultCCPairsNew),
        ord_union(ScheduledCCPairs,NextCCPairs,NewScheduledCCPairs),
	entities_descendants(NewScheduledCCPairs,[Class|Visisted],ResultCCPairsNew,FinalCCPairs).
entities_descendants([Class-Conns|ScheduledCCPairs],Visisted,ResultCCPairs,FinalCCPairs) :-
	!,
        % Class has no parents
	entities_descendants(ScheduledCCPairs,[Class-Conns|Visisted],ResultCCPairs,FinalCCPairs).
entities_descendants([],_,ResultCCPairs,ResultCCPairs).

% arg must be either ground class expr or class; if var then
% enumerate named classes. todo: insts?
class_or_expr(ID) :-
        \+ ground(ID),
        !,
        setof(ID,referenced_description(ID),IDs),
        member(ID,IDs).
class_or_expr(ID) :- ground(ID).

% ----------------------------------------
% INDIVIDUALS
% ----------------------------------------

is_individual(ID) :-  namedIndividual(ID).
is_individual(ID) :-  classAssertion(_,ID).


individual_ancestor_over(ID,PID,Over) :-
        setof(ID,is_individual(ID),IDs),
        member(ID,IDs),
	debug(graph_reasoner,'individual_ancestor_over(~w)',[ID]),
	entities_ancestors([ID-[]],[],[],L),
	member(PID-Over,L).

/*
individual_ancestor(Individual,ParentExpr) :-
        individual_ancestor_over(Individual,Parent,Conns),
        % we exclude individual expressions here; there will be an alternate path to the named individual
        % over different connections
        not_excluded(Parent),
        % build the individual expression from the connections
        translate_conns_to_class_expression(Conns,Parent,ParentExpr).
*/
individual_ancestor(Individual,ParentExpr) :-
        classAssertion(Class,Individual),
        class_ancestor(Class,ParentExpr).

% ----------------------------------------
% LCS
% ----------------------------------------

individual_pair_common_subsumer(I,J,CS) :-
        individual_ancestor(I,CS),
        individual_ancestor(J,CS).
