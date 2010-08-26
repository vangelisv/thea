/* -*- Mode: Prolog -*- */

:- module(owl2_graph_reasoner,
          [
           class_ancestor/2,
           class_ancestor_over/3
           ]).

:- use_module(owl2_model).
:- use_module(owl2_reasoner).

:- multifile owl2_reasoner:reasoner_ask_hook/2.
:- multifile owl2_reasoner:initialize_reasoner_hook/3.

owl2_reasoner:initialize_reasoner_hook(graph_reasoner,graph_reasoner,_).

owl2_reasoner:reasoner_ask_hook(graph_reasoner,subClassOf(A,B)) :-
	class_ancestor(A,B).

% ----------------------------------------
% DIRECT CONNECTION
% ----------------------------------------

class_parent_over(Class,Parent,sub) :-
        subClassOf(Class,Parent).
class_parent_over(Class,Parent,sub) :-
        equivalent_to(Class,Parent).
class_parent_over(someValuesFrom(Prop,Parent),Parent,some-Prop).
class_parent_over(allValuesFrom(Prop,Parent),Parent,all-Prop).
class_parent_over(intersectionOf(CL),Parent,sub) :-
        member(Parent,CL).

% ----------------------------------------
% COMBINATORIAL LOGIC
% ----------------------------------------

% we can collapse certain chains of connections

class_parent_chain(Class,Parent,InConns,NewConns) :-
        class_parent_over(Class,Parent,ConnNext),
        combine_props(InConns,ConnNext,NewConns).

% note that connection list maintained in reverse order
combine_props([ConnPrev|InConns],ConnNext,NewConns) :-
        combine_prop_pair(ConnPrev,ConnNext,NewConn),
        !,
        combine_props(InConns,NewConn,NewConns).
combine_props(InConns,ConnNext,[ConnNext|InConns]).

combine_prop_pair(sub,sub,sub).
combine_prop_pair(sub,Q-P,Q-P).
combine_prop_pair(Q-P,sub,Q-P).
combine_prop_pair(some-Prop,some-Prop,some-Prop) :-
        transitiveProperty(Prop).
combine_prop_pair(all-Prop,all-Prop,all-Prop) :-
        transitiveProperty(Prop).
combine_prop_pair(some-Prop1,some-Prop2,some-Prop3) :-
        subPropertyOf(propertyChain([Prop1,Prop2]),Prop3).
combine_prop_pair(all-Prop1,all-Prop2,all-Prop3) :-
        subPropertyOf(propertyChain([Prop1,Prop2]),Prop3).

% ----------------------------------------
% TRAVERSING UP GRAPH
% ----------------------------------------

class_ancestor(Class,ParentExpr) :-
        class_ancestor_over(Class,Parent,Conns),
        translate_conns_to_class_expression(Conns,Parent,ParentExpr).

translate_conns_to_class_expression([Conn|Conns],Parent,ParentExpr) :-
        translate_conn_to_class_expression(Conn,Parent,ParentExpr_1),
        translate_conns_to_class_expression(Conns,ParentExpr_1,ParentExpr).
translate_conns_to_class_expression([],P,P) :- !.

translate_conn_to_class_expression(sub,Parent,Parent) :- !.
translate_conn_to_class_expression(some-Prop,Parent,someValuesFrom(Prop,Parent)) :- !.
translate_conn_to_class_expression(all-Prop,Parent,allValuesFrom(Prop,Parent)) :- !.


class_ancestor_over(ID,PID,Over) :-
	class(ID),
	debug(graph_reasoner,'class_ancestor_over(~w)',[ID]),
	classes_ancestors([ID-[]],[],[],L),
	member(PID-Over,L).

%% classes_ancestors(+ScheduledCCPairs,+Visited,+AccumulatedResults,?FinalResults)
classes_ancestors([Class-Conns|ScheduledCCPairs],Visisted,ResultCCPairs,FinalCCPairs) :-
	setof(Parent-NewConns,
              (   class_parent_chain(Class,Parent,Conns,NewConns),
                  \+ord_memberchk(Parent,Visisted)),
              NextCCPairs),
	!,
	ord_union(ResultCCPairs,NextCCPairs,ResultCCPairsNew),
        ord_union(ScheduledCCPairs,NextCCPairs,NewScheduledCCPairs),
	classes_ancestors(NewScheduledCCPairs,[Class|Visisted],ResultCCPairsNew,FinalCCPairs).
classes_ancestors([Class-Conns|ScheduledCCPairs],Visisted,ResultCCPairs,FinalCCPairs) :-
	!,
        % Class has no parents
	classes_ancestors(ScheduledCCPairs,[Class-Conns|Visisted],ResultCCPairs,FinalCCPairs).
classes_ancestors([],_,ResultCCPairs,ResultCCPairs).
