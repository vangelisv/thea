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
owl2_reasoner:reasoner_ask_hook(graph_reasoner,classAssertion(C,I)) :-
	individual_ancestor(I,C).
owl2_reasoner:reasoner_ask_hook(graph_reasoner,individual_cs(I,J,CS)) :-
	individual_pair_common_subsumer(I,J,CS).



% ----------------------------------------
% DIRECT CONNECTION
% ----------------------------------------

entity_parent_over(Class,Parent,sub) :-
        subClassOf(Class,Parent).
entity_parent_over(Class,Parent,sub) :-
        equivalent_to(Class,Parent).
entity_parent_over(someValuesFrom(Prop,Parent),Parent,some-Prop).
entity_parent_over(allValuesFrom(Prop,Parent),Parent,all-Prop).
entity_parent_over(intersectionOf(CL),Parent,sub) :-
        member(Parent,CL).
entity_parent_over(Class,Parent,inst) :-
        classAssertion(Parent,Class).
entity_parent_over(Class,Parent,irel-Prop) :-
        propertyAssertion(Prop,Class,Parent).

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
combine_prop_pair(inst,sub,inst).
combine_prop_pair(sub,sub,sub).
combine_prop_pair(sub,Q-P,Q-P).
combine_prop_pair(Q-P,sub,Q-P).
combine_prop_pair(some-Prop,some-Prop,some-Prop) :-
        transitiveProperty(Prop).
combine_prop_pair(all-Prop,all-Prop,all-Prop) :-
        transitiveProperty(Prop).
combine_prop_pair(irel-Prop,irel-Prop,irel-Prop) :-
        transitiveProperty(Prop).
combine_prop_pair(some-Prop1,some-Prop2,some-Prop3) :-
        subPropertyOf(propertyChain([Prop1,Prop2]),Prop3).
combine_prop_pair(all-Prop1,all-Prop2,all-Prop3) :-
        subPropertyOf(propertyChain([Prop1,Prop2]),Prop3).
combine_prop_pair(irel-Prop,inst,some-Prop).


% ----------------------------------------
% COMBINATORIAL LOGIC
% ----------------------------------------

% we can collapse certain chains of connections

entity_parent_chain(Class,Parent,InConns,NewConns) :-
        entity_parent_over(Class,Parent,ConnNext),
        combine_props(InConns,ConnNext,NewConns).

% note that connection list maintained in reverse order
combine_props([ConnPrev|InConns],ConnNext,NewConns) :-
        combine_prop_pair(ConnPrev,ConnNext,NewConn),
        !,
        combine_props(InConns,NewConn,NewConns).
combine_props(InConns,ConnNext,[ConnNext|InConns]).


% ----------------------------------------
% TRAVERSING UP GRAPH
% ----------------------------------------

not_excluded(Parent) :- atom(Parent).
not_excluded(intersectionOf(_)).

class_ancestor(Class,ParentExpr) :-
        class_ancestor_over(Class,Parent,Conns),
        % we exclude class expressions here; there will be an alternate path to the named class
        % over different connections
        not_excluded(Parent),
        % build the class expression from the connections
        translate_conns_to_class_expression(Conns,Parent,ParentExpr).

% remembers, the head of the connection list will refer to the parent
translate_conns_to_class_expression([Conn|Conns],Parent,ParentExpr) :-
        translate_conn_to_class_expression(Conn,Parent,ParentExpr_1),
        translate_conns_to_class_expression(Conns,ParentExpr_1,ParentExpr).
translate_conns_to_class_expression([],P,P) :- !.

translate_conn_to_class_expression(inst,Parent,Parent) :- !.
translate_conn_to_class_expression(sub,Parent,Parent) :- !.
translate_conn_to_class_expression(some-Prop,Parent,someValuesFrom(Prop,Parent)) :- !.
translate_conn_to_class_expression(all-Prop,Parent,allValuesFrom(Prop,Parent)) :- !.


class_ancestor_over(ID,PID,Over) :-
	class(ID),
	debug(graph_reasoner,'class_ancestor_over(~w)',[ID]),
	entities_ancestors([ID-[]],[],[],L),
	member(PID-Over,L).

%% entities_ancestors(+ScheduledCCPairs,+Visited,+AccumulatedResults,?FinalResults)
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
entities_ancestors([],_,ResultCCPairs,ResultCCPairs).

% ----------------------------------------
% INDIVIDUALS
% ----------------------------------------

individual_ancestor_over(ID,PID,Over) :-
        namedIndividual(ID),
	debug(graph_reasoner,'individual_ancestor_over(~w)',[ID]),
	entities_ancestors([ID-[]],[],[],L),
	member(PID-Over,L).

individual_ancestor(Individual,ParentExpr) :-
        individual_ancestor_over(Individual,Parent,Conns),
        % we exclude individual expressions here; there will be an alternate path to the named individual
        % over different connections
        not_excluded(Parent),
        % build the individual expression from the connections
        translate_conns_to_class_expression(Conns,Parent,ParentExpr).

% ----------------------------------------
% LCS
% ----------------------------------------

individual_pair_common_subsumer(I,J,CS) :-
        individual_ancestor(I,CS),
        individual_ancestor(J,CS).
