:- module(owl2_instance_util,
          [
           graph_to_class_expressions/2,
           graph_to_class_expressions/3,
           invert_graph/2,
           instance_graph/3,
           instance_graph_export/4
           ]).

:- use_module(owl2_model).
:- use_module(library(option)).

%:- record atts(max_depth:integer=3, property_list:list).

%% instance_graph(+Individual,?Edges:list,+Opts) is det
%
%    Given an individual, find all edges extending from that
%    individual.
%
%    Each edge is of form edge(PropertyExpression,In,Out), where
%    PropertyExpression can be a named property or inverseOf(Prop).
%
%    Options:
%    * max_depth(MaxD:int) - default is 3
%    * property_list(Props:list) - defaults to unground/all properties
%
instance_graph(XL,G,Opts) :-
        is_list(XL),
        !,
        findall(1-X,member(X,XL),Nodes),
        instance_graph_extend(Nodes,[],[],G,Opts).
instance_graph(X,G,Opts) :-
        \+ is_list(X),
        !,
        Nodes=[1-X],
        instance_graph_extend(Nodes,[],[],G,Opts).

%% instance_graph_extend(+Nodes:list,+VisitedL:list,+EdgesIn:list,?Edges:list,+Opts)  is det
% deterministically extend a list of edges
instance_graph_extend([],_,E,E,_Opts) :- !.
instance_graph_extend([D-_|Nodes],VisitedL,EdgesIn,EdgesOut,Opts) :-
        option(max_depth(MaxD),Opts,3),
        D>MaxD,
        !,
        instance_graph_extend(Nodes,VisitedL,EdgesIn,EdgesOut,Opts).

instance_graph_extend([Node|Nodes],VisitedL,EdgesIn,EdgesOut,Opts) :-
        Node = D-X,
        Dplus1 is D+1,
        solutions(Edge,extend_node(X,Edge,Opts),NextEdges),
        solutions(Dplus1-Y,
                  (   member(edge(_,X,Y),NextEdges),
                      \+ member(Y,VisitedL)),
                  NextNodes),
        append(NextNodes,Nodes,Nodes2),
        append(NextEdges,EdgesIn,NewEdges),
        instance_graph_extend(Nodes2,[X|VisitedL],NewEdges,EdgesOut,Opts).

extend_node(X,edge(P,X,Y),Opts) :-
        propertyAssertion(P,X,Y),
        option(property_list(Props),Opts,_),
        memberchk(P,Props).

% inverse assertion
extend_node(X,edge(PI,X,Y),Opts) :-
        propertyAssertion(P,Y,X),
        option(property_list(Props),Opts,_),
        memberchk(P,Props),
        (   inverseProperties(P,PI)
        ->  true
        ;   PI=inverseOf(P)).

invert_graph([],[]).
invert_graph([edge(P,X,Y)|Edges],[edge(PI,Y,X)|Edges2]) :-
        inverseProperties(P,PI),
        !,
        invert_graph(Edges,Edges2).
invert_graph([_|Edges],Edges2) :-
        invert_graph(Edges,Edges2).

%all_chains(L) :-

%% edge_to_class_expression(+Edge,?ClassExpression)
edge_to_class_expression(edge(inverseOf(P),X,Y),CE) :-
        inverseProperties(P,PI),
        edge_to_class_expression(edge(PI,Y,X),CE).

edge_to_class_expression(edge(P,X,Y),intersectionOf([XC,someValuesFrom(P,YC)])) :-
        atom(P),
        classAssertion(XC,X),
        classAssertion(YC,Y).

edge_chain_to_class_expression([Edge],CE) :-
        !,
        edge_to_class_expression(Edge,CE).
edge_chain_to_class_expression([Edge|Edges],intersectionOf([XC,someValuesFrom(P,CE)])) :-
        Edge=edge(P,X,_),
        classAssertion(XC,X),
        edge_chain_to_class_expression(Edges,CE).
        
graph_to_class_expressions(Edges,CEs) :-
        solutions(CE,
                  (   member(Edge,Edges),
                      edge_to_class_expression(Edge,CE)),
                  CEs).

graph_to_class_expressions(Edges,CEs,MaxLen) :-
        solutions(CE,
                  (   between(1,MaxLen,Len),
                      edge_chain(Edges,Chain,Len),
                      edge_chain_to_class_expression(Chain,CE),
                      debug(xp,'chain: ~w ~w ~w',[Len,Chain,CE])),
                  CEs).

edge_chain(Edges,EdgeChain,MaxLen) :-
        select(Edge,Edges,EdgesRest),
        edge_chain([Edge],EdgesRest,EdgeChain,MaxLen).

edge_chain(EdgeChain,_,EdgeChain,MaxLen) :-
        length(EdgeChain,Len),
        Len >= MaxLen,
        !.
        
edge_chain(EdgeChain,Edges,ECOut,MaxLen) :-
        EdgeChain = [edge(P,X,Y)|EdgeChainRest],
        select(edge(P2,W,X),Edges,EdgesRest),
        \+ member(edge(_,_,W),EdgeChainRest),
        edge_chain([edge(P2,W,X),edge(P,X,Y)|EdgeChainRest],EdgesRest,ECOut,MaxLen).

%% solutions(+Template,+Goal,-Set)
%   deterministic setof (will not fail). Goal is existentially quantified
solutions(X,Goal,Xs):-
        (   setof(X,Goal^Goal,Xs)
        ->  true
        ;   Xs=[]).

inst_dotpropval(X,label=Label) :- labelAnnotation_value(X,Label),atom(Label).
inst_dotpropval(_,font='Verdana').

prop_dotpropval(X,label=Label) :- property_dotlabel(X,Label).
prop_dotpropval(_,font='Verdana').
prop_dotpropval(_,weight=1).

property_dotlabel(inverseOf(P),Label) :-
        (   inverseProperties(P,PI);inverseProperties(PI,P)),
        !,
        property_dotlabel(PI,Label).
property_dotlabel(P,Label) :- labelAnnotation_value(P,Label),atom(Label),!.
property_dotlabel(P,P) :- atom(P),!.
property_dotlabel(P,PA) :- sformat(PA,'~w',[P]).

%% instance_graph_export(X,Fmt,File,Opts)
instance_graph_export(X,Fmt,File,Opts) :-
        instance_graph(X,G,Opts),
        graph_to_img_file(G,Fmt,File).


%% graph_to_img_file(+Edges:list,+Fmt,+File) is det
graph_to_img_file(G,Fmt,File) :-
        ensure_loaded(bio(dotwriter)), % requires blip
        solutions(X,(   member(edge(_,X,_),G)
                    ;   member(edge(_,_,X),G)),
                  Xs),
        findall(node(X,PVs),
                (   member(X,Xs),
                    findall(PV,inst_dotpropval(X,PV),PVs)),
                DNodes),
        findall(edge(X,Y,PVs),
                (   member(edge(Type,Y,X),G),
                    Type=inverseOf(_),
                    findall(PV,prop_dotpropval(Type,PV),PVs)),
                DEdges1),
        findall(edge(X,Y,PVs),
                (   member(edge(Type,X,Y),G),
                    Type\=inverseOf(_),
                    findall(PV,prop_dotpropval(Type,PV),PVs)),
                DEdges2),
        flatten([DNodes,DEdges1,DEdges2],Subterms),
        DotG=graph(my_graph,Subterms),
        dotwriter:graph_to_dot_file(DotG,Fmt,File).

        

