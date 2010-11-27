/* -*- Mode: Prolog -*- */

:- module(owl2_dotty,
          [
           visualize_obj/1,
           visualize_obj/2,
           visualize_objs/1,
           visualize_objs/2,
           visualize_all/0
           ]).

:- use_module(owl2_model).
:- use_module(util/dot).

:- multifile user:parse_arg_hook/3.
user:parse_arg_hook(['--display-object',Ob|L],L,goal(owl2_dotty:visualize_obj(Ob))).
user:parse_arg_hook(['--display-all-objects'|L],L,goal(owl2_dotty:visualize_all)).

visualize_obj(Name) :-
        visualize_obj(Name,[]).
%visualize_obj(Name,[follow(equivalentClasses)]).
visualize_obj(Name,Opts) :-
        resolve_obj(Name,Obj),
        obj_dotgraph(Obj,G,Opts),
        graph_display(G,open).

visualize_objs(Names) :-
        visualize_objs(Names,[]).
visualize_objs(Names,Opts) :-
        maplist(resolve_obj,Names,Objs),
        objlist_dotgraph(Objs,G,Opts),
        graph_display(G,open).

visualize_all :-
        setof(E,entity(E),Objs),
        visualize_objs(Objs).

resolve_obj(N,Obj) :- labelAnnotation_value(Obj,N),!.
resolve_obj(Obj,Obj).

entity_edge(X,Y,R,Opts) :- equivalent_to(X,Z),\+option(follow(equivalentClasses),Opts),atom(X),\+atom(Z),entity_edge(Z,Y,R,Opts).
entity_edge(X,Y,cr,_Opts) :- equivalent_to(X,intersectionOf(L)),atom(X),member(Y,L),class(Y).
entity_edge(X,Y,eq,Opts) :- equivalent_to(X,Y),option(follow(equivalentClasses),Opts).
%entity_edge(X,Y,R,Opts) :- equivalent_to(X,Z),entity_edge(Z,Y,R,Opts). % cycle
entity_edge(X,Y,subClassOf,_Opts) :- subClassOf(X,Y),class(Y).
entity_edge(X,Y,R,Opts) :- subClassOf(X,Z),\+class(Z),entity_edge(Z,Y,R,Opts).
entity_edge(someValuesFrom(R,Y),Y,R,_).
entity_edge(I,C,type,_) :- classAssertion(C,I).
entity_edge(I,J,R,_) :- propertyAssertion(R,I,J).
entity_edge(intersectionOf(L),X,cr,_Opts) :- member(X,L),class(X).
entity_edge(intersectionOf(L),Y,R,Opts) :- member(X,L),\+class(X),entity_edge(X,Y,R,Opts).
%entity_edge(intersectionOf(L),Y,R,Opts) :- member(X,L),entity_edge(X,Y,R,follow(equivalentClasses)).

entities_edges([InEdge|ScheduledEdges],Visisted,ResultEdges,FinalEdges,Opts) :-
        debug(dot,'edge: ~w',[InEdge]),
        InEdge = edge(_,Obj,_),
	findall(Edge,
                (   entity_edge(Obj,Parent,Conn,Opts),
                    Edge=edge(Obj,Parent,Conn),
                    \+ord_memberchk(Edge,Visisted)),
                NextEdges_1),
        sort(NextEdges_1,NextEdges),
        NextEdges \= [],
	!,
	ord_union(ResultEdges,NextEdges,ResultEdgesNew),
        ord_union(ScheduledEdges,NextEdges,NewScheduledEdges),
	entities_edges(NewScheduledEdges,[InEdge|Visisted],ResultEdgesNew,FinalEdges,Opts).
entities_edges([E|ScheduledEdges],Visisted,ResultEdges,FinalEdges,Opts) :-
	!,
        % Obj has no parents
	entities_edges(ScheduledEdges,[E|Visisted],ResultEdges,FinalEdges,Opts).
entities_edges([],_,ResultEdges,ResultEdges,_). % iterature until all scheduled nodes processed

objlist_dotgraph(Objs,G,Opts) :-
        findall(edge(s,Obj,null),
                member(Obj,Objs),
                Edges),
        entities_edges(Edges,[],[],OutEdges,Opts),
        owlgraph_dotgraph(OutEdges,G,Opts).
obj_dotgraph(Obj,G,Opts) :-
        entities_edges([edge(s,Obj,null)],[],[],OutEdges,Opts),
        owlgraph_dotgraph(OutEdges,G,Opts).

owlgraph_dotgraph(Edges,G,Opts) :-
        %maplist(writeln,Edges),
        findall(DE,(member(E,Edges),
                    edge_to_dotedge(E,DE,Opts)),
                DEs),
        findall(N,
                (   member(edge(N,_,_),Edges)
                ;   member(edge(_,N,_),Edges)),
                Ancs_1),
        sort(Ancs_1,Ancs),
        findall(N,
                (   member(A,Ancs),
                    obj_to_dotnode(A,N,Opts)),
                Nodes),
        append(Nodes,DEs,GTerms),
        GFlat=graph(g,[],GTerms),
        graph_nest(GFlat,G,[cr]),
        %G=GFlat,
        writeln(G).

edge_to_dotedge(edge(S,T,R),E2,Opts) :-
        safe(S,S2),
        safe(T,T2),
        edge_to_dotedge_1(edge(S2,T2,R),E2,Opts).

edge_to_dotedge_1(E,edge(S,T,Props),Opts) :-
        E=edge(S,T,_R),
        findall(P=V,edge_prop(E,P,V,Opts),Props).

edge_prop(edge(_,_,R),label,Label,_) :- labelAnnotation_value(R,Label).
edge_prop(edge(_,_,R),label,R,_) :- atom(R),\+labelAnnotation_value(R,_).

obj_to_dotnode(X,node(X2,Props),Opts) :-
        obj_to_dotnode_1(X,node(X,Props),Opts),
        safe(X,X2).

obj_to_dotnode_1(X,node(X,Props),Opts) :-
        findall(P=V,node_prop(X,P,V,Opts),Props).

node_prop(X,label,Label,_) :- labelAnnotation_value(X,Label).
node_prop(X,label,X,_) :- atom(X), \+ labelAnnotation_value(X,_).
node_prop(X,label,'',_) :- \+ atom(X).
node_prop(X,shape,box,_) :- class(X).
node_prop(X,shape,box,_) :- \+ atom(X).

safe(X,X) :- atom(X),!.
safe(X,Y) :- term_to_atom(X,Y).


