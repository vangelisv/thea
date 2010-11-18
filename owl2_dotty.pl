/* -*- Mode: Prolog -*- */

:- module(owl2_dotty,
          [
           display_obj/1,
           display_obj/2,
           display_objs/1,
           display_objs/2,
           display_all/0
           ]).

:- use_module(owl2_model).
:- use_module(util/dot).

:- multifile user:parse_arg_hook/3.
user:parse_arg_hook(['--display-object',Ob|L],L,goal(owl2_dotty:display_obj(Ob))).
user:parse_arg_hook(['--display-all-objects'|L],L,goal(owl2_dotty:display_all)).

display_obj(Name) :-
        display_obj(Name,[]).
display_obj(Name,Opts) :-
        resolve_obj(Name,Obj),
        obj_dotgraph(Obj,G,Opts),
        graph_display(G,open).

display_objs(Names) :-
        display_objs(Names,[]).
display_objs(Names,Opts) :-
        maplist(resolve_obj,Names,Objs),
        objlist_dotgraph(Objs,G,Opts),
        graph_display(G,open).

display_all :-
        setof(E,entity(E),Objs),
        display_objs(Objs).

resolve_obj(N,Obj) :- labelAnnotation_value(Obj,N),!.
resolve_obj(Obj,Obj).


%entity_edge(X,Y,cr) :- equivalent_to(X,intersectionOf(L)),member(Y,L),class(Y).
%entity_edge(X,Y,R) :- equivalent_to(X,intersectionOf(L)),member(Z,L),\+class(Z),entity_edge(Z,Y,R).
entity_edge(X,Y,eq) :- equivalent_to(X,Y).
%entity_edge(X,Y,R) :- equivalent_to(X,Z),entity_edge(Z,Y,R). % cycle
entity_edge(X,Y,subClassOf) :- subClassOf(X,Y),class(Y).
entity_edge(X,Y,R) :- subClassOf(X,Z),\+class(Z),entity_edge(Z,Y,R).
entity_edge(someValuesFrom(R,Y),Y,R).
entity_edge(intersectionOf(L),X,cr) :- member(X,L),class(X).
entity_edge(intersectionOf(L),Y,R) :- member(X,L),\+class(X),entity_edge(X,Y,R).
%entity_edge(intersectionOf(L),Y,R) :- member(X,L),entity_edge(X,Y,R).

entities_edges([edge(_,Obj,_)|ScheduledEdges],Visisted,ResultEdges,FinalEdges,Opts) :-
	setof(edge(Obj,Parent,Conn),
              (   entity_edge(Obj,Parent,Conn),
                  \+ord_memberchk(Parent,Visisted)),
              NextEdges),
	!,
	ord_union(ResultEdges,NextEdges,ResultEdgesNew),
        ord_union(ScheduledEdges,NextEdges,NewScheduledEdges),
	entities_edges(NewScheduledEdges,[Obj|Visisted],ResultEdgesNew,FinalEdges,Opts).
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
        %graph_nest(GFlat,G,[cr]),
        G=GFlat,
        writeln(G).

edge_to_dotedge(edge(S,T,R),E2,Opts) :-
        safe(S,S2),
        safe(T,T2),
        edge_to_dotedge_1(edge(S2,T2,R),E2,Opts).

edge_to_dotedge_1(edge(S,T,R),edge(S,T,[label=R]),_).

obj_to_dotnode(X,node(X2,Props),Opts) :-
        obj_to_dotnode_1(X,node(X,Props),Opts),
        safe(X,X2).

obj_to_dotnode_1(X,node(X,Props),Opts) :-
        findall(P=V,node_prop(X,P,V,Opts),Props).

node_prop(X,label,Label,_) :- labelAnnotation_value(X,Label).
node_prop(X,label,X,_) :- atom(X), \+ labelAnnotation_value(X,Label).
node_prop(X,label,'',_) :- \+ atom(X).


safe(X,X) :- atom(X),!.
safe(X,Y) :- term_to_atom(X,Y).


