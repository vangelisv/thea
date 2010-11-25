/* -*- Mode: Prolog -*- */

:- module(dot,
          [
           graph_to_dot_atom/2,
           graph_to_dot_file/2,
           graph_to_dot_file/3,
           graph_nest/3,
           graph_nest_pairs/3,
           graph_display/2,
           graph_display/3
           ]).

% pldoc at end of file

%% solutions(+Template,+Goal,-Set)
%   deterministic setof (will not fail). Goal is existentially quantified
solutions(X,Goal,Xs):-
        (   setof(X,Goal^Goal,Xs)
        ->  true
        ;   Xs=[]).



% ----------------------------------------
% grammar for writing dotfiles
% ----------------------------------------
% designed for generation, but could be extended for parsing

open_t --> ['{'].
close_t --> ['}'].
opena --> ['['].
closea --> [']'].
nl --> ['\n'].
sc --> [';'].

dot(X) --> dot(X,0).

dot(graph(Name,Atts,Elts),0) --> !,[digraph],name(Name),open_t,nl,atts(Atts,1),nl,dot(Elts,1),close_t,nl.
dot(graph(Name,Atts,Elts),I) --> I>0,dot(subgraph(Name,Atts,Elts),I). % any nested graph is a subgraph
dot(subgraph(Name,Atts,Elts),I) --> !,{Iplus1 is I+1},tab(I),[subgraph],name(Name),open_t,nl,atts(Atts,Iplus1),dot(Elts,Iplus1),tab(I),close_t,nl.

dot(graph(Name,Elts),I) --> dot(graph(Name,[],Elts),I).
dot(subgraph(Name,Elts),I) --> dot(subgraph(Name,[],Elts),I).

dot([],_) --> [].
dot([H|T],I) --> dot(H,I),nl,dot(T,I).

dot(node(Name,Atts),I) --> tab(I),name(Name),opena,node_atts(Atts),closea,sc,nl.
dot(node(Name),I) --> dot(node(Name,[]),I).

dot(edge(A,B,Atts),I) --> tab(I),name(A),['->'],name(B),opena,node_atts(Atts),closea,sc,nl.
dot(edge(A,B),I) --> dot(edge(A,B,[]),I).

att(T=V) --> !,name(T),['='],quote(V).
att(A) --> att(A=true).

node_atts([]) --> [],!.
node_atts([A]) --> !,att(A).
node_atts([A|L]) --> !,att(A),[','],node_atts(L).
node_atts(X) --> att(X).  % allow singletons as bare elements

atts([],_) --> [],!.
atts([A|L],I) --> !,tab(I),att(A),[';'],nl,atts(L,I).
atts(X) --> att(X),[';']. % allow singletons as bare elements


tab(0) --> !,[].
tab(I) --> ['  '],{Iminus1 is I-1},tab(Iminus1).

quote(cluster(X)) --> !,name(cluster(X)).
quote(X) --> {concat_atom(['"',X,'"'],Q)},[Q].   % TODO

name(cluster(N)) --> !,{escape_id(N,X),atom_concat('cluster_',X,C)},[C].
name(N) --> {escape_id(N,X)},[X].


escape_id(ID,ID2):-
        compound(ID),
        !,
        ID=..L,
        maplist(escape_id,L,L2),
        concat_atom(L2,'__',ID2).
escape_id(ID,ID2):-
        atom_chars(ID,L),
        escape_chars(L,L2),
        atom_chars(ID2,L2).

escape_chars([],[]).
escape_chars([H|T],[H|T2]):-
        safe_char(H),
        !,
        escape_chars(T,T2).
escape_chars([_|T],['_','_'|T2]):-
        escape_chars(T,T2).

safe_char(C):- C @>= a, C @=< z,!.
safe_char(C):- C @>= 'A', C @=< 'Z',!.
safe_char(C):- C @>= '0', C @=< '9',!. % '0
safe_char('_').

% ----------------------------------------
% EXPORTED UTILITY PREDICATES
% ----------------------------------------

%% graph_to_dot_atom(+GraphTerm,?Atom)
% translate a graph to an atom in dot syntax
graph_to_dot_atom(G,A):-
        dot(G,Toks,[]),
        concat_atom(Toks,' ',A).

%% graph_to_dot_file(+GraphTerm,+File) is det
%
% translate Graph to dot and write to File
graph_to_dot_file(GraphTerm,File):-
        graph_to_dot_file(GraphTerm,dot,File).

%% graph_to_dot_file(+GraphTerm,+Fmt,+File) is det
%
% @param Fmt see dot documentation. E.g. png
%
% translate Graph to dot, translate to an image format and write to File.
% if File is var, write to user_output
graph_to_dot_file(GraphTerm,Fmt,File):-
        Opts=[], % TODO
        (nonvar(Fmt) -> true ; Fmt=dot),
        (   nonvar(File) -> true ; tmp_file(Fmt,FileBase),concat_atom([FileBase,Fmt],'.',File)),
        graph_to_dot_atom(GraphTerm,Dot),
        (   Fmt=dot
        ->  DotFile=File
        ;   tmp_file(dot,DotFile)),
        tell(DotFile),
        write(Dot),
        told,
        (   Fmt=dot
        ->  true
        ;   (   member(rankdir(RankDir),Opts)
            ->  true
            ;   RankDir='BT'),
            concat_atom([dot,' -o ',File,' -Grankdir=',RankDir,' -T',Fmt,' ',DotFile],Cmd),
            shell(Cmd)).


%% graph_display(+GraphTerm,+Cmd)
%   displays a graph using Cmd.
%   e.g. 'open' or 'xv'
graph_display(GraphTerm,DisplayCmd):-
        graph_display(GraphTerm,png,DisplayCmd).

%% graph_display(+GraphTerm,+Fmt,+Cmd)
%   displays a graph in Fmt using Cmd
graph_display(GraphTerm,Fmt,DisplayCmd):-
        graph_to_dot_file(GraphTerm,Fmt,File),
        debug(dot,'File: ~w',[File]),
        concat_atom([DisplayCmd,File],' ',FullCmd),
        shell(FullCmd).


% ----------------------------------------
% GRAPH DATA STRUCTURE TRANSFORMATIONS
% ----------------------------------------
% primarily to make nesting easier

%% graph_nest(+Graph,?NestGraph,+ContainmentRelations:list) is det
%
% given a graph term and a set of edge labels, translate the graph such that
% edges of the designated type are translated to subgraphs (clusters in graphviz)
%
% For example, in an anatomical ontology, the partOf relation can be used
% as a containment relation such that organ boxes and located in the organism box
%
% The relation used should not have multiple parents, otherwise the results
% are unpredictable.
%
% @see http://wiki.geneontology.org/index.php/OBO-Edit:Experimental_Graphviz_Views
graph_nest(G,GX,Relations):-
        G=graph(_,_,Elts),
        findall(A-B,(member(E,Elts),E=edge(A,B,Atts),member(label=Label,Atts),member(Label,Relations)),NestPairs),
        graph_nest_pairs(G,GX,NestPairs).

%% graph_nest_pairs(+Graph,?NestGraph,+NestPairs:list) is det
% 
% use NestPairs to nest the graph
% 
% @see graph_nest/3
graph_nest_pairs(graph(G,Atts,Elts),graph(G,[compound=true|Atts],EltsX),NestPairs):-
        
        % collect all edges, removing those that will turn into cluster nestings
        findall(E,(member(E,Elts),E=edge(A,B,_),\+ member(A-B,NestPairs)),Edges),
        
        % collect all nodes that are neither cluster members nor clusters (these will be children of root graph)
        findall(N,(member(N,Elts),N=node(A,_), \+ member(_-A,NestPairs), \+ member(A-_,NestPairs)),Nodes),

        % find roots of cluster hierarchy (these will be subgraph roots)
        solutions(X,(member(_-X,NestPairs),\+ member(X-_,NestPairs)),Roots),

        % unifies with a list of subgraph clusters and nodes
        findall(SubGraph,
                (   member(X,Roots),
                    subgraph(X,Elts,NestPairs,SubGraph)),
                SubGraphs),

        maplist(fix_edge_for_cluster(NestPairs),Edges,EdgesX),

        % put it all together
        flatten([Nodes,EdgesX,SubGraphs],EltsX).

        

% succeed once if this node should be transformed into a cluster
% TODO: if the nest hierarchy is not a strict tree, either throw and
% exception OR duplicate the repeating nodes with different IDs, and
% add an equivalence arc between them
subgraph(X,OrigElts,NestPairs,subgraph(cluster(X),Atts,SubGraphs)):-
        findall(SubGraph,
                (   member(Child-X,NestPairs),
                    subgraph(Child,OrigElts,NestPairs,SubGraph)),
                SubGraphs),
        SubGraphs\=[],
        !,
        %atom_concat('cluster_',X,Cluster),
        member(node(X,Atts),OrigElts).

% no child nodes: terminal
subgraph(X,OrigElts,_,N):-
        N=node(X,_),
        member(N,OrigElts).

% edges between clusters must be declared with lhead and ltail;
% furthermore, these must be between terminal (non-cluster) nodes.
% there are 3 cases to be considered:
%  * cluster to cluster
%  * cluster to terminal
%  * terminal to cluster
fix_edge_for_cluster(NestPairs,Edge,EdgeX):-
        Edge=edge(A,B,Atts),
        member(_-A,NestPairs), % A is a cluster
        member(_-B,NestPairs), % B is a cluster
        !,                      % edge is between two clusters
        select_random_terminal_node(A,NestPairs,AX),
        select_random_terminal_node(B,NestPairs,BX),
        EdgeX=edge(AX,BX,[ltail=cluster(A),lhead=cluster(B)|Atts]).
fix_edge_for_cluster(NestPairs,Edge,EdgeX):-
        Edge=edge(A,B,Atts),
        member(_-A,NestPairs), % A is a cluster
        \+ member(_-B,NestPairs), % B is a terminal [redundant check]
        !,                      % between a cluster and a terminal
        select_random_terminal_node(A,NestPairs,AX),
        EdgeX=edge(AX,B,[ltail=cluster(A)|Atts]).
fix_edge_for_cluster(NestPairs,Edge,EdgeX):-
        Edge=edge(A,B,Atts),
        member(_-B,NestPairs), % B is a cluster
        \+ member(_-A,NestPairs), % A is a terminal [redundant check]
        !,                      % between a terminal and a cluster 
        select_random_terminal_node(B,NestPairs,BX),
        EdgeX=edge(A,BX,[lhead=cluster(B)|Atts]).
fix_edge_for_cluster(_,Edge,Edge). % between a terminal and a terminal


select_random_terminal_node(A,NestPairs,A):-
        \+ member(_-A,NestPairs),
        !.
select_random_terminal_node(A,NestPairs,AX):-
        member(Child-A,NestPairs),
        select_random_terminal_node(Child,NestPairs,AX).



/** <module> maps graph structures to dot format

  ---+ Synopsis

  ==
  :- use_module(bio(dot)).

  % data: nodes are reactions, edges are input/outputs
  % (note: dot does not use these facts directly)
  n(1, 'cyclin_cdc2k dissociation').
  n(2, 'cdc2k phosphorylation').
  n(3, 'cdc2k dephosphorylation').
  e(1,2,'cdc2k').
  e(2,3,'cdc2k-p').
  e(3,2,'cdc2k').
  
  % demo: convert prolog db to a graph datastructured,
  % then translate this to a diagram
  % outputs: <img alt="biochemical pathway" src="cdc.png"/>
  % <cdc.png> CDC
  demo:-
    findall(node(ID,[label=N,font='Verdana']),n(ID,N),Nodes),
    findall(edge(ID1,ID2,[label=Type,weight=1]),e(ID1,ID2,Type),Edges),
    flatten([Nodes,Edges],Subterms),
    G=graph(my_graph,Subterms),
    graph_to_dot_file(G,png,'cdc.png'),
    writeln('Please view the file "cdc.png" in an image viewer').
  ==
  
  ---+ Description

  utility module - maps a graph data structure into a dot program. dot
files can be imported into applications like OmniGraffle, displayed
using imagemagick, or transformed into pngs etc using Dot
programs 'dot' and 'neatto'

  ---++ Data structure

  ==
  GraphTerm = graph(Name,Terms)
       Term = node(ID,Props) | edge(ID1,ID2,Props)
  ==

  Props = [P1=V1, ..., Pn=Vn]

  where each properties is specified by the graphviz spec
  
  ---+ Nesting

  Graphviz allows nesting of boxes. This module allows easy
  construction of nested diagrams by allowing a simple graph data
  structure to be transformed into a nested structure by specifying
  one of the edge labels to be a nesting label.  See graph_nest/3

  For example, =|graph_nest(G,G2,[inside])|= holds for:

  ==
  G = graph(mygraph,[node(n1,[]), ..., edge(n1,n2,inside),edge(n1,n3,adj),..])
  ==

  ==
  G2 = graph(mygraph,[subgraph(cluster(n2),[node(n1,[]), ..., edge(n1,n3,adj),..]), ...])
  ==
  
  ---+ Dependencies

  AT&T Dot (the 'dot' or 'neato' executable)

  ---+ TODO

  should we provide hook predicates to avoid the need to construct a
  data structure?
  
  */
