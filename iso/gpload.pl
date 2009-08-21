
:- initialization(load_all).

load_all:-
        [owl2_model],
        [owl2_io],
        [owl2_metamodel],
        [owl2_xml].



t2:-
        load_axioms('../testfiles/wine.pl',owlpl),
        subClassOf(X,Y),
        write(X-Y),nl,fail.




nb_current(_,_) :- fail.

%%	forall(:Cond, :Action) is semidet.
%
%	True if for all solutions of Cond, Action is true

forall(Cond, Action) :-
	\+ (Cond, \+ Action).


%%	maplist(:Goal, ?List)
%
%	True if Goal can succesfully be applied on all elements of List.
%	Arguments are reordered to gain performance as well as to make
%	the predicate deterministic under normal circumstances.

maplist(Goal, List) :-
	maplist_(List, Goal).

maplist_([], _).
maplist_([Elem|Tail], Goal) :-
	call(Goal, Elem),
	maplist_(Tail, Goal).

%%	maplist(:Goal, ?List1, ?List2)
%
%	True if Goal can succesfully be applied to all succesive pairs
%	of elements of List1 and List2.

maplist(Goal, List1, List2) :-
	maplist_(List1, List2, Goal).

maplist_([], [], _).
maplist_([Elem1|Tail1], [Elem2|Tail2], Goal) :-
	call(Goal, Elem1, Elem2),
	maplist_(Tail1, Tail2, Goal).

%%	maplist(:Goal, ?List1, ?List2, ?List3)
%
%	True if Goal can succesfully be applied to all succesive triples
%	of elements of List1..List3.

maplist(Goal, List1, List2, List3) :-
	maplist_(List1, List2, List3, Goal).

maplist_([], [], [], _).
maplist_([Elem1|Tail1], [Elem2|Tail2], [Elem3|Tail3], Goal) :-
	call(Goal, Elem1, Elem2, Elem3),
	maplist_(Tail1, Tail2, Tail3, Goal).


%%	maplist(:Goal, ?List1, ?List2, ?List3, List4)
%
%	True if Goal  can  succesfully  be   applied  to  all  succesive
%	quadruples of elements of List1..List4

maplist(Goal, List1, List2, List3, List4) :-
	maplist_(List1, List2, List3, List4, Goal).

maplist_([], [], [], [], _).
maplist_([Elem1|Tail1], [Elem2|Tail2], [Elem3|Tail3], [Elem4|Tail4], Goal) :-
	call(Goal, Elem1, Elem2, Elem3, Elem4),
	maplist_(Tail1, Tail2, Tail3, Tail4, Goal).

