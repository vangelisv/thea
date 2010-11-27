/* -*- Mode: Prolog -*- */

:- module(owl2_text_display,
          [display_term/1,
           display_term/2,

           display_class_tree/1,
           display_class_tree/2
           ]).

:- use_module(owl2_model).


% ----------------------------------------
% DISPLAY
% ----------------------------------------
% cut and pasted from thea-owl-i. todo: move to module
% added stream. s/saveopt/display/

%% display_term(+T,+Opts) is det
%% display_term(+T) is det
%
% writes out a prolog term (which may be an axiom, an expression, or
% an arbitrary prolog term that contains an axiom or expression with it)
display_term(T) :-
    display_term(T,[]).

display_term(T,Opts) :-
        select(display(combined),Opts,Opts2),
        !,
        format('% ',[]),
        display_term(T,Opts2),
        format('~q.~n',[T]).
display_term(T,Opts) :-
        member(display(tr(T,TG,Ont,Ax)),Opts),
        !,
        forall(TG,
               assert_axiom(Ax,Ont)).
display_term(T,Opts) :-
        member(display(tabular),Opts),
        !,
        T=..L,
        display_subterms(L,'\t',Opts),
        nl.
display_term(T,Opts) :-
        member(display(prolog),Opts),
        !,
        format('~q.~n',[T]).
display_term(T,Opts) :-
        member(display(plsyn),Opts),
        member(display(labels),Opts),
        !,
        map_IRIs(use_label_as_IRI,T,T2),
        plsyn_owl(X,T2),
        (   member(display(show_orig),Opts)
        ->  format('~q.~n',[T])
        ;   true),
        (   member(display(no_plquote),Opts)
        ->  format('~w~n',[X])
        ;   format('~q.~n',[X])).
display_term(T,Opts) :-
        member(display(plsyn),Opts),
        !,
        plsyn_owl(X,T),
        format('~q.~n',[X]).
display_term(T,Opts) :-
        member(display(labels),Opts),
        !,
        map_IRIs(use_label_as_IRI,T,X),
        format('~w~n',[X]).
display_term(T,_) :-
        format('~w~n',[T]).

display_subterms(_,[],_,_).
display_subterms([T|L],Sep,Opts) :-
        !,
        display_subterm(T,Sep,Opts),
        (   L=[]
        ->  true
        ;   format(Sep,[]),
            display_subterms(L,Sep,Opts)).

display_subterm(T,Opts) :-
        display_subterm(T,' ',Opts).

display_subterm(T,Sep,Opts) :-
        member(display(labels),Opts),
        member(display(plsyn),Opts),
        !,
        map_IRIs(use_label_as_IRI,T,T2),
        plsyn_owl(X,T2),
        format('~w~w~w',[T,Sep,X]).
display_subterm(T,_Sep,Opts) :-
        member(display(plsyn),Opts),
        !,
        plsyn_owl(X,T),
        format('~w',[X]).
display_subterm(T,Sep,Opts) :-
        member(display(labels),Opts),
        !,
        format('~w',[T]),
        format('~w',[Sep]),
        (   labelAnnotation_value(T,N)
        ->  format('~w',[N])
        ;   true).
display_subterm(T,_,_) :-
        format('~w',[T]).


%% display_class_tree(+Class,+Opts:list) is det
%
% shows a class in a tree context
display_class_tree(Class) :-
        display_class_tree(Class,[]).

display_class_tree(Class,Opts) :-
        member(traverse(P),Opts),
        !,
        forall(display_class_tree(Class,
                        X-Y-(   subClassOf(X,Y)
                            ;   X=someValuesFrom(P,Y)),
                        _,
                        []),
               true).
display_class_tree(Class,Opts) :-
        forall(display_class_tree(Class,X-Y-subClassOf(X,Y),_,Opts),
               true).


% display_class_tree(+Class, +Template, ?Tab, +Opts)
% @param Class IRI of class to work back from
% @param Template X-Y-Goal
% @param Tab Description here
% @param Opts Description here

display_class_tree(Class,Template,Tab2,Opts) :-
        copy_term(Template,Class-Parent-Goal),
        (   Goal
        *-> display_class_tree(Parent,Template,Tab,Opts),
            Tab2 = [' '|Tab],
            writetab(Tab2)
        ;   Tab2=[]),
        write_owl_class(Class,Opts),
        nl.

writetab([]).
writetab([_|L]):-
        write(' '),
        writetab(L).

write_owl_class(Class,Opts) :- display_subterm(Class,Opts).

/*
write_owl_class(Class,_) :-
        labelAnnotation_value(Class,Label),
        !,
        write(Label).
write_owl_class(Class,_) :-
        write(Class).
*/
