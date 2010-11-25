/* -*- Mode: Prolog -*- */

:- module(owl2_text_display,
          [display_term/1,
           display_term/2,
           display_term/3,

           display_class_tree/1,
           display_class_tree/2
           ]).

:- use_module(owl2_model).


% ----------------------------------------
% DISPLAY
% ----------------------------------------
% cut and pasted from thea-owl-i. todo: move to module
% added stream. s/saveopt/display/

%% display_term(+Stream,+T,+Opts) is det
%% display_term(+T,+Opts) is det
%% display_term(+T) is det
%
% writes out a prolog term (which may be an axiom, an expression, or
% an arbitrary prolog term that contains an axiom or expression with it)
display_term(T) :-
    display_term(T,[]).
display_term(T,Opts) :-
        display_term(user_output,T,Opts).

display_term(Stream,T,Opts) :-
        select(display(combined),Opts,Opts2),
        !,
        format(Stream,'% ',[]),
        display_term(Stream,T,Opts2),
        format(Stream,'~q.~n',[T]).
display_term(_Stream,T,Opts) :-
        member(display(tr(T,TG,Ont,Ax)),Opts),
        !,
        forall(TG,
               assert_axiom(Ax,Ont)).
display_term(Stream,T,Opts) :-
        member(display(tabular),Opts),
        !,
        T=..L,
        display_subterms(Stream,L,'\t',Opts),
        nl.
display_term(Stream,T,Opts) :-
        member(display(prolog),Opts),
        !,
        format(Stream,'~q.~n',[T]).
display_term(Stream,T,Opts) :-
        member(display(plsyn),Opts),
        member(display(labels),Opts),
        !,
        map_IRIs(use_label_as_IRI,T,T2),
        plsyn_owl(X,T2),
        (   member(display(show_orig),Opts)
        ->  format(Stream,'~q.~n',[T])
        ;   true),
        (   member(display(no_plquote),Opts)
        ->  format(Stream,'~w~n',[X])
        ;   format(Stream,'~q.~n',[X])).
display_term(Stream,T,Opts) :-
        member(display(plsyn),Opts),
        !,
        plsyn_owl(X,T),
        format(Stream,'~q.~n',[X]).
display_term(Stream,T,Opts) :-
        member(display(labels),Opts),
        !,
        map_IRIs(use_label_as_IRI,T,X),
        format(Stream,'~w~n',[X]).
display_term(Stream,T,_) :-
        format(Stream,'~w~n',[T]).

display_subterms(_,[],_,_).
display_subterms(Stream,[T|L],Sep,Opts) :-
        !,
        display_subterm(Stream,T,Sep,Opts),
        (   L=[]
        ->  true
        ;   format(Stream,Sep,[]),
            display_subterms(Stream,L,Sep,Opts)).

display_subterm(T,Opts) :-
        display_subterm(user_output,T,' ',Opts).

display_subterm(Stream,T,Sep,Opts) :-
        member(display(labels),Opts),
        member(display(plsyn),Opts),
        !,
        map_IRIs(use_label_as_IRI,T,T2),
        plsyn_owl(X,T2),
        format(Stream,'~w~w~w',[T,Sep,X]).
display_subterm(Stream,T,_Sep,Opts) :-
        member(display(plsyn),Opts),
        !,
        plsyn_owl(X,T),
        format(Stream,'~w',[X]).
display_subterm(Stream,T,Sep,Opts) :-
        member(display(labels),Opts),
        !,
        format(Stream,'~w',[T]),
        format(Stream,'~w',[Sep]),
        (   labelAnnotation_value(T,N)
        ->  format(Stream,'~w',[N])
        ;   true).
display_subterm(Stream,T,_,_) :-
        format(Stream,'~w',[T]).


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
