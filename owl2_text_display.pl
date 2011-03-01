/* -*- Mode: Prolog -*- */

:- module(owl2_text_display,
          [display_term/1,
           display_term/2,

           display_class_tree/1,
           display_class_tree/2,

           display_instance_tree/2,
           display_object_tree/2
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
        contract_iri(T,Tc,Opts),
        format('~w~w~w',[Tc,Sep,X]).
display_subterm(T,_Sep,Opts) :-
        member(display(plsyn),Opts),
        !,
        plsyn_owl(X,T),
        format('~w',[X]).
display_subterm(T,Sep,Opts) :-
        member(display(labels),Opts),
        !,
        contract_iri(T,Tc,Opts),
        format('~w',[Tc]),
        format('~w',[Sep]),
        (   labelAnnotation_value(T,N)
        ->  format('~w',[N])
        ;   true).
display_subterm(T,_,_) :-
        format('~w',[T]).

contract_iri(X,Y,Opts) :-
        ground(X),
        atom(X),
        member(display(fragments),Opts),
        atomic_list_concat([_,Y],'#',X),
        !.
contract_iri(X,X,_).

        

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

display_instance_tree(X,Opts) :-
        display_instance_tree('',X,[],[_,_,_,_,_],[],Opts).
display_instance_tree(P,X,D,MD,VL,Opts) :-
        \+ member(X,VL),
        \+ D=MD,
        !,
        writetab(D),
        display_subterm(P,Opts),
        write(' '),
        display_subterm(X,Opts),
        nl,
        D2=[x|D],
        forall(propertyAssertion(P2,X,X2),
               display_instance_tree(P2,X2,D2,MD,[X|VL],Opts)).
display_instance_tree(_,_,_,_,_,_).

display_object_tree(X,Opts) :-
        display_object_tree('',X,[],[_,_,_,_,_,_,_],[],Opts).
display_object_tree(P,X,D,MD,VL,Opts) :-
        \+ ((atom(X),           % don't worry about revisiting expressions
             member(X,VL))),
        \+ D=MD,
        !,
        writetab(D),
        write('|'),
        display_subterm(P,Opts),
        write(' '),
        display_subterm(X,Opts),
        nl,
        D2=[x|D],
        forall(object_tree_link(X,X2,P2),
               display_object_tree(P2,X2,D2,MD,[X|VL],Opts)).
display_object_tree(_,_,_,_,_,_).

object_tree_link(X,Y,eq) :- equivalent_to(X,Y),atom(X),\+atom(Y).
object_tree_link(intersectionOf(L),Y,'*') :- member(Y,L).
object_tree_link(someValuesFrom(P,Y),Y,P).
object_tree_link(allValuesFrom(P,Y),Y,only-P).
object_tree_link(X,Y,P) :- propertyAssertion(P,X,Y),atom(Y). % config?
object_tree_link(X,Y,inst) :- classAssertion(Y,X),\+builtin_class(Y).

% ----------------------------------------
% MESSAGES
% ----------------------------------------

:- multifile prolog:message//1.
prolog:message(owlfmt(Fmt,Args)) -->
        {findall(Arg2,(member(Arg,Args),
                     owlpp(Arg,[],Toks,[]),
                     atomic_list_concat(Toks,Arg2)),
                 Args2),
         sformat(Out,Fmt,Args2)},
         [Out].

prolog:message(owlpp(A)) --> owlpp(A,[]).
prolog:message(owlpp(A,Opts)) --> owlpp(A,Opts).

owlpp(literal(type(_,V)),Opts) --> {atom(V)},!,[V].
owlpp(literal(V),Opts) --> {atom(V)},!,[V].
owlpp(A,Opts) --> {\+atom(A),A=..[P,Arg],!},owlpp(P,Opts),spc,owlpp(Arg,Opts).
owlpp(A,Opts) --> {\+atom(A),A=..[P|Args],!},owlpp_args(P,Args,Opts).
owlpp(A,_Opts) --> {atom(A),labelAnnotation_value(A,V)},!,owlpp_iri(A),spc,owlpp_qt(V).
owlpp(A,_Opts) --> [A].

owlpp_args(_,[],_) --> !,[].
owlpp_args(_,[A],Opts) --> !,owlpp(A,Opts).
owlpp_args(P,[A|Args],Opts) --> !,owlpp(A,Opts),spc,owlpp(P,Opts),spc,owlpp_args(P,Args,Opts).

owlpp_iri(A) --> ['<',A,'>'].
owlpp_qt(A) --> ['\'',A,'\''].
spc --> [' '].









