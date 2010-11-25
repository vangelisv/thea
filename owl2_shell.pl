/* -*- Mode: Prolog -*- */

:- use_module(owl2_model).
:- use_module(owl2_plsyn).
:- use_module(owl2_util).
:- use_module(owl2_text_display).

:- op(1100,xfy,--).
:- op(1090,fx,new).
:- op(1090,fx,add).
:- op(1090,fx,rm).
:- op(1090,fx,m).
:- op(1090,fx,q).
:- op(1090,fx,v).
:- op(1090,fx,t).
:- op(1090,fx,rq).
:- op(1090,fx,set).
:- op(1090,fx,unset).
:- op(800,xfy,to).

:- dynamic transaction/1.
:- dynamic redo_stack/1.


:- initialization(shell_init).

:- redefine_system_predicate('>'(_,_)).

user:file_search_path(home, Home) :-
	getenv('HOME',Home).

rcfile(F) :- absolute_file_name(home('.thearc'),F).

shell_init :-
        set_prolog_flag(verbose,normal),
        print_message(banner,thea_shell_welcome),
        rcfile(F),
        exists_file(F),
        !,
        consult(F).

save_settings :-
        rcfile(F),
        open(F,write,S,[]),
        Q=settings(_,_),
        forall(Q,format(S,'~q.~n',[Q])),
        close(S).

% EDIT CMDS
edit_op(Op,AxiomIn) :- current_ontology(Ont),!,tr(AxiomIn,Axiom), Act =.. [Op,Axiom,Ont],Act,print_message(informational,Act),assertz(transaction(Act)).
edit_op(_,_) :- print_message(error,no_current_ontology).
add AxiomIn :- edit_op(assert_axiom,AxiomIn).
rm AxiomIn :- edit_op(retract_axiom,AxiomIn).
undo :- transaction(Act),undo(Act),print_message(informational,undo(Act)),assertz(redo_stack(Act)),retract(transaction(Act)).
undo(assert_axiom(A,O)) :- retract_axiom(A,O).
undo(retract_axiom(A,O)) :- assert_axiom(A,O).
redo :- redo_stack(Act),Act,print_message(informational,redo(Act)),retract(redo_stack(Act)).
new Ont :- assert_axiom(ontology(Ont)),nb_setval(ontology,Ont).

% DISPLAY CMDS
t N :- label2iri(N,X), current_opts(Opts), display_class_tree(X,Opts).
q QueryIn :- tr(QueryIn,Query), forall(Query,v(Query)).
%q :- tr(Query,Axiom), q(_).
v Axiom :- current_opts(Opts),display_term(Axiom,Opts).
'--'(GoalIn,Cmd) :-
        tr(GoalIn,Goal),
        current_opts(Opts),
        open(pipe(Cmd),write,S,[]),
        forall(Goal,display_term(S,Goal,Opts)),
        close(S).
Cmd > File :-                   % TODO
        open(File,write,S,[]),
        with_output_to(S,Cmd),
        close(S).
ls :-
        forall(ontology(Ont),
               (   (   nb_current(ontology,Ont)
                   ->  write('*')
               ;   write(' ')),
                   writeln(Ont))).

set (P to V) :- !,unset(P), assert(settings(P,V)),save_settings.
set P + V :- !,assert(settings(P,V)),save_settings.
set P - V :- !,retractall(settings(P,V)),save_settings.
unset P :- retractall(settings(P,_)),save_settings.
settings :- q(settings(_,_)).


tr(In,Out) :- plsyn_owl(In,Out),!.

:- multifile settings/2.
:- dynamic settings/2.
opt_doc(display,'options for tuning screen display of axioms').
opt_value(display,combined,'show both prolog and pretty').
opt_value(display,tabular,'TSV for axiom arguments').
opt_value(display,prolog,'show prolog syntax rather than pretty').
opt_value(display,plsyn,'show prolog plsyn syntax').
opt_value(display,labels,'show labels not IRIs').

current_opts(Opts) :-
        findall(Opt,
                (   settings(P,V),
                    Opt =.. [P,V]),
                Opts).
current_ontology(Ont) :- nb_current(ontology,Ont),!.
current_ontology(Ont) :-
        ontology(Ont),
        !,
        set_current_ontology(Ont).

set_current_ontology(Ont) :-
        nb_setval(Ont),
        print_message(informational,shell('Set ontology to ~w'-[Ont])).

label2iri(Label,Obj) :- labelAnnotation_value(Obj,Label),!.
label2iri(X,X).


prolog:message(thea_shell_welcome) -->
               ['%  ::: Welcome to Thea Shell :::'].

prolog:message(redo(Act)) --> ['Redo: '],prolog:message(Act).
prolog:message(undo(Act)) --> ['Undo: '],prolog:message(Act).
prolog:message(assert_axiom(A,Ont)) --> ['Asserting '],prolog:message(ax(A)),[' into ',Ont].
prolog:message(retract_axiom(A,Ont)) --> ['Retracting '],prolog:message(ax(A)),[' into ',Ont].
prolog:message(ax(Ax)) --> [Atom],{current_opts(Opts),with_output_to(atom(Atom),display_term(Ax,Opts))}.



