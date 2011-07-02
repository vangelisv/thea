/* -*- Mode: Prolog -*- */

:- use_module(owl2_model).
:- use_module(owl2_plsyn).
:- use_module(owl2_util).
:- use_module(owl2_reasoner).
:- use_module(owl2_text_display).
:- use_module(owl2_popl,[popl_translate/1,(=::=)/2]).
:- use_module(owl2_dotty).
:- use_module(owl2_profiles).

:- op(1100,xfy,--).
:- op(1090,fx,new).
:- op(1090,fx,add).
:- op(1090,fx,init).
:- op(1090,fx,rm).
:- op(1090,fx,m).
:- op(1090,fx,q).
:- op(1090,fx,qi).
:- op(1090,fx,pq).
:- op(1090,fx,v).
:- op(1090,fx,itree).
:- op(1090,fx,t).
:- op(1090,fx,l).
:- op(1090,fx,set).
:- op(1090,fx,unset).
:- op(1100,xfy,===>). % from POPL
:- op(1000,xfy,where).
:- op(1000,xfy,with).
:- op(800,xfy,to).
:- op(800,fx,(?)).

:- dynamic transaction/1.
:- dynamic redo_stack/1.
:- discontiguous cmd_doc/4.
:- multifile edit_template/2.

:- initialization(shell_init).

:- redefine_system_predicate('>'(_,_)).
:- redefine_system_predicate(help).

user:file_search_path(home, Home) :-
	getenv('HOME',Home).

rcfile(F) :- absolute_file_name(home('.thearc'),F).

shell_init :-
        set_prolog_flag(verbose,normal),
        print_message(banner,thea_shell_welcome),
        RDFS_Label='http://www.w3.org/2000/01/rdf-schema#label',
        assert_axiom(annotationProperty(RDFS_Label)),
        assert_axiom(annotationAssertion(RDFS_Label,RDFS_Label,literal(lang(en,label)))),
        consult_rc.

consult_rc :-
        rcfile(F),
        exists_file(F),
        !,
        style_check(-singleton),
        consult(F),
        style_check(+singleton).
consult_rc.


save_settings :-
        rcfile(F),
        open(F,write,S,[]),
        Q=settings(_,_),
        forall(Q,format(S,'~q.~n',[Q])),
        close(S).

% EDIT CMDS
cmd_doc(edit,new,[axiom],'Add axiom to current ontology').
cmd_doc(edit,add,[axiom],'Add axiom to current ontology').
cmd_doc(edit,rm,[axiom],'Retract axiom to current ontology').
cmd_doc(edit,undo,[],'Undo last add/rm').
cmd_doc(edit,redo,[],'Redo last undo').

edit_op(ActIn) :- tr(ActIn,Act), Act,print_message(informational,Act),asserta(transaction(Act)).
%edit_op(_,_) :- print_message(error,no_current_ontology).
add AxiomIn where Goal :- !, forall(Goal,add(AxiomIn)).
add AxiomIn :- current_ontology(Ont),!,edit_op(assert_axiom(AxiomIn,Ont)).
add _ :- throw(error(set_current_ont)).
rm AxiomIn :- edit_op(retract_axiom(AxiomIn)). % todo - to make this undoable we must ground variables within a forall...
undo :- transaction(Act),undo(Act),print_message(informational,undo(Act)),asserta(redo_stack(Act)),retract(transaction(Act)).
undo(assert_axiom(A,O)) :- retract_axiom(A,O).
undo(retract_axiom(A,O)) :- assert_axiom(A,O).
redo :- redo_stack(Act),Act,print_message(informational,redo(Act)),retract(redo_stack(Act)).
new Ont :- assert_axiom(ontology(Ont)),nb_setval(ontology,Ont).

'===>'(AIn,BIn where G) :- !,tr(AIn,A),tr(BIn,B),popl_translate(A ===> B where G).
'===>'(AIn,BIn) :- tr(AIn,A),tr(BIn,B),popl_translate(A ===> B).

% EDIT TEMPLATES
+(TN) :-
        current_opts(Opts),
        T=..[TN,IRI],
        % example: template(X), [iri(prefix(obo)),class(X),N-annotationAssertion(label,X,N)]
        edit_template(T,[iri(IRI_Template)|Fields]),
        get_field_info(iri(IRI_Template),IRI),
        %get_field_axiom_list(Fields,IRI,Axs_1),
        findall(Ax,(member(Field,Fields),
                    get_field_axiom(Field,IRI,Ax)),
                Axs_1),
        flatten(Axs_1,Axs),
        nl,
        writeln('% ------------------'),
        writeln('% AXIOMS TO ADD:'),
        writeln('% ------------------'),
        (   member(display(combined),Opts)
        ->  Opts_2=Opts
        ;   Opts_2=[display(combined)|Opts]),
        forall(member(Ax,Axs),
               (   tr(Ax,Ax_2),
                   display_term(Ax_2,Opts_2))),
        input('% OK? [enter for yes, any other char for no]',Ok),
        nl,
        (   Ok=''
        ->  maplist(add,Axs),
            writeln('% AXIOMS ADDED. Type "undo." to retract.')
        ;   print_message(informational,not_added)).
/*
get_field_axiom_list([],_,[]).
get_field_axiom_list([Field|Fields],IRI,[Ax|Axs]) :-
        get_field_axiom(Field,IRI,Ax),
        get_field_axiom_list(Fields,IRI,Axs).
*/

get_field_info(iri(obo(Prefix)),IRI) :-
        !,
        get_field_info(iri(obo(Prefix,7)),IRI).
get_field_info(iri(obo(IDSpace,NumDigits)),IRI) :-
        !,
        concat_atom(['http://purl.obolibrary.org/obo/',IDSpace,'_'],Prefix),
        get_field_info(iri(prefix(Prefix,NumDigits)),IRI).
get_field_info(iri(prefix(Prefix,NumDigits)),IRI) :-
        findall(X,(entity(E),concat_atom([Prefix,X],E),is_numeric_atom(X)),Xs),
        sort(Xs,SXs),
        (   reverse(SXs,[Last|_])
        ->  true
        ;   Last='-1'),
        atom_number(Last,LastID),
        ID is LastID+1,
        concat_atom(['~',NumDigits,'d'],Fmt),
        format(atom(A1),Fmt,[ID]),sub_atom(A1,2,_,0,PaddedID),
        atom_concat(Prefix,PaddedID,IRI).

get_field_axiom(multi(T),_,AxiomsOut) :-
        !,
        repeat,
        (   get_field_axiom(T,_,AxiomsOut)
        ->  true
        ;   !,
            fail).
get_field_axiom(Val-Axioms,_,AxiomsOut) :-
        !,
        repeat,
        nl,
        write('% Template: '),writeln(Axioms),
        tr(Axioms,AxiomsOut),
        write('% Enter value: '),write(Val),
        input(' >> ',Val),
        (   Val=''
        ->  !,
            fail
        ;   \+ is_valid_axiom_list(AxiomsOut)
        ->  print_message(error,not_valid_axioms(AxiomsOut)),
            fail
        ;   format('% Val=~w~n',[Val]),
            tr(AxiomsOut,AxiomsOut_2),
            show(AxiomsOut_2),
            !,
            true).
get_field_axiom(Axiom,_,Axiom) :-
        Axiom\=_-_,
        Axiom\=multi(_),
        !,
        show(Axiom).
is_numeric_atom(A) :- catch(atom_number(A,_),_,fail).

input(Prompt,Val) :-
        write(Prompt),
        read_line_to_codes(user_input,Codes),
        atom_codes(Val,Codes).

is_valid_axiom_list(L) :- is_list(L),!,forall(member(A,L),is_valid_axiom(A)).
is_valid_axiom_list(A) :- is_valid_axiom(A).


% DISPLAY CMDS
cmd_doc(display,t,[class],'Show a tree').
cmd_doc(display,q,[term],'Query axioms').
cmd_doc(display,v,[axiom],'Query axioms').
cmd_doc(display,--,[cmd],'pipe through unix shell. E.g. q _<_ -- \'grep neuron\'.').

t N :- label2iri(N,X), nb_setval(obj,X),current_opts(Opts), display_class_tree(X,Opts).
l N :-
        label2iri(N,X),
        format('% IRI: ~w~n',[X]),
        nb_setval(obj,X),
        current_opts(Opts),
        setof(A,
              (   axiom_references(A,X),
                  axiom_type(A,T)),
              As),
        format('% Axiom Type: ~w~n',[T]),
        forall(member(A,As),
               display_term(A,Opts)),
        fail.
l _.

%% trshow(+Term) is det
%% show(+Term) is det
trshow(XIn) :- tr(XIn,X),show(X).
show(X) :- current_opts(Opts),display_term(X,Opts).
v ObjIn :- tr(ObjIn,Obj),current_opts(Opts),visualize_obj(Obj,Opts).
'--'(GoalIn,Cmd) :-
        tr(GoalIn,Goal),
        open(pipe(Cmd),write,S,[]),
        with_output_to(S,forall(Goal,true)),
        close(S).
Cmd > File :-                   % TODO
        open(File,write,S,[]),
        with_output_to(S,Cmd),
        close(S).

itree ObjIn :- tr(ObjIn,Obj),current_opts(Opts),display_object_tree(Obj,Opts).

% LIST CMDS
cmd_doc(display,ls,[],'List ontologies').
cmd_doc(display,lsa,[],'List axioms in current ontology').

ls :-
        forall(ontology(Ont),
               (   (   current_ontology(Ont)
                   ->  write('*')
               ;   write(' ')),
                   writeln(Ont))).
lsa :-
        current_ontology(Ont),
        !,
        current_opts(Opts),
        forall(ontologyAxiom(Ont,A),
               display_term(A,Opts)).
lsa :-
        !,
        current_opts(Opts),
        forall(axiom(A),
               display_term(A,Opts)).

cmd_doc(display,stats,[],'Stats on current ontology').
stats :- current_ontology(Ont),stats(Ont).
stats(Ont) :-
        forall(aggregate(count,A,Arity^(ontologyAxiom(Ont,A),functor(A,T,Arity)),Num),
               format('#~w\t~w~n',[T,Num])).

% QUERY CMDS

%% gi(Axiom) - entailment query - translates Axiom to prolog axiom term and calls reasoner_ask/1
%% g(Axiom) - assertion query  - translates Axiom to prolog goal and calls it directly
cmd_doc(query,gi,[axiom],'Entailment query - translates Axiom to native prolog axiom term and calls reasoner. as qi/1 with no result mapping.').
cmd_doc(query,g,[axiom],'Assertion query - translates Axiom to prolog goal and calls directly. As q/1 with no result mapping.').
gi(AxiomIn) :- tr(AxiomIn,Axiom),reasoner_ask(Axiom).
g(AxiomIn) :- tr(AxiomIn,Axiom),Axiom.
{AxiomIn} :- tr(AxiomIn,Axiom),reasoner_ask(Axiom).


% select from asserted database
q SelectIn where QueryIn :- !,tr(QueryIn,Query),tr(SelectIn,Select), forall(Query,show(Select)).
q QueryIn :- !,tr(QueryIn,Query), forall(Query,show(Query)).

% select from asserted database, no translation
pq Select where Query :- !, forall(Query,show(Select)).
pq Query :- !, forall(Query,show(Query)).

% select from reasoned database
qi SelectIn where QueryIn :- !,tr(QueryIn,Query),tr(SelectIn,Select), forall(reasoner_ask(Query),show(Select)).
qi QueryIn :- !,tr(QueryIn,Query), forall(reasoner_ask(Query),show(Query)).

cmd_doc(reasoner,init,[name],'Initialize a reasoner on current ont. E.g. "reasoner pellet.". Stores current reasoner in global variable.').
init (RN with Opts) :-
        !,
        print_message(informational,setting('Reasoner options',Opts)),
        initialize_reasoner(RN,R,Opts),
        print_message(informational,setting('Reasoner',R)).

init RN :-
        findall(Opt,settings(reasoner_opt,Opt),Opts),
        init(RN with Opts).

clio :-
        ensure_loaded(library(thea2/bin/thea_clio_startup)),
        cp_server.


% HELP
cmd_doc(help,?,[cmd],'Get help about a command.').
cmd_doc(help,h,[],'Show history.').
? X :- usage(X).
help :- usage.



set (P to V) :- !,unset(P), assert(settings(P,V)),save_settings.
set P + V :- !,assert(settings(P,V)),save_settings.
set P - V :- !,retractall(settings(P,V)),save_settings.
unset P :- retractall(settings(P,_)),save_settings.
settings :- forall(settings(P,V),writeln(P=V)).


%tr(In,Out) :- plsyn_owl(In,Out),!.
%tr(In,Out) :- plsyn_owl(In,X),map_IRIs(get_IRI_from_label,X,Out).
tr(In,Out) :- plsyn_owl(In,X),map_IRIs(label2iri,X,Out).

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
                    atom(P),
                    Opt =.. [P,V]),
                Opts).

current_ontology(Ont) :- nb_current(ontology,Ont),!.
current_ontology(Ont) :-
        ontology(Ont),
        !,
        set_current_ontology(Ont).
set_current_ontology(Ont) :-
        nb_setval(ontology,Ont),
        print_message(informational,shell('Set ontology to ~w'-[Ont])).

label2iri(Label,Obj) :- labelAnnotation_value(Obj,Label),!.
label2iri(Label,Obj) :- entity(Obj),atomic_list_concat([_,Label],'#',Obj).
label2iri(X,X).


shcol(Num) :-  format('\033['),write(Num),write(m).
shnocol :- shcol(0).

% ----------------------------------------
% MESSAGES
% ----------------------------------------

:- multifile prolog:message//1.
prolog:message(thea_shell_welcome) -->
               ['%  ::: Welcome to Posh, the Prolog OWL Shell :::'].

% TODO - colors
prolog:message(redo(Act)) --> ['Redo: '],prolog:message(Act).
prolog:message(undo(Act)) --> ['Undo: '],prolog:message(Act).
prolog:message(assert_axiom(A,Ont)) --> ['Asserting '],prolog:message(ax(A)),[' into ',Ont].
prolog:message(retract_axiom(A,Ont)) --> ['Retracting '],prolog:message(ax(A)),[' into ',Ont].
prolog:message(ax(Ax)) --> [Atom],{current_opts(Opts),with_output_to(atom(Atom),display_term(Ax,Opts))}.
prolog:message(shell(M)) --> [M].
prolog:message(setting(P,V)) --> ['Setting: '],[P],['=\t'],{term_to_atom(V,VA)},[VA].



usage :-
        writeln('BOMB Shell Commands'),
        forall(cmd_doc(_,C,_,Doc),
               format('~w\t~w~n',[C,Doc])),
        nl.
usage(C) :-
        cmd_doc(_,C,_,Doc),
        writeln(Doc).



