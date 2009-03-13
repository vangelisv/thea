/* -*- Mode: Prolog -*- */

:- module(owl2_util,
          [
           owl_parse_pro/1,
           owlx_file_to_prolog/1,
           rdf_file_to_prolog/1,
           rdf_file_to_prolog/2,
           write_owl_as_prolog/0,
           write_owl_as_plsyn/0,
           remove_namespaces/0,
           use_labels_for_IRIs/0,
           treeview/1,
           treeview/2
          ]).

:- use_module(owl2_model).
:- use_module(owl2_metamodel).
:- use_module(owl2_from_rdf).
:- use_module(owl2_xml).
:- use_module(owl2_basic_reasoner).

owl_parse_pro(F):-
        owl2_model:consult(F).


owlx_file_to_prolog(F):-
        owl_parse_xml(F),
        write_owl_as_prolog.

rdf_file_to_prolog(F):-
        owl_parse_rdf(F),
        write_owl_as_prolog.

rdf_file_to_prolog(F,Opts):-
        owl_parse_rdf(F,Opts),
        write_owl_as_prolog.

write_owl_as_prolog:-
        forall(axiompred(PS),
               write_axioms(PS)).

write_owl_as_plsyn:-
        ensure_loaded(owl2_plsyn),
        forall(axiompred(PS),
               write_axioms_as_plsyn(PS)).

write_axioms_as_plsyn(P/A):-
        !,
        functor(H,P,A),
        forall(H,(plsyn_owl(Pl,H),format('~q.~n',[Pl]))).


write_axioms(P/A):-
        !,
        functor(H,P,A),
        write_axioms(H).

write_axioms(H):-
        forall(H,format('~q.~n',[H])).

retract_axioms :-
        findall(A,axiom(A),Axioms),
        maplist(retract,Axioms).

remove_namespaces:-
        translate_IRIs(remove_ns).

use_labels_for_IRIs:-
        translate_IRIs(use_label_as_IRI).

remove_ns(IRI,X) :-
        concat_atom([_,X],'#',IRI).
use_label_as_IRI(IRI,X) :-
        entityLabel(IRI,X),
        !.
use_label_as_IRI(IRI,X) :-
        remove_ns(IRI,X).

translate_IRIs(Goal):-
        findall(A,axiom(A),Axioms),
        maplist(map_IRIs(Goal),Axioms,Axioms2),
        maplist(retract,Axioms),
        maplist(assert_axiom,Axioms2).

map_IRIs(G,X,X2) :-
        atom(X),
        call(G,X,X2),
        !.
map_IRIs(G,X,X2) :-
        X=..[F|Args],
        maplist(map_IRIs(G),Args,Args2),
        X2=..[F|Args2].

entityLabel(X,Label) :-
        annotationAssertion('http://www.w3.org/2000/01/rdf-schema#label', X, literal(type(_,Label))).
% TODO: deprecate
entityLabel(X,Label) :-
        propertyAssertion('http://www.w3.org/2000/01/rdf-schema#label', X, literal(type(_,Label))).

write_new_preds:-
        typedg(_,_),
        fail.

typedg(TG,G) :-
        export_list(owl2_model,EL),
        member(TP/Arity,EL),
        functor(TG,TP,Arity),
        clause(TG,(G,_)),
        owl2_model:axiompred(P/Arity),
        functor(G,P,Arity),
        (   owlpredicate_typed(P2,TP),
            P2\=P
        ->  format('**** ~w ~w ~w~n',[P,P2,TP])
        ;   true),
        \+ owlpredicate_typed(_,TP),
        format('~q.~n',[owlpredicate_typed(P,TP)]).

owlpredargs :-
        export_list(owl2_model,EL),
        member(TP/Arity,EL),
        functor(TG,TP,Arity),
        clause(TG,(_,subsumed_by(_,L))),
        \+ owlpredicate_arguments(TP,_),
        format('~q.~n',[owlpredicate_arguments(TP,L)]),
        fail.




treeview(Class) :-
        forall(treeview(Class,X-Y-subClassOf(X,Y),_,[]),
               true).

treeview(Class,Opts) :-
        member(traverse(P),Opts),
        !,
        forall(treeview(Class,
                        X-Y-(   subClassOf(X,Y)
                            ;   X=someValuesFrom(P,Y)),
                        _,
                        []),
               true).
treeview(Class,Opts) :-
        forall(treeview(Class,X-Y-subClassOf(X,Y),_,Opts),
               true).


%% treeview(+Class,+Template,?Tab,+Opts)
%
% @param Class IRI of class to work back from
% @param Template X-Y-Goal
% @param Tab
% @param Opts
treeview(Class,Template,Tab2,Opts) :-
        copy_term(Template,Class-Parent-Goal),
        (   Goal
        *-> treeview(Parent,Template,Tab,Opts),
            Tab2 = [' '|Tab],
            writetab(Tab2)
        ;   Tab2=[]),
        write_owl_class(Class,Opts),
        nl.

writetab([]).
writetab([_|L]):-
        write(' '),
        writetab(L).

write_owl_class(Class,_) :-
        entityLabel(Class,Label),
        !,
        write(Label).
write_owl_class(Class,_) :-
        write(Class).



/** <module> Various utility predicates for OWL ontologies

  ---+ Synopsis

==
:- use_module(bio(owl2_util)).

% 
demo:-
  nl.
  

==

---+ Details

This is a collection of very ad-hoc predicates for doing things with OWL.
This should not be regarded as stable.


---+ Additional Information



*/
