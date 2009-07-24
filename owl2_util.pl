/* -*- Mode: Prolog -*- */

:- module(owl2_util,
          [
           owl_parse_pro/1,
           owlx_file_to_prolog/1,
           rdf_file_to_prolog/1,
           rdf_file_to_prolog/2,
           write_owl_as_prolog/0,
           remove_namespaces/0,
           remove_ns/2,
           use_labels_for_IRIs/0,
           prefix_IRIs/1,
           translate_IRIs/1,
           map_IRIs/3,
           use_class_labels_as_synonyms/1,
           class_label_synonym_axiom/2,
           write_ontology_summary/0,
           treeview/1,
           treeview/2
          ]).

:- use_module(swrl). % required for retracting swrl rules
:- use_module(owl2_model).
:- use_module(owl2_metamodel).
:- use_module(owl2_from_rdf).
:- use_module(owl2_xml).
:- use_module(owl2_basic_reasoner).

% @Deprecated
% use owl2_io
owl_parse_pro(F):-
        owl2_model:consult(F).


% @Deprecated
% use owl2_io
owlx_file_to_prolog(F):-
        owl_parse_xml(F),
        write_owl_as_prolog.

% @Deprecated
% use owl2_io
rdf_file_to_prolog(F):-
        owl_parse_rdf(F),
        write_owl_as_prolog.

% @Deprecated
% use owl2_io
rdf_file_to_prolog(F,Opts):-
        owl_parse_rdf(F,Opts),
        write_owl_as_prolog.

% @Deprecated
% use owl2_io
write_owl_as_prolog:-
        forall(axiompred(PS),
               write_axioms(PS)).


write_axioms(P/A):-
        !,
        functor(H,P,A),
        write_axioms(H).

write_axioms(H):-
        forall(H,format('~q.~n',[H])).

% @Deprecated
% use retract_all_axioms/0 in owl2_model
retract_axioms :-
        findall(A,axiom(A),Axioms),
        maplist(retract,Axioms).

%% remove_namespaces
% calls translate_IRIs/1 with remove_ns/2 as argument,
% which has the effect of stripping URI prefixes from all
% names. Note that we cannot then write the results
% straight out as RDF since our identifiers are no longer IRIs.
% however, namespace-stripped simple atom identifiers are
% useful when working with prolog.
%
% @see prefix_IRIs/1 for the converse operation
remove_namespaces:-
        translate_IRIs(remove_ns).

%% prefix_IRIs(+NS)
% attaches a prefix to all names.
prefix_IRIs(X):-
        translate_IRIs(prefix_IRI(X)).

%% use_labels_for_IRIs/0
% uses rdfs:label if available, if not uses local part of URI.
% e.g.
% if we have axioms like subClassOf('http://x.org#1','http://x.org#2'),
% and label annotationAssertion/2 axioms, then this we be replaced by
% subClassOf(human,animal) etc
use_labels_for_IRIs:-
        translate_IRIs(use_label_as_IRI).

remove_ns(IRI,X) :-
        concat_atom([_,X],'#',IRI),
        !.
remove_ns(X,X).


use_label_as_IRI(IRI,X) :-
        labelAnnotation_value(IRI,X),
        !.
use_label_as_IRI(IRI,X) :-
        remove_ns(IRI,X),
        !.
use_label_as_IRI(X,X).

prefix_IRI(Pre,X,Y) :-
        (   entity(X) ; ontology(X)),
        !,
        atom_concat(Pre,X,Y).
prefix_IRI(_,X,X) :- !.


%% translate_IRIs(+Goal)
% Goal must be a 2 argument predicate Goal(+IRI,?TranslatedAtom)
% all IRIs are translated. The result need not be a valid IRI, it
% can be any prolog atom (possibly compound term as well)
:- module_transparent translate_IRIs/1.
translate_IRIs(Goal):-
        findall(A,axiom(A),Axioms),
        maplist(map_IRIs(Goal),Axioms,Axioms2),
        maplist(retract,Axioms),
        maplist(assert_axiom,Axioms2).

:- module_transparent map_IRIs/3.
map_IRIs(_,[],[]) :- !.
map_IRIs(G,[X|Xs],[X2|X2s]) :-
        !,
        map_IRIs(G,X,X2),
        map_IRIs(G,Xs,X2s).
map_IRIs(G,X,X2) :-
        atom(X),
        call(G,X,X2),
        !.
map_IRIs(G,X,X2) :-
        X=..[F|Args],
        Args\=[],
        !,
        maplist(map_IRIs(G),Args,Args2),
        call(G,F,F2),           % swrl axioms use IRIs as functors
        X2=..[F2|Args2].
map_IRIs(G,X,X2) :-
        call(G,X,X2),
        !.

%% use_class_labels_as_synonyms(+NS)
% sometimes ontologies have opaque numeric IRIs.
% sometimes it is convenient to work with a knowledge base in
% which the labels are used. We can get the reasoner to combine
% these by stating equivalentClasses/2 axioms.
use_class_labels_as_synonyms(NS) :-
        forall(labelAnnotation_value(IRI,X),
               (   atom_concat(NS,X,C),
                   assert_axiom(class(C)),
                   assert_axiom(equivalentClasses([C,IRI])))).

class_label_synonym_axiom(NS,Ax) :-
        entity(IRI),
        labelAnnotation_value(IRI,X),
        atom_concat(NS,X,C),
        (   Ax=class(C)
        ;   Ax=equivalentClasses([C,IRI])).



write_ontology_summary:-
        forall(axiompred(PS),
               write_axiom_summary(PS)).

write_axiom_summary(P/A) :-
        functor(H,P,A),
        aggregate(count,H,H,Count),
        format('Axiom: ~w count = ~w',[P,Count]).

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
        labelAnnotation_value(Class,Label),
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
