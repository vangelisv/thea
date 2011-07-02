:- use_module(library(thea/owl2_model)).
:- use_module(library(thea/owl2_io)).
:- use_module(library(thea/owl2_graph_reasoner)).

%% ontology_references_class_in(?O,?C,?O2)
% true of O contains an axiom that references C, and C is declared in O2
% (classes only for now, no individuals)
% e.g. ontology_references_class_in(obi,lung,fma)
ontology_references_class_in(O,C,O2) :-
        ontologyAxiom(O,Ax),
        axiom_references(Ax,C),
        class(C),
        ontologyAxiom(O2,class(C)),
        O2\=O.

%% mireot(LocalOnt,ExtOnt,Strategy)
% bring in all referenced classes from ExtOnt into LocalOnt
mireot(O,O2,Strategy) :-
        setof(ref(O,C,O2),
              ontology_references_class_in(O,C,O2),
              Refs),
        % we do not exclude self-pairs from the LCA which ensures we also
        % capture leaf nodes too
        findall(ref(O,CA,O2),
                mireot_extend_refs(O,O2,Refs,CA,Strategy),
                ExtRefs),
        setof(Ref,member(Ref,ExtRefs),ExtRefsU),
        forall(member(ref(O,C,O2),ExtRefsU),
               mireot_class(O,C,O2)).


mireot_extend_refs(O,O2,Refs,CA,lca) :-
        member(O-C1-O2,Refs),
        member(O-C2-O2,Refs),
        least_common_ancestor(C1,C2,CA).
%mireot_extend_refs(O,O2,Refs,CA,ancestor) :-
%        member(O-C-O2,Refs),
%        entailed(subClassOf(X,A)).

mireot_class(O,C,O2) :-
        assert_axiom(class(C),O),
        assert_axiom(annotationAssertion('http://purl.obolibrary.org/obo/OBI_0000283',C,O2),O), % imported_from
        format(user_error,'  Mireoted ~w~n',[C]).

mireot_files(InFile,OutFile,O) :-
        load_axioms(InFile,owl,[imports(true)]),
        (   var(O)
        ->  (   imports_closure_root(O)
            ->  true
            ;   throw('must specify ontology'))
        ;   true),
        format(user_error,'base ontology: ~w~n',[O]),
        forall(mireot(O,O2),
               format(user_error,'mireoted from ~w~n',[O2])),
        save_axioms(OutFile,owl,[ontology(O)]).

%% imports_closure_root(?O) is nondet
% may have zero solutions of ontologies mutually import (cf food+wine ontologies)
imports_closure_root(O) :-
        ontology(O),
        \+ ontologyImport(_,O).

% TODO: move to generic place
common_ancestor(X,Y,A) :-
  entailed(subClassOf(X,A)),
  entailed(subClassOf(Y,A)).

least_common_ancestor(X,Y,A) :-
  common_ancestor(X,Y,A),
  \+ ((common_ancestor(X,Y,A2),
       A2\=A,
       entailed(subClassOf(A2,A)))).

extract_ontology_subset(Obj,Ont) :-
        debug(mireot,'getting subset',[]),
        findall(P,class_ancestor(Obj,P),Set),
        debug(mireot,'finding axioms',[]),
        findall(Ax,(member(P,Set),
                    axiom_directly_about(Ax,P),
                    debug(mireot,'  checking: ~w',[Ax]),
                    forall((axiom_directly_references(Ax,P2),class(P2)),
                           member(P2,Set))),
               Axs),
        debug(mireot,'asserting axioms',[]),
        forall(member(Ax,Axs),
               assert_axiom(Ax,Ont)).

/** <module> referencing external ontology terms

---+ Synopsis

==
swipl -g "[mireot],mireot_files('http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine',
               'myfile.owl','http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine')"  
==

---+ Details

If an ontology has a large import chain then loading the full chain
into memory can be prohibitive.  Sometimes it is more preferable to
copy the class declaration from the referenced ontology and relevant
metadata into the source ontology.

See:
http://obi-ontology.org/page/MIREOT

*/

