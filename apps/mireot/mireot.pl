:- use_module(library('thea2/owl2_model')).
:- use_module(library('thea2/owl2_io')).
:- use_module(library('thea2/owl2_basic_reasoner')).

% classes only for now...
ontology_references_class_in(O,C,O2) :-
        ontologyAxiom(O,Ax),
        axiom_references(Ax,C),
        class(C),
        ontologyAxiom(O2,class(C)),
        O2\=O.

mireot(O,O2) :-
        setof(O-C-O2,
              ontology_references_class_in(O,C,O2),
              Refs),
        forall(member(O-C-O2,Refs),
               mireot_class(O,C,O2)).

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

