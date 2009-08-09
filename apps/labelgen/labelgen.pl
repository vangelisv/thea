:- use_module(library('thea2/owl2_model')).
:- use_module(library('thea2/owl2_plsyn')).
:- use_module(library('thea2/owl2_io')).
:- use_module(library('thea2/owl2_basic_reasoner')).

:- multifile term//1.

class_label(C,Label) :-
        equivalent_to(C,Expr),
        plsyn_owl(ExprX,Expr),
        phrase(term(ExprX),Toks,[]),
        concat_atom(Toks,' ',Label).

use_ids_as_labels :-
        forall(class(C),
               assert_axiom(annotationAssertion('http://www.w3.org/2000/01/rdf-schema#label', C, literal(C)))).

t :-
        load_axioms('testont_phenotype.pl'),
        consult('grammars/phenotype.pl'),
        use_ids_as_labels,
        forall(class_label(P,Label),
               format('~w => ~w~n',[P,Label])).


/** <module> label generation from class expressions

---+ Synopsis

==
swipl -g "[labelgen],t"
==

---+ Details

This is a rewrite of Obol to use OWL.

See
http://wiki.geneontology.org/index.php/Obol

*/

