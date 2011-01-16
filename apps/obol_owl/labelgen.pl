/* -*- Mode: Prolog -*- */

:- use_module(library('thea2/owl2_model')).
:- use_module(library('thea2/owl2_plsyn')).
:- use_module(library('thea2/owl2_io')).
:- use_module(library('thea2/owl2_reasoner')).

:- multifile user:parse_arg_hook/3.
:- multifile user:arg_info_hook/3.
user:parse_arg_hook(['--grammar',Gr|L],L,goal(consult(apps/obol_owl/grammars/Gr))).
user:parse_arg_hook(['--obol-parse',Label|L],L,
                    query(X,parse_label_to_expression(Label,X))).
user:parse_arg_hook(['--obol-parse-all'|L],L,
                    query(equivalentTo(E,X),
                          (   class(E),
                              labelAnnotation_value(E,Label),
                              parse_label_to_expression(Label,X)))).

/*
  EXAMPLE:
  
  thea --load-app obol_owl/labelgen --grammar dcg_anatomy testfiles/fly_anatomy_XP.owlpl --goal generate_all_grammar_terminals --obol-parse-all --save-opts plsyn,labels

*/

:- multifile term//1.
:- multifile primitive//1.
:- dynamic primitive//1.
term(X) --> primitive(X).


class_label(C,Label) :-
        equivalent_to(C,Expr),
        expression_label(Expr,Label).

expression_label(Expr,Label) :-
        plsyn_owl(ExprX,Expr),
        phrase(term(ExprX),Toks,[]),
        concat_atom(Toks,' ',Label).

parse_label_to_expression(Label,Expr) :-
        concat_atom(Toks,' ',Label),
        phrase(term(ExprX),Toks,[]),
        \+atom(ExprX),
        plsyn_owl(ExprX,Expr).

/*
term(E,ToksD,Tail) :-
        labelAnnotation_value(E,Label),
        atomic_list_concat(Toks,' ',Label),
        append(Toks,Tail,ToksD). % make a difference list
*/

generate_all_grammar_terminals :-
        generate_all_grammar_terminals(['http://www.w3.org/2000/01/rdf-schema#label']).

generate_all_grammar_terminals(Props) :-
        findall(E-Label,(member(P,Props),
                         anyPropertyAssertion(P,E,Literal),
                         rdf_literal_to_atom(Literal,Label)),
                Pairs),
        forall(member(E-Label,Pairs),
               generate_grammar_terminal(E,Label)).

generate_grammar_terminal(E,Label) :-
        atomic_list_concat(Toks,' ',Label),
        append(Toks,Tail,ToksD), % make a difference list
        assert(primitive(E,ToksD,Tail)).

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

