/* -*- Mode: Prolog -*- */

:- module(swrl_rdf_hooks,
          [
           ]).

:- use_module(library('semweb/rdf_db.pl')).
:- use_module(swrl).
:- use_module(owl2_from_rdf).
:- use_module(owl2_model).
:- use_module(owl2_metamodel).

:- rdf_register_ns(swrl,'http://www.w3.org/2003/11/swrl#',[force(true)]).

:- multifile owl2_from_rdf:owl_parse_axiom(X).
owl2_from_rdf:owl_parse_axiom(X) :-
        debug(owl2,'trying swrl hooks for: ~w',[X]),
        owl_parse_axiom(X),
        !.

use_owl(A,B,C) :-
        owl2_from_rdf:use_owl(A,B,C).
test_use_owl(A,B,C) :-
        owl2_from_rdf:test_use_owl(A,B,C).

owl_parse_axiom(swrl:implies(Body,Head)) :-
        use_owl(X,'rdf:type','swrl:Imp'), % TODO: named rules
        !,
        use_owl(X,'swrl:body',RdfBody),
        use_owl(X,'swrl:head',RdfHead),
        swrl_description_list(RdfBody,Body),
        swrl_description_list(RdfHead,Head).

swrl_description_list('http://www.w3.org/1999/02/22-rdf-syntax-ns#nil',[]) :- !.
swrl_description_list(X,[F|R]) :-
	% use_owl(X,'rdf:type','rdf:List'), -- required?
	use_owl(X,'rdf:first',Element),
	swrl_description(Element,F),
	use_owl(X,'rdf:rest',Y),
	!,
        swrl_description_list(Y,R).

swrl_description(X,i(X)) :-
        test_use_owl(X,'rdf:type','swrl:Variable'), % do not consume
        !.
swrl_description(X,G) :-
        use_owl(X,'rdf:type','swrl:IndividualPropertyAtom'),
        use_owl(X,'swrl:propertyPredicate',P),
        use_owl(X,'swrl:argument1',A1),
        use_owl(X,'swrl:argument2',A2),
        swrl_description(P,PP),
        swrl_description(A1,A1P),
        swrl_description(A2,A2P),
        G=..[PP,A1P,A2P].
swrl_description(X,X) :- !.



/** <module> additional RDF parsing of SWRL rules

  ---+ Synopsis

==
[owl2_from_rdf].
[swrl].
[swrl_rdf_hooks].
owl_parse_rdf('testfiles/dl-safe-ancestor.owl').
==

---+ Details





*/
