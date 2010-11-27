/* -*- Mode: Prolog -*- */

:- module(owl2_plsyn,[
                      write_owl_as_plsyn/0,
                      write_owl_as_plsyn/1,

                      plsyn_owl/2,
                      plsyn_owl/3,
                      
                      op(980,xfy,(--)),
                      op(950,xfy,\^), % disjoint classes
                      %op(950,fx,class),
                      op(950,fx,individual),
                      op(950,xfy,disjointUnion),
                      op(950,fx,class),
                      op(950,fx,functional),
                      op(950,fx,transitive),
                      op(950,fx,symmetric),
                      op(950,fx,asymmetric),
                      op(950,fx,reflexive),
                      op(950,fx,irreflexive),
                                % 700 <
                                % 700 =
                      op(700,xfy,inverseOf),
                      %op(700,xfy,(->)),
                      op(650,xfy,(::)),
                      op(600,fx,not),
                      op(500,xfy,or),
                      op(200,xfy,and),
                      op(200,xfy,that),
                      op(150,xfy,some),
                      op(150,xfy,only),
                      op(150,xfy,value),
                      op(150,xfy,min),
                      op(150,xfy,max),
                      op(150,xfy,exactly),
                      op(125,xfy,of) % required for QCRs
                     ]).


:- use_module(owl2_model).
:- use_module(owl2_util).
:- use_module(swrl).
:- use_module(library(readutil)).

:- op(980,xfy,(--)).
:- op(950,fx,individual).

:- op(950,xfy,disjointUnion).

:- op(950,xfy,\^). % disjoint classes

:- op(950,fx,class).
:- op(950,fx,functional).
:- op(950,fx,transitive).
:- op(950,fx,symmetric).
:- op(950,fx,asymmetric).
:- op(950,fx,reflexive).
:- op(950,fx,irreflexive).
% 700 <
% 700 =
:- op(700,xfy,inverseOf).
%:- op(700,xfy,(->)).
:- op(650,xfy,(::)).
:- op(600,fx,not).
:- op(500,xfy,or).
:- op(200,xfy,and).
:- op(200,xfy,that).
:- op(150,xfy,some).
:- op(150,xfy,only).
:- op(150,xfy,value).
:- op(150,xfy,min).
:- op(150,xfy,max).
:- op(150,xfy,exactly).
:- op(125,xfy,of).
:- op(100,fx,(?)).

:- multifile owl2_io:load_axioms_hook/3.
owl2_io:load_axioms_hook(File,plsyn,Opts) :-
        owl_parse_plsyn(File,Opts). % TODO

:- multifile owl2_io:save_axioms_hook/3.
owl2_io:save_axioms_hook(_File,plsyn,Opts) :-
        write_owl_as_plsyn(Opts).

owl_parse_plsyn(File,_Opts) :-
        open(File,read,IO,[]),
        repeat,
        (   at_end_of_stream(IO)
        ->  true
        ;   read_term(IO,PlTerm,[module(owl2_plsyn)]),
            plsyn2owl(PlTerm,Axiom),
            (   nb_current(ontology,Ont)
            ->  assert_axiom(Axiom,Ont)
            ;   assert_axiom(Axiom)),
            (   Axiom=ontology(OntNew)
            ->  nb_setval(ontology,OntNew)
            ;   true),
            fail),
        close(IO).

write_owl_as_plsyn:-
        write_owl_as_plsyn([]).

write_owl_as_plsyn(Opts):-
	setof(Ont,member(ontology(Ont),Opts),Onts),
	!,
	% this clause optimized for ontology filtering
        forall((member(Ont,Onts),
		ontologyAxiom(Ont,A),
		\+exclude_axiom(A,Opts)),
	       (   plsyn_owl(Pl,A,Opts),
		   format('~q.~n',[Pl]))).
write_owl_as_plsyn(Opts):-
        forall((axiom(A),\+exclude_axiom(A,Opts)),
	       (   plsyn_owl(Pl,A,Opts),
		   format('~q.~n',[Pl]))).


% TODO: move somewhere generic
exclude_axiom(H,Opts) :-
	setof(Ont,member(ontology(Ont),Opts),Onts),
	\+ ((ontologyAxiom(Ont,H),
	     member(Ont,Onts))).

%% plsyn_owl(?Pl,?Owl)
% as plsyn_owl/3
plsyn_owl(Pl,Owl) :-
	plsyn_owl(Pl,Owl,[]).

%% plsyn_owl(?Pl,?Owl,+Opts:list)
%
% convert between a plsyn prolog term and an owl2_model.pl prolog term.
% either one of Pl or Owl must be ground
plsyn_owl(Pl,Owl,Opts) :-
	select(use_labels,Opts,Opts2),
	!,
	map_IRIs(owl2_util:use_label_as_IRI,[Owl],[Owl2]),
	plsyn_owl(Pl,Owl2,Opts2).
plsyn_owl(Pl,Owl,_) :-
        nonvar(Pl),
        plsyn2owl(Pl,Owl),
        !.
plsyn_owl(Pl,Owl,_) :-
        nonvar(Owl),
        owl2plsyn(Owl,Pl),
        !.
plsyn_owl(Pl,Pl,_) :-
        var(Pl).



% allow translation of vars, for example for queries or templates
plsyn2owl(V,V) :-
        var(V),
        !.

% e.g. r < r1 * r2 *r3 ...
plsyn2owl(R @< R1*R2,subPropertyOf(propertyChain(Chain),R)) :-
        plsyn2owl_ec(R1*R2,(*),Chain),
        !.

plsyn2owl(Pl,Owl) :-
        Pl=..[PlPred,PlProp,of(Num,PlC)],
        cardinality_pred(PlPred),
        plpred2owlpred(PlPred,OwlPred),
        !,
        plsyn2owl(PlProp,Prop),
        plsyn2owl(PlC,C),
        Owl=..[OwlPred,Num,Prop,C].
plsyn2owl(Pl,Owl) :-
        Pl=..[PlPred|Args],
        plpred2owlpred(PlPred,OwlPred),
        !,
        maplist(plsyn2owl,Args,Args2),
        Owl=..[OwlPred|Args2].
plsyn2owl(Pl,Owl) :-
        Pl=..[PlPred|Args],
        plpred2owlpred_list(PlPred,OwlPred), % TODO - reverse
        !,
        maplist(plsyn2owl,Args,Args2),
        Owl=..[OwlPred,[Args2]].

% TODO: entity annotations
plsyn2owl(Ax--Comments,[PlAx,axiomAnnotation('rdfs:comment',literal(Comments))]) :-
        !,
        plsyn2owl(Ax,PlAx).

% we can chain over a=b=c=d as equivalent/sameAs is transitive
% (note we cannot do this for different/disjoint)
plsyn2owl(A=B,sameIndividual(ECs)) :-
        !,
        plsyn2owl_ec(A=B,(=),ECs).
plsyn2owl(A==B,equivalentClasses(ECs)) :-
        !,
        plsyn2owl_ec(A==B,(==),ECs).
plsyn2owl(A=@=B,equivalentProperties(ECs)) :-
        !,
        plsyn2owl_ec(A=@=B,(=@=),ECs).
plsyn2owl(A and B,intersectionOf(ECs)) :-
        !,
        plsyn2owl_ec(A and B,and,ECs).
plsyn2owl(A \^ B,disjointClasses(ECs)) :-
        !,
        plsyn2owl_ec(A \^ B,\^,ECs).
plsyn2owl(A or B,unionOf(ECs)) :-
        !,
        plsyn2owl_ec(A or B,or,ECs).
plsyn2owl(Pl,Owl) :-
        % Assume OwlPred is valid, translate sub-args
        Pl=..[OwlPred|Args],
        Args\=[],
        !,
        maplist(plsyn2owl,Args,Args2),
        Owl=..[OwlPred|Args2].
plsyn2owl(X,X) :- !.


%% plsyn2owl_ec(+Term,+Op,?Elts:list)
% e.g. a and b and c and V ==> [a,b,c,V]
plsyn2owl_ec(T,_,[T]) :-
        var(T),
        !.
plsyn2owl_ec(T,Op,L) :-
        T=..[Op,A,B],
        !,
        plsyn2owl_ec(A,Op,LA),
        plsyn2owl_ec(B,Op,LB),
        append(LA,LB,L).
plsyn2owl_ec(A,_,[AX]) :-
        plsyn2owl(A,AX).

owl2plsyn(Owl,Pl) :-
        Owl=..[OwlPred|Args],
        plpred2owlpred(PlPred,OwlPred),
        !,
        maplist(owl2plsyn,Args,Args2),
        Pl=..[PlPred|Args2].
owl2plsyn(Owl,Pl) :-
        Owl=..[OwlPred|Args],
        plpred2owlpred_list(PlPred,OwlPred),
        !,
        maplist(owl2plsyn,Args,Args2),
        Pl=..[PlPred,[Args2]].
owl2plsyn(propertyAssertion(P,S,O),Pl) :-
        atom(P),
        op(999,xfy,P),
        owl2plsyn(S,SX),
        owl2plsyn(O,OX),
        Pl=..[P,SX,OX].
owl2plsyn(equivalentProperties(Args),Pl) :-
        maplist(owl2plsyn,Args,Args2),
        list_to_chain(Args2,(=@=),Pl).
owl2plsyn(equivalentClasses(Args),Pl) :-
        maplist(owl2plsyn,Args,Args2),
        list_to_chain(Args2,(==),Pl).
owl2plsyn(sameIndividuals(Args),Pl) :-
        maplist(owl2plsyn,Args,Args2),
        list_to_chain(Args2,(=),Pl).
owl2plsyn(intersectionOf(Args),Pl) :-
        % sort atoms first for aesthetic reasons: prefer <a and (r some b)> over <(r some b) and a>
        sort(Args,ArgsSorted),
        maplist(owl2plsyn,ArgsSorted,Args2),
        list_to_chain(Args2,and,Pl).
owl2plsyn(disjointClasses(Args),Pl) :-
        maplist(owl2plsyn,Args,Args2),
        list_to_chain(Args2,\^,Pl).
owl2plsyn(unionOf(Args),Pl) :-
        maplist(owl2plsyn,Args,Args2),
        list_to_chain(Args2,or,Pl).
owl2plsyn(implies(A,C),(A2->C2)) :-
        swrlatoms2plsyn(A,A2),
        swrlatoms2plsyn(C,C2).
owl2plsyn(literal(type(_,X)),X) :- !.
owl2plsyn(literal(X),X) :- atom(X),!.
owl2plsyn(Owl,Pl) :-
        Owl=..[P|Args],
        Args\=[],
        !,
        maplist(owl2plsyn,Args,Args2),
        Pl=..[P|Args2].
owl2plsyn(X,X) :- !.

swrlatoms2plsyn(A,A2) :-
        is_list(A),
        !,
        maplist(swrlatom2plsyn,A,AL),
        list_to_chain(AL,(,),A2).
swrlatoms2plsyn(A,A2) :-
        !,
        swrlatom2plsyn(A,A2).

swrlatom2plsyn(description(CE,I),H) :-
        !,
        swrlatom2plsyn(I,I2),
        H=..[CE,I2].
swrlatom2plsyn(differentFrom(X,Y),X2 \= Y2) :-
        !,
        swrlatom2plsyn(X,X2),
        swrlatom2plsyn(Y,Y2).
swrlatom2plsyn(IPA,IPA2) :-
        IPA=..[P,X,Y],
        !,
        swrlatom2plsyn(X,X2),
        swrlatom2plsyn(Y,Y2),
        IPA2=..[P,X2,Y2].

% TODO -- decide on correct functor for variables
swrlatom2plsyn(v(V),X) :- !, swrlatom2plsyn(i(V),X).
swrlatom2plsyn(i(V),X) :- number(V),!,VA is V+96,atom_codes(A,[VA]),atom_concat('?',A,X).
swrlatom2plsyn(i(V),X) :- atom_concat('?',V,X).
swrlatom2plsyn(X,X) :- !.



list_to_chain([X],_,Pl) :- !, owl2plsyn(X,Pl).
list_to_chain([X1|L],Op,Pl) :-
        !,
        list_to_chain(L,Op,X2),
        owl2plsyn(X1,X1Pl),
        Pl=..[Op,X1Pl,X2].


plpred2owlpred(transitive,transitiveProperty).
plpred2owlpred(functional,functionalProperty).
plpred2owlpred(symmetric,symmetricProperty).
plpred2owlpred(reflexive,reflexiveProperty).

%plpred2owlpred(inverseOf,inverseProperties).

plpred2owlpred(min,minCardinality).
plpred2owlpred(max,maxCardinality).
plpred2owlpred(exact,exactCardinality).

plpred2owlpred(some,someValuesFrom).
plpred2owlpred(only,allValuesFrom).
plpred2owlpred(value,hasValue).
plpred2owlpred(not,complementOf).


plpred2owlpred(inverseOf,inverseProperties).

plpred2owlpred(::,classAssertion).
plpred2owlpred(<,subClassOf).
plpred2owlpred(@<,subPropertyOf).

plpred2owlpred_list(\=,differentIndividuals). 
%plpred2owlpred_list(\=,disjointClasses). 

cardinality_pred(min).
cardinality_pred(max).
cardinality_pred(exact).


/** <module> prolog-style syntactic sugar for OWL

  ---+ Synopsis

Ontologies can be authored or written in prolog syntax, with
convenience predicates declared infix in order to resemble Manchester
Syntax.

The following is a valid plsyn file:

==
ontology(spicy).
spicy_tomato_pizza == pizza and hasPart some (topping and hasQuality some spicy) and hasPart some tomato.
pizza < hasPart some mozzarella.
pizza_with_4_cheeses == pizza and hasPart exactly 4 of cheese.
==

This file can be loaded via load_axioms/2 like this:

==
load_axioms('myfile.plsyn',plsyn).
==

---+ Details

The class expression syntax should resembly manchester syntax as far
as possible. Additional symbols such as <, =, ==, @< are also used for
axioms.

We are forced to introduce an extra keyword 'of' for cardinality
expressions to make this parseable by prolog.

TODO: show translation table

---++ Infix Predicate Symbols for Axioms

  * < --- subClassOf/2
  * @< --- subPropertyOf/2
  * == --- equivalentClasses/1
  * = --- sameIndividual/1
  * \= --- differentIndividuals/1
  * :: --- classAssertion/2
 
---++ Property Characteristic Prefix Predicates

the following are all declared prefix

  * transitive --- transitiveProperty/1
  * symmetric --- symmetricProperty/1
  * reflexive --- reflexiveProperty/1
  * functional --- functionalProperty/1

---++ Other Infix Predicates

  * inverseOf --- inverseProperties/2

---++ Class Expressions

  * and --- intersectionOf
  * or --- unionOf
  * some --- someValuesFrom
  * only --- allValuesFrom
  * value --- hasValue
  * not --- complementOf (prefix)
  * exactly --- exactCardinality
  * min --- minCardinality
  * max --- maxCardinality
  * of --- required when making QCRs. E.g. exactly 5 of finger

 ---++ Property Expressions

 Use the '*' symbol. For example

 ==
 uncleOf @< fatherOf * brotherOf.
 ==

 ---++ Uses

 plsyn makes it easier to mix OWL ontologies and ontology templates into prolog programs.

For example, consider the following failure-driven loop to add an OWL
axiom with a complex nested expression for all facts of a 4-argument predicate:

==
generate_ownership_assertions :-
        owns(Person,Number,Thing,Loc),
        plsyn_owl( owns exactly Number of (Thing and located_in value Loc) :: Person, OwlAxiom),
        assert_axiom(OwlAxiom),
        fail.
==

The owl2_popl.pl module facilitates this kind of translation

*/
