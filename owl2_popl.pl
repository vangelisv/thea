/* -*- Mode: Prolog -*- */

:- module(owl2_popl,
          [

           popl_translate/1,
           popl_translate/2,
           execute_popl_file/1,
           execute_popl_file/2,
           
           op(1100,xfy,===>),
           op(1000,xfy,where),
           op(1000,xfy,forall),
           op(990,fx,add)

          ]).

:- op(1100,xfy,===>).
:- op(1000,xfy,where).
:- op(1000,xfy,forall).
:- op(990,fx,add).

:- use_module(owl2_model).
:- use_module(owl2_plsyn).
:- use_module(owl2_util).
:- use_module(owl2_visitor).
:- use_module(owl2_reasoner).


%% popl_translate(+Directive) is det
% see popl_translate/2
popl_translate(T) :-
        popl_translate(T, [translate(labels)]).

%% popl_translate(+Directive,+Opts:list) is det
%
% Directive can be one of:
%  * InExpression ===> OutExpression
%  * InExpression ===> OutExpression where Goal
%  * add Expression where Goal
%
% OR a list of directives
%
% Opts can be:
%  * syntax(Syntax)
%     currently only val allowed is plsyn
popl_translate( T, Opts) :-
        forall(member(Opt,Opts),
               process_option(Opt)),
        preprocess_directives(T, T2, Opts),
        perform_translation(T2, Opts).

process_option(reasoner(R)) :- !,initialize_reasoner(R).
process_option(_).

preprocess_directives(L,L2,Opts) :-
        is_list(L),
        !,
        findall(X2,
                (   member(X,L),
                    once(preprocess_directive(X,X2,Opts))),
                L2).
preprocess_directives(X,X2,Opts) :-
        preprocess_directive(X,X2,Opts).

preprocess_directive( D forall G, Tr, Opts) :-
        !,
        G,
        preprocess_directive(D where true, Tr, Opts).
              
preprocess_directive( In ===> +(Out) where G, tr(_,In2,(In2,Out2),G2,Opts), Opts) :-
        nonvar(Out),
        nonvar(G),
        !,
        preprocess_term(In,In2,Opts),
        preprocess_term(Out,Out2,Opts),
        preprocess_term(G,G2,Opts).
preprocess_directive( In ===> +(Out), tr(_,In2,(In2,Out2),true,Opts), Opts) :-
        nonvar(Out),
        !,
        preprocess_term(In,In2,Opts),
        preprocess_term(Out,Out2,Opts).
preprocess_directive( In ===> Out where G, tr(_,In2,Out2,G2,Opts), Opts) :-
        nonvar(G),
        !,
        preprocess_term(In,In2,Opts),
        preprocess_term(Out,Out2,Opts),
        preprocess_term(G,G2,Opts).
preprocess_directive( In ===> Out , tr(_,In2,Out2,true,Opts), Opts) :-
        !,
        preprocess_term(In,In2,Opts),
        preprocess_term(Out,Out2,Opts).
preprocess_directive( add Out where G, null, Opts) :-
        % todo
        !,
        preprocess_term(Out,Out2,Opts),
        preprocess_term(G,G2,Opts),
        forall(G2,perform_directive(add, Out2,Opts)).
preprocess_directive( Term, _, _) :-
        print_message(error,invalid(Term)),
        fail.


preprocess_term(T,T3,Opts) :-
        \+ member(noplsyn,Opts),
        !,
        plsyn_owl(T,T2),
        preprocess_term(T2,T3,[noplsyn|Opts]).
preprocess_term(T,T3,Opts) :-
        select(translate(labels),Opts,Opts2),
        !,
        map_IRIs(get_IRI_from_label,T,T2),
        preprocess_term(T2,T3,Opts2).
preprocess_term(T,T3,Opts) :-
        select(translate(TG),Opts,Opts2),
        !,
        map_IRIs(TG,T,T2),
        preprocess_term(T2,T3,Opts2).

preprocess_term(T,T,_) :- !.

perform_directive(add, [],_) :- !.
perform_directive(add, [H|T],Opts) :-
        !, 
        perform_directive(add, H,Opts),
        perform_directive(add, T,Opts).
perform_directive(add, (H,T),Opts) :-
        !,
        perform_directive(add, H,Opts),
        perform_directive(add, T,Opts).
perform_directive(add, A, Opts) :-
        !,
        assert_axiom_wrap(A,Opts).

assert_axiom_wrap(A,Opts) :-
        select(post_translate(TG),Opts,Opts2),
        !,
        map_IRIs(TG,A,A2),
        assert_axiom_wrap(A2,Opts2).
assert_axiom_wrap(A,_) :-
        plsyn_owl(A,A2),
        assert_axiom(A2).







%% execute_popl_file(+File)
% see execute_popl_file/2,
execute_popl_file(F) :-
        execute_popl_file(F, []).


%% execute_popl_file(+File, +Opts:list)
execute_popl_file(F, Opts) :-
        read_file_to_terms(F,Terms,[]),
        findall(Opt,member(option(Opt),Terms),FileOpts),
        append(FileOpts,Opts,AllOpts),
        forall(member( (Head :- Goal),Terms),
               user:assert( (Head :- Goal) )),
        findall(Directive,(member(Directive,Terms),
                           \+Directive= ( _ :- _ ),
                           \+Directive=option(_)),
                Directives),
        popl_translate(Directives, AllOpts).



perform_translation(null, _) :- !. % add directives are handled up-front
perform_translation(X, Opts) :-
        memberchk(filter(AxiomTemplate,Goal),Opts),
        !,
        perform_translation(X, AxiomTemplate, Goal, Opts).
perform_translation(X, Opts) :-
        memberchk(filter(AxiomTemplate),Opts),
        !,
        perform_translation(X, AxiomTemplate, AxiomTemplate, Opts).
perform_translation(X, Opts) :-
        perform_translation(X, A, axiom(A), Opts). % default

%% perform_translation(+Tr, +AxiomTemplate, +AxiomGoal, +Opts)
perform_translation(X, AxiomTemplate, Goal, Opts) :-
        debug(popl,'collecting axioms... ~w',[X]),
        setof(AxiomTemplate,Goal,Axioms),
        !,
        perform_translation_on_axioms(X, Axioms, Opts).
perform_translation(_,_,_,_).

%% perform_translation_on_axioms(+Tr, +Axioms:list, +Opts:list)
perform_translation_on_axioms(X, Axioms, Opts) :-
        debug(popl,'performing: ~w',[X]),
        findall(A-A2,
                (   member(A,Axioms),
                    debug(popl_detail,'  axiom: ~w',[A]),
                    axiom_rewrite_list(A,X,A2),
                    A2\=[A],
                    A2\=[],
                    debug(popl_detail,'    REWRITE: ~w',[A2])
                    ),
                Pairs),
        replace_pairs(Pairs, Opts).

replace_pairs([],_) :- !.
replace_pairs([P|Ps],Opts) :-
        P = A-A2,
        debug(popl,'~w ==> ~w',[A,A2]),
        replace_pair(P,Opts),
        replace_pairs(Ps,Opts).

replace_pair(A-L,Opts) :-
        member(replacement_ontology(O),Opts),
        !,
        retract_axiom(A),
        forall(member(A2,L),
               assert_axiom(A2,O)).
replace_pair(A-L,_Opts) :-
        setof(O,ontologyAxiom(O,A),Os),
        !,
        retract_axiom(A),
        forall((member(A2,L),member(O,Os)),
               assert_axiom(A2,O)).
replace_pair(A-L,_Opts) :-
        % no ontology - orphan
        retract_axiom(A),
        !,
        forall(member(A2,L),
               assert_axiom(A2)).



% DEPRECATED
normalize_term(X,X) :- var(X),!.
normalize_term(X,X) :- atom(X),!.
normalize_term([],[]) :- !.
normalize_term(L,S) :-
        L=[_|_],
        !,
        sort(L,S).
normalize_term(propertyChain(L),propertyChain(L)) :-
        !. % order is important with property chains
normalize_term(T,T2) :-
        !,
        T=..[F|Args],
        maplist(normalize_term,Args,Args2),
        T2=..[F|Args2].


/** <module> OWL2 Prolog Ontology Processing Language

  ---+ Synopsis

==
use_module(library(thea2/owl2_io)).
use_module(library(thea2/owl2_popl)).

load_axioms('my_test_ont.owl').

% assert inverse annotation properties.
% normally a reasoner will infer inverse propertyAssertion/2 statements. Here we are assuming
% that you are taking liberties with OWL-DL and are declaring inverses for properties used in annotation
% assertions.
replace_matching_axioms_where(annotationAssertion(X,R,Y),annotationAssertion(Y,RInv,X),inverseProperties([R,RInv])).

% replace <overlaps some X> with <hasPart some partOf some X>.
% note that you can't do this with a reasoner
replace_expression_in_all_axioms(someValuesFrom(overlaps,X), someValuesFrom(hasPart,someValuesFrom(partOf,X))).
==

---+ Details

A simple prolog ontology processing language, inspired by OPPL.

  See:
  http://oppl2.sourceforge.net/

Note that this module is almost superfluous, as prolog is already a
very effective language for processing facts. However, this module
provides a useful high level structure for many common operations.

---++ Comparison with OPPL2

OPPL/OPPL2 does not provide an equivalent to
replace_expression_in_all_axioms/3, it only allows axiom replacement.

---+++ Adding disjoint axioms
  
  
OPPL2:
  
==  
?x:CLASS, ?y:CLASS
SELECT ?x subClassOf gender,
?y subClassOf gender
WHERE ?x!=?y
BEGIN
ADD ?x disjointWith ?y
END;  
==  

Prolog:
  
==
add_axioms_where(disjointWith(X,Y),
  (subClassOf(X,'http://foo.org#gender'),subClassOf(Y,'http://foo.org#gender'))).
==

---+++ Country example

  http://oppl2.sourceforge.net/ontologies/roberts.owl
  
OPPL2:
  
==  
?country:INDIVIDUAL[instanceOf Country], ?adiacentCountry:INDIVIDUAL[instanceOf Country]
SELECT ?country adjacentTo ?adiacentCountry
BEGIN
REMOVE ?country adjacentTo ?adiacentCountry,
ADD ?country instanceOf hasLandBoundary some (LandBoundaryFragment
and boundaryOf value ?adiacentCountry)
END;  
==

Prolog:
  
==
use_module(owl2_plsyn).
replace_matching_axioms_where(
  propertyAssertion(adjacentTo,X,Y),
  classAssertion( hasLandBoundary some ('LandBoundaryFragment' and boundaryOf value Y), X),
  [syntax(plsyn)]
).
==

---++ Arbitrary prolog code

Prolog is more expressive than OPPL2, as arbitrary prolog code and
builtins can be used. This includes arithmetic, string processing,
etc. It's not clear if OPPL2 supplies any of these features.

---++ Command Line examples  

Make every sibling pair disjoint:  
==  
thea --popl "add disjointClasses(X,Y) where (subClassOf(X,A),subClassOf(Y,A),X\=Y)" testfiles/caro.owl --to owl  
==  

What if we want to restrict this to a certain set of classes - for
example, all asserted sibling pairs anywhere under "epithelium". We
can use a reasoner to find all subclasses of epithelium.
  
The following invokes pellet via the OWL API:
  
==  
thea-jpl --reasoner pellet --popl "add disjointClasses([X,Y]) where (labelAnnotation_value(E,epithelium),reasoner_ask(reflexiveSubClassOf(A,E)),subClassOf(X,A),subClassOf(Y,A),X\=Y)" testfiles/caro.owl --to owlpl
==

The above makes pairwise disjointness axioms - with OWL2 we can state this as a list. This is even easier:  
  
==  
thea-jpl --reasoner pellet --popl "add disjointClasses(L) where labelAnnotation_value(E,epithelium),reasoner_ask(reflexiveSubClassOf(A,E)),setof(X,subClassOf(X,A),L)" testfiles/caro.owl --to owlpl   
==  

You can also load all directives into a file:
  
==  
thea-jpl --reasoner pellet --popl-file testfiles/epithelium.popl testfiles/caro.owl --to owlpl
==
  
---+ TODO

* Add a grammar for OPPL2
  
*/
