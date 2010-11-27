/* -*- Mode: Prolog -*- */

:- module(owl2_popl,
          [
           replace_matching_axioms/2,
           replace_matching_axioms/3,
           replace_matching_axioms_where/3,
           replace_matching_axioms_where/4,
           replace_expression_in_all_axioms/2,
           replace_expression_in_all_axioms/3,

           popl_translate/1,
           popl_translate/2,
           execute_popl_file/1,
           execute_popl_file/2,
           
           op(1100,xfy,===>),
           op(1000,xfy,where),
           op(950,fx,add)

          ]).

:- op(1100,xfy,===>).
:- op(1000,xfy,where).
:- op(950,fx,add).

:- use_module(owl2_model).
:- use_module(owl2_plsyn).


%% popl_translate(+Directive) is det
% see popl_translate/2
popl_translate(T) :-
        popl_translate(T, []).

%% popl_translate(+Directive,+Opts:list) is det
%
% Directive can be one of:
%  * InExpression ===> OutExpression
%  * InExpression ===> OutExpression where Goal
%  * add Expression where Goal
%
% Opts can be:
%  * syntax(Syntax)
%     currently only val allowed is plsyn
popl_translate( X1 ===> X2 where G, Opts) :-
        debug(popl,'Replacing axioms',[]),
        %replace_matching_axioms_where(X1,X2,G,Opts),
        debug(popl,'Replacing expressions',[]),
        replace_expression_in_all_axioms_where(X1,X2,G,Opts).

popl_translate( X1 ===> X2, Opts) :-
        debug(popl,'translating ~w ===> ~w',[X1,X2]),
        debug(popl,'Replacing axioms',[]),
        replace_matching_axioms(X1,X2,Opts),
        debug(popl,'Replacing expressions ~w ===> ~w',[X1,X2]),
        replace_expression_in_all_axioms(X1,X2,Opts).

popl_translate( add X2 where G, Opts) :-
        replace_matching_axioms_where(true,X2,G,Opts),
        replace_expression_in_all_axioms_where(true,X2,G,Opts).

%% execute_popl_file(+File)
% see execute_popl_file/2,
execute_popl_file(F) :-
        execute_popl_file(F, []).


%% execute_popl_file(+File, +Opts:list)
execute_popl_file(F, Opts) :-
        read_file_to_terms(F,Directives,[]),
        forall(member(Directive,Directives),
               popl_translate(Directive, Opts)).

%% replace_matching_axioms(+AxiomTemplateOld,+AxiomTemplateNew,+Opts:list) is det
% replace all occurrences of AxiomTemplateOld with AxiomTemplateNew.
%
% if Opts contains copy(true) then a copy of the old one is retained.
replace_matching_axioms(Ax1,Ax2,Opts) :-
        forall(axiom(Ax1),replace_axiom(Ax1,Ax2,Opts)).

%% replace_matching_axioms(+AxiomTemplateOld,+AxiomTemplateNew) is det
% as replace_matching_axioms/3, default options
replace_matching_axioms(Ax1,Ax2) :-
        replace_matching_axioms(Ax1,Ax2,[]).

%% replace_matching_axioms_where(+AxiomTemplateOld,+AxiomTemplateNew,+WhereGoal,+Opts:list) is det
% replace all occurrences of AxiomTemplateOld with AxiomTemplateNew for all WhereGoal.
% Opts as for replace_axiom/3
replace_matching_axioms_where(Ax1a,Ax2a,Ga,Opts) :-
        select(syntax(plsyn),Opts,Opts2),
        !,
        plsyn_owl(Ax1a,Ax1),
        plsyn_owl(Ax2a,Ax2),
        plsyn_owl(Ga,G),
        replace_matching_axioms_where(Ax1,Ax2,G,Opts2).
replace_matching_axioms_where(Ax1,Ax2,G,Opts) :-
        member(reasoner(R),Opts),
        !,
        ensure_loaded(owl2_reasoner),
        forall((reasoner_ask(R,G),
                Ax1),
               replace_axiom(Ax1,Ax2,Opts)),
        forall((G,Ax1),replace_axiom(Ax1,Ax2,Opts)).
replace_matching_axioms_where(Ax1,Ax2,G,Opts) :-
        forall((G,Ax1),replace_axiom(Ax1,Ax2,Opts)).

%% replace_matching_axioms_where(+AxiomTemplateOld,+AxiomTemplateNew,+WhereGoal) is det
% as replace_matching_axioms_where/3, default options
replace_matching_axioms_where(Ax1,Ax2,G) :-
        replace_matching_axioms_where(Ax1,Ax2,G,[]).


%% replace_axiom(+AxOld,+AxNew,+Opts:list) is det
% retract old axiom and replace it with new.
% if Opts contains copy(true) then a copy of the old
% one is retained.
% if Opts contains syntax(plsyn) then plsyn may be used.
replace_axiom(Ax,Ax,[]) :- !. % null replacement - do nothing
replace_axiom(Ax1a,Ax2a,Opts) :-
        select(syntax(plsyn),Opts,Opts2),
        !,
        plsyn_owl(Ax1a,Ax1),
        plsyn_owl(Ax2a,Ax2),
        replace_axiom(Ax1,Ax2,Opts2).
replace_axiom(true,Ax2,_Opts) :-
        !,
        assert_axiom(Ax2).
replace_axiom(Ax1,Ax2,Opts) :-
        !,
        % do this prior to retraction;
        % this predicate should be in the same place as retract axiom..
        findall(ontologyAxiom(O,Ax2),
                ontologyAxiom(O,Ax1),
                NewOntAxioms),
        (   member(copy(true),Opts)
        ->  true
        ;   retract_axiom(Ax1)),
        debug(popl,'Replacing ~w ==> ~w',[Ax1,Ax2]),
        assert_axiom(Ax2),
        % TODO - make optional
        maplist(assert_axiom,NewOntAxioms),
        debug(popl,'  Done',[]).




%% replace_axiom(+AxOld,+AxNew) is det
% as replace_axiom/3, default options
replace_axiom(Ax1,Ax2) :-
        replace_axiom(Ax1,Ax2,[]).

%% replace_expression_in_all_axioms(+TemplateIn,+TemplateOut)
% as replace_expression_in_all_axioms/3, default options
replace_expression_in_all_axioms(T1,T2) :-
        replace_expression_in_all_axioms(T1,T2,[]).

%% replace_expression_in_all_axioms(+TemplateIn,+TemplateOut,+Opts:list)
%
% rewrite the owl2_model database TemplateIn->TemplateOut.
%
% e.g.
% ==
% replace_expression_in_all_axioms(allValuesFrom(partOf,X),someValuesFrom(partOf,X),[])
% ==
%
% see replace_axiom/3 for Opts
replace_expression_in_all_axioms(T1,T2,Opts) :-
        replace_expression_in_all_axioms_where(T1,T2,true,Opts).
%        forall(axiom(Ax),
%               replace_expression_in_axiom(T1,T2,Ax,Opts)).


%% replace_expression_in_all_axioms_where(+TemplateIn,+TemplateOut,+WhereGoal,Opts)
replace_expression_in_all_axioms_where(T1a,T2a,Ga,Opts) :-
        select(syntax(plsyn),Opts,Opts2),
        !,
        plsyn_owl(T1a,T1),
        plsyn_owl(T2a,T2),
        plsyn_owl(Ga,G),
        replace_expression_in_all_axioms_where(T1,T2,G,Opts2).
replace_expression_in_all_axioms_where(T1,T2,G,Opts) :-
        member(ontology(Ont),Opts),
        !,
        normalize_term(T1,T1_Norm),
        findall(Ax-Ax2,
                (   G,
                    ontologyAxiom(Ont,Ax),
                    debug(popl,'testing: ~w ',[Ax]),
                    replace_expression_in_axiom_term(T1_Norm,T2,Ax,Ax2),
                    Ax2\=Ax,
                    debug(popl,'  scheduling: ~w ==> ~w',[Ax,Ax2])),
                Replacements),
        forall(member(Ax-Ax2,Replacements),
               replace_axiom(Ax,Ax2,Opts)).
replace_expression_in_all_axioms_where(T1,T2,G,Opts) :-
        normalize_term(T1,T1_Norm),
        findall(Ax-Ax2,
                (   G,
                    axiom(Ax),
                    replace_expression_in_axiom_term(T1_Norm,T2,Ax,Ax2),
                    Ax2\=Ax,
                    debug(popl,'scheduling: ~w ==> ~w',[Ax,Ax2])),
                Replacements),
        forall(member(Ax-Ax2,Replacements),
               replace_axiom(Ax,Ax2,Opts)).


% DEPRECATED
replace_expression_in_axiom(T1,T2,Ax,Opts) :-
        %debug(popl,'TEST Replacing ~w ==> ~w for ~w',[T1,T2,Ax]),
        replace_expression_in_axiom_term(T1,T2,Ax,Ax2),
        replace_axiom(Ax,Ax2,Opts),
        debug(popl,'DONE Replaced ~w ==> ~w for macro ~w ==> ~w',[Ax,Ax2,T1,T2]).

%% replace_expression_in_axiom_term(+T1,+T2,+Ax1,?Ax2)
% Ax2 is the same as Ax1 with all instances of T1 replaced with T2.
% T1 is replaced no matter how deep it is within the expression
replace_expression_in_axiom_term(T1,T2,Ax1,Ax2) :-
        nonvar(Ax1),
        normalize_term(Ax1,Ax1_Norm),
        Ax1_Norm=T1,
        Ax2=T2, % top-level match
        !.
replace_expression_in_axiom_term(T1,T2,Ax1,Ax2) :-
        axiom(Ax1),
        normalize_term(Ax1,Ax1_Norm),
        debug(popl_detail,'IN ~w ==> ~w :: ~w',[T1,T2,Ax1_Norm]),
        Ax1_Norm =.. [P|Args1],
        maplist(replace_expression(T1,T2),Args1,Args2),
        debug(popl_detail,'  OUT: ~w',[Args2]),
        Ax2 =.. [P|Args2].
        %debug(popl,'OUT ~w ==> ~w :: ~w',[T1,T2,Ax2]).



%% replace_expression(+T1,+T2,+Expr1,?Expr2)
% Expr2 is the same as Expr1 with all instances of T1 replaced with T2.
% T1 is replaced no matter how deep it is placed.
%replace_expression(T1,T2,X1,_) :-
%        debug(popl,'repl(~w ==> ~w) in ~w',[T1,T2,X1]),
%        fail.
replace_expression(_,_,X,X) :- var(X),!. % allow expressions with variables
replace_expression(T1,T2,X1,X2) :-
        debug(popl_detail,'attempting_match(~w == ~w)',[T1-T2,X1-X2]),
        copy_term(T1-T2,X1-X2), % copy and match
        debug(popl,'repl(~w ==> ~w)',[X1,X2]),
        !. % do not recurse below
replace_expression(T1,T2,X1,X2) :-
        is_list(X1),
        !,
        maplist(replace_expression(T1,T2),X1,X2).
replace_expression(T1,T2,X1,X2) :-
        X1 =.. [F|Args1],
        Args1\=[],
        !,
        maplist(replace_expression(T1,T2),Args1,Args2),
        X2 =.. [F|Args2].
replace_expression(_,_,X,X).

normalize_term(X,X) :- var(X),!.
normalize_term(X,X) :- atom(X),!.
normalize_term([],[]) :- !.
normalize_term(L,S) :-
        L=[_|_],
        !,
        sort(L,S).
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
