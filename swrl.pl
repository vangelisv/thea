/* -*- Mode: Prolog -*- */

:- module(swrl,
          [
           implies/2,
           swrlAtom/1,
           swrl_to_owl_axioms/2,
           prolog_clause_to_swrl_rule/2,
           prolog_source_to_swrl_rules/2,
           prolog_source_to_axioms/2
          ]
    ).

:- use_module(owl2_model).
:- use_module(library('semweb/rdf_db.pl'),[rdf_register_ns/3]).

:- multifile owl2_model:axiompred/1, owl2_model:axiom/1.

%% implies(?Antecedent:list(swrlAtom), ?Consequent:list(swrlAtom)) is nondet
% Informally, a rule may be read as meaning that if the antecedent
% holds (is "true"), then the consequent must also hold. An empty
% antecedent is treated as trivially holding (true), and an empty
% consequent is treated as trivially not holding (false). Rules with
% an empty antecedent can thus be used to provide unconditional facts;
% however such unconditional facts are better stated in OWL itself,
% i.e., without the use of the rule construct. Non-empty antecedents
% and consequents hold iff all of their constituent atoms hold, i.e.,
% they are treated as conjunctions of their atoms.
owl2_model:axiompred(implies/2).
:- dynamic implies/2.
:- multifile implies/2.
owl2_model:axiom(implies(A,C)):- implies(A,C).

:- multifile owl2_model:assert_axiom_hook/1.
owl2_model:assert_axiom_hook(implies(A,C)) :-
        assert(swrl:implies(A,C)).

%% swrlAtom(?SWRLAtom)
% true if SWRLAtom is a term consistent with SWRL atom syntax
%==
% atom ::= description '(' i-object ')'
%	 | dataRange '(' d-object ')'
%	 | individualvaluedPropertyID '(' i-object i-object ')'
%	 | datavaluedPropertyID '(' i-object d-object ')'
%	 | sameAs '(' i-object i-object ')'
%	 | differentFrom '(' i-object i-object ')'
%	 | builtIn '(' builtinID { d-object } ')'
%==
%
% for named descriptions we allow prolog terms, e.g. animal('?x'), eats('?x','?y').
% for class expressions this would lead to illegal prolog terms, so we
% instead prolog-reify this as description(CE,I_Obj); e.g.
% =|Artist(?x) & (<=1 artistStyle)(?x) & creator(?z,?x) -> (<= 1 style/period)(?z)|=
% is translated as
%
%==
% implies([
%   artist(i(x)),
%   description(maxCardinality(1,artistStyle),i(x)),
%   creator(i(z),i(x))],
%   description(maxCardinality(1,'style/period'),i(z))]
%==
% we also allow named descriptions to be used here. e.g. description(artist,'?x')
swrlAtom(A):-
        (   A=description(X,CE)
        ->  classExpression(CE),i_object(X)
        ;   A=dataRange(DR,X)
        ->  dataRange(DR),d_object(X)
        ;   A=sameAs(X,Y)
        ->  i_object(X),i_object(Y)
        ;   A=differentFrom(X,Y)
        ->  i_object(X),i_object(Y)
        ;   A=builtin(X,L)
        ->  builtin(X),list_of_d_object(L)
        ;   A=..[F,X]
        ->  class(F),
            i_object(X)
        ;   A=..[F,X,Y]
        ->  i_object(X),
            (   objectProperty(F)
            ->  i_object(X)
            ;   dataProperty(F)
            ->  d_object(Y))).

i_object(X) :- i_variable(X).
i_object(X) :- individual(X).

d_object(X) :- d_variable(X).
d_object(X) :- literal(X).

i_variable(i(_)).
d_variable(d(_)).

list_of_d_object(L) :- forall(member(X,L),d_object(X)).

%% normalize_swrl_rule(+Rule,?RuleNormalized)
% true if RuleNormalized is the canonical form of Rule
%
% this module admits syntactic sugar shortcuts for
% some SWRL idioms. This expands these.
% For example foo(?x) ==> description(foo,?x)
normalize_swrl_rule(implies(A,C),implies(AX,CX)) :-
        (   is_list(A)
        ->  maplist(normalize_swrl_atom,A,AX)
        ;   maplist(normalize_swrl_atom,[A],AX)),
        (   is_list(C)
        ->  maplist(normalize_swrl_atom,C,CX)
        ;   maplist(normalize_swrl_atom,[C],CX)).

normalize_swrl_atom(A, description(Class,Ob) ) :-
        A=..[Class,Ob],
        !.
%normalize_swrl_atom(A, propertyAssertion(P,X,Y) ) :-
%        A=..[P,X,Y],
%        !.
normalize_swrl_atom(A, A).
                   

%% swrl_to_owl_axioms(+SWRLRule,?OWLAxioms) is semidet
% true if SWRLRule can be translated into OWLAxiom.
%
% a subset of OWL-DL can be recapitulated as rules; however, it
% if often best to treat these rules as OWL Axioms if an OWL
% reasoner is to be used
%
% can infer:
% * subClassOf/2 between 2 classes
% * subClassOf/2 between a class and an intersectionOf class description
% * subPropertyOf/2 between property IDs
% * subPropertyOf/2 involving role chains
% * classAssertion/2 based on antecedent-free rules
% * propertyAssertion/3 based on antecedent-free rules
swrl_to_owl_axioms(Rule, Axioms) :-
        normalize_swrl_rule(Rule,implies(A,Cs)),
        findall(Axiom,
                (   member(C,Cs),
                    debug(swrl,'Translating ~w -> ~w',[A,C]),
                    swrl_to_owl(A,C,Axiom)),
                Axioms),
        Axioms\=[].



%% swrl_to_owl(+AntecedentList:list,+Consequent,?Axiom) is nondet
swrl_to_owl([description(Sub,v(X))],description(Super,v(X)), subClassOf(Sub,Super)) :- !.
swrl_to_owl([A],C,subPropertyOf(SubP,SuperP)) :-
        A=..[SubP,v(X),v(Y)],
        C=..[SuperP,v(X),v(Y)],
        !.
swrl_to_owl([A],C,symmetricProperty(P)) :-
        A=..[P,v(X),v(Y)],
        C=..[P,v(Y),v(X)],
        !.
swrl_to_owl([A],C,inverseProperties(P,Q)) :-
        A=..[P,v(X),v(Y)],
        C=..[Q,v(Y),v(X)],
        !.
swrl_to_owl(AL,C,transitiveProperty(P)) :-
        C=..[P,v(X),v(Y)],
        subgoals_to_property_chain(AL,PL,X,Y),
        PL=[P,P],
        !.
swrl_to_owl(AL,C,subPropertyOf(propertyChain(PL),P)) :-
        C=..[P,v(X),v(Y)],
        subgoals_to_property_chain(AL,PL,X,Y),
        PL=[_,_|_],
        !.
swrl_to_owl(AL, description(Sub,v(X)), subClassOf(Sub,intersectionOf(DL))) :-
        subgoals_to_intersection(AL,v(X),DL),
        !.
% TODO: this is cheating to allow for round-tripping.
% for now, when translating pl->swrl, disjunctions in body are left
% as disjunctions. We should either (a) translate these to unionOf
% descriptions during the parse or (b) eliminate the disjunctions
% by rewriting the rule as a disjunctive collection of conjunctions
swrl_to_owl([AL], description(Sub,v(X)), subClassOf(Sub,unionOf(DL))) :-
        AL=(_;_),
        !,
        subgoals_to_union(AL,v(X),DL),
        !.
% e.g. upstreamOfGene(X) :- upstream_of(X,G),gene(G).
swrl_to_owl(AL, description(Sub,v(X)), subClassOf(someValuesFrom(P,D),Sub)) :-
        select(A1,AL,[A2]),
        A1=..[P,v(X),v(Y)],
        A2=description(D,v(Y)),
        !.
swrl_to_owl(AL,C,subClassOf(Sub,intersectionOf(DL))) :-
        C=..[Sub,v(X)],
        subgoals_to_intersection(AL,X,DL),
        DL=[_,_|_],
        !.
swrl_to_owl(AL,C,subClassOf(Sub,D)) :- % non-normalized form of above rule
        C=..[Sub,v(X)],
        subgoals_to_intersection(AL,X,[D]),
        !.
swrl_to_owl(AL,C,subClassOf(Sub,D)) :- 
        C=..[Sub,v(X)],
        subgoals_to_description(AL,X,D),
        !.
swrl_to_owl([],description(C,I),classAssertion(C,I)) :-
        I\=v(_),
        !.
swrl_to_owl([],C,propertyAssertion(P,X,Y)) :-  % do we need data/object split?
        C=..[P,X,Y],
        X\=v(_),
        Y\=v(_),
        !.
swrl_to_owl([A],description(D,X),propertyDomain(P,D)) :-
        A=..[P,X,_],
        !.
swrl_to_owl([A],description(R,X),propertyRange(P,R)) :-
        A=..[P,_,X],
        !.
% see email to owl-dev 2009-06-15 "class specific inverse"
% I believe this is called 'marker properties'
swrl_to_owl(AL,C,Axiom) :-
        C=..[P,X,Y],            % e.g. hasPet(x,y)
        select(A1,AL,[A2]),     
        A1=..[P2,X,Y],          % e.g. owns(x,y)
        A2=description(D,Y),  % e.g. animal(y)
        atom(D),
        atom_concat(D,'_p',DP), % e.g. isAnimal
        !,
        member(Axiom,
               [subClassOf(D,hasSelf(DP)),
                subPropertyOf(propertyChain([P2,DP]),P)]).


%% subgoals_to_property_chain(+Terms,?Properties,+StartVar,?EndVar)
% true if Terms is a chain of goals P1(V0,V1),P2(V1,V2),...,Pn(Vn-1,Vn)
% and Properties = [P1,P2,...]
% and V0=StartVar and Vn=Endvar.
% The terms can be in any order
subgoals_to_property_chain([],[],X,X) :- !.
subgoals_to_property_chain(AL,[P|PL],X,Y) :-
        select(A,AL,AL2),
        A=..[P,v(X),v(Z)],
        subgoals_to_property_chain(AL2,PL,Z,Y).
subgoals_to_property_chain(AL,[inverseOf(P)|PL],X,Y) :-
        select(A,AL,AL2),
        A=..[P,v(Z),v(X)],
        subgoals_to_property_chain(AL2,PL,Z,Y).

subgoals_to_intersection([],_,[]).
subgoals_to_intersection([A|AL],V,[D|DL]) :-
        A=description(D,V),
        subgoals_to_intersection(AL,V,DL).
% TODO:
%subgoals_to_intersection([A|AL],V,[D|DL]) :-
%        C=..[P,v(X),v(Y)],
%        A=someValuesFrom(D,Y),
%        subgoals_to_intersection(AL,V,DL).

% not valid swrl, but we allow this for roundtripping
subgoals_to_union((A;AL),V,[D|DL]) :-
        !,
        A=description(D,V),
        subgoals_to_union(AL,V,DL).
subgoals_to_union(description(D,_),_,[D]).

% TODO
subgoals_to_description([],_,[]).
subgoals_to_description([A|AL],V,[D|DL]) :-
        subgoal_to_description(A,V,D),
        subgoals_to_description(AL,V,DL).

subgoal_to_description(description(D,V),V,D).

%% prolog_term_to_swrl_hook( +Term, ?SWRLAtom:swrlAtom )
% define this to extend the translation.
% for example, translation of n-ary relations, lists etc
:- multifile prolog_term_to_swrl_hook/2.

%% prolog_clause_to_swrl_rule( +Term, ?SWRLAtom:swrlAtom )
%
% Prolog clause terms are clauses of the form
%== 
% hasUncle(X1,X3):- hasParent(X1,X2),hasBrother(X2,X3)
%==
% 
% Are translated to embedded swrl.pl rule terms, using
% the implies/2 functor.
% 
% complex atoms still require wrapping:
%==
% description(maxCardinality(1,'style/period'),Z) :-
%   artist(X),
%   description(maxCardinality(1,artistStyle),X),
%   creator(Z,X).
%==

prolog_clause_to_swrl_rule(Term,SWRL):-
        numbervars(Term,1,_,[functor_name(v)]),
        prolog_clause_to_swrl_rule2(Term,SWRL),
        debug(swrl,'translated: ~w ==> ~w',[Term,SWRL]),
        !.
prolog_clause_to_swrl_rule(Term,_):-
        throw(error(prolog_clause_to_swrl_rule(Term))).

% implications
prolog_clause_to_swrl_rule2( (C:-A), implies(Ax,Cx) ):-
        !,
        prolog_term_to_swrl_atom(C,Cx),
        prolog_term_to_swrl_atom(A,Ax).
% implications - alternate syntax
prolog_clause_to_swrl_rule2( ('->'(A,C)), implies(Ax,Cx) ):-
        !,
        prolog_term_to_swrl_atom(C,Cx),
        prolog_term_to_swrl_atom(A,Ax).
% facts
prolog_clause_to_swrl_rule2(C, implies([],Cx) ):- % fact
        !,
        prolog_term_to_swrl_atom(C,Cx).

prolog_term_to_swrl_atom( A, AX ):-
        prolog_term_to_swrl_hook(A,AX). % extendable
prolog_term_to_swrl_atom( (A,B), [Ax|Bx] ):-
        !,
        prolog_term_to_swrl_atom(A,Ax),
        prolog_term_to_swrl_atom(B,Bx1),
        (   is_list(Bx1)
        ->  Bx=Bx1
        ;   Bx=[Bx1]).
prolog_term_to_swrl_atom( v(A), v( A)):- !.
prolog_term_to_swrl_atom( A=B, sameAs(AX,BX) ):-
        !,
        prolog_term_to_swrl_atom( A, AX),
        prolog_term_to_swrl_atom( B, BX).
prolog_term_to_swrl_atom( AneqB, differentFrom(AX,BX) ):-
        (   AneqB = (A\=B)
        ;   AneqB = (\+ A=B)),
        !,
        prolog_term_to_swrl_atom( A, AX),
        prolog_term_to_swrl_atom( B, BX).
prolog_term_to_swrl_atom( Goal, builtin(B,[Return|ArgsX2]) ):-
        Goal=..[P|Args],
        pred_swrlb(P,B),
        !,
        maplist(prolog_term_to_swrl_atom,Args,ArgsX),
        % SWRL builtins typically have the return value last
        reverse(ArgsX,[Return|ArgsXR]),
        reverse(ArgsXR,ArgsX2).
prolog_term_to_swrl_atom( Goal, builtin(B,ArgsX) ):-
        goal_swrlb(Goal,Builtin),
        Builtin=..[B|Args],
        !,
        maplist(prolog_term_to_swrl_atom,Args,ArgsX).
prolog_term_to_swrl_atom( A, description(FX,BX) ):-
        A=..[F,B],
        !,
        default_ns(F,FX),
        prolog_term_to_swrl_atom( B, BX).
prolog_term_to_swrl_atom( A, AX ):-
        A=..[F,B,C],
        !,
        prolog_term_to_swrl_atom( B, BX),
        prolog_term_to_swrl_atom( C, CX),
        default_ns(F,FX),
        AX=..[FX,BX,CX].        % TODO: canonicalize to expanded form?
prolog_term_to_swrl_atom( A, literal(type('xsd:integer',A)) ) :-
        number(A),
        !.
prolog_term_to_swrl_atom( A, A) :-
        atom(A),
        !.
        
%% prolog_source_to_swrl_rules(+File,?Rules)
%
% TODO: use prolog source in pldoc style to generate annotation axioms
prolog_source_to_swrl_rules(File,Rules) :-
        read_file_to_terms(File,Terms,[]),
        prolog_terms_to_swrl_rules(Terms,Rules).

prolog_terms_to_swrl_rules([],[]).
prolog_terms_to_swrl_rules([T|Terms],[R|Rules]) :-
        prolog_clause_to_swrl_rule(T,R),
        !,
        prolog_terms_to_swrl_rules(Terms,Rules).

prolog_terms_to_swrl_rules([T|Terms],Rules) :-
        format(user_error,'Cannot translate: ~q~n',[T]),
        prolog_terms_to_swrl_rules(Terms,Rules).

prolog_source_to_axioms(File,Axioms) :-
        prolog_source_to_swrl_rules(File,Rules),
        findall(Axiom,
                (   member(Rule,Rules),
                    (   swrl_to_owl_axioms(Rule,Axioms),
                        Axioms\=[]
                    ->  member(Axiom,Axioms)
                    ;   Axiom=Rule)),
                Axioms).


%% goal_swrlb(+Goal,?Swrl)
% builtins
% TODO: currently incomplete - add more
goal_swrlb(concat_atom(L,A),G):-
        G=..[stringConcat,A|L].
goal_swrlb(X is A+B,add(A,B,X)).
goal_swrlb(X is A-B,subtract(A,B,X)).
goal_swrlb(X is A*B,multiply(A,B,X)).
goal_swrlb(X is A/B,divide(A,B,X)).

% arithmetic TODO
pred_swrlb(<,lessThan). 
pred_swrlb(=,equal). % TODO: detect type
pred_swrlb(\=,notEqual). % TODO: detect type
pred_swrlb(=<,lessThanOrEqual). 
pred_swrlb(>,greaterThan). 
pred_swrlb(>=,greaterThanOrEqual). 

pred_swrlb(=,stringEqualIgnoreCase). % TODO: detect type
pred_swrlb(atom_length,stringLength).
pred_swrlb(upcase_atom,upperCase).
pred_swrlb(downcase_atom,lowerCase).

default_ns(F,FX) :-
        rdf_register_ns('_d','http://x.org#',[force(true)]),
        atom_concat('_d:',F,FX).



% IO HOOKS
:- multifile owl2_io:load_axioms_hook/3.
owl2_io:load_axioms_hook(File,pl_swrl,Opts) :-
        read_file_to_terms(File,Terms,Opts),
        forall(member(Term,Terms),
               (   prolog_clause_to_swrl_rule(Term,SWRL_Rule),
                   assert_axiom(SWRL_Rule))).

% Format: pl_swrl_owl
%  first translates a prolog clause such as
%  ==
%  a(X) :- b(X).
%  ==
% to a swrl rule, embedded using implies/2.
% then translate this to an owl subClassOf/2 axiom.
owl2_io:load_axioms_hook(File,pl_swrl_owl,Opts) :-
        read_file_to_terms(File,Terms,Opts),
        forall(member(Term,Terms),
               (   prolog_clause_to_swrl_rule(Term,SWRL_Rule),
                   (   swrl_to_owl_axioms(SWRL_Rule,Axioms)
                   ->  maplist(assert_axiom,Axioms)
                   ;   assert_axiom(SWRL_Rule)))).



/** <module> Semantic Web Rules Language

  ---+ Synopsis

  Example SWRL Rule embedded as prolog swrl.pl fact:
  
==
implies([hasParent(v(x),v(y)),hasBrother(v(y),v(z))],[hasUncle(v(x),v(z))]).
==


---+ Details

This extends the owl2_model.pl collection of allowed axioms (see axiom/1) with the implies/2 axiom.
    
http://www.w3.org/Submission/SWRL/

This module also intends to allow for easy conversion between a natural prolog style and SWRL axioms.
See for example prolog_clause_to_swrl_rule/2


*/
