/* -*- Mode: Prolog -*- */

:- module(swrl,
          [
           implies/2,
           swrlAtom/1,
           swrl_to_owl_axioms/2,
           prolog_term_to_swrl_rule/2,
           prolog_source_to_swrl_rules/2,
           prolog_source_to_axioms/2
          ]
    ).

:- use_module(owl2_model).

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
owl_model:axiom(implies(A,C)):- implies(A,C).

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
% some SWRL idioms. This expands these
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
normalize_swrl_atom(A, A).
                   

%% swrl_to_owl_axioms(+SWRLRule,?Axioms) is nondet
% true if SWRLRule can be translated into Axiom.
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
% * classAssertion/1 based on antecedent-free rules
% * propertyAssertion/1 based on antecedent-free rules
swrl_to_owl_axioms(Rule, Axioms) :-
        normalize_swrl_rule(Rule,implies(A,Cs)),
        findall(Axiom,
                (   member(C,Cs),
                    debug(swrl,'Translating ~w -> ~w',[A,C]),
                    swrl_to_owl(A,C,Axiom)),
                Axioms).


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
swrl_to_owl(AL,C,subPropertyOf(P,propertyChain(PL))) :-
        C=..[P,v(X),v(Y)],
        subgoals_to_property_chain(AL,PL,X,Y),
        PL=[_,_|_],
        !.
swrl_to_owl(AL,C,subClassOf(Sub,intersectionOf(DL))) :-
        C=..[Sub,v(X)],
        subgoals_to_intersection(AL,X,DL),
        DL=[_,_|_],
        !.
swrl_to_owl(AL,C,subClassOf(Sub,D)) :-
        C=..[Sub,v(X)],
        subgoals_to_intersection(AL,X,[D]),
        !.
swrl_to_owl([],description(C,I),classAssertion(C,I)) :-
        I\=v(_),
        !.
swrl_to_owl([],C,propertyAssertion(P,X,Y)) :-
        C=..[P,X,Y],
        X\=v(_),
        Y\=v(_),
        !.

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

subgoals_to_intersection([],_,[]).
subgoals_to_intersection([A|AL],V,[D|DL]) :-
        A=description(D,V),
        subgoals_to_intersection(AL,V,DL).
subgoals_to_intersection([A|AL],V,[D|DL]) :-
        C=..[P,v(X),v(Y)],
        A=someValuesFrom(D,Y),
        subgoals_to_intersection(AL,V,DL).


%% prolog_term_to_swrl_hook( +Term, ?SWRLAtom:swrlAtom )
% define this to extend the translation.
% for example, translation of n-ary relations, lists etc
:- multifile prolog_term_to_swrl_hook/2.

%% prolog_term_to_swrl_rule( +Term, ?SWRLAtom:swrlAtom )
%
% Prolog Terms are clauses of the form
%== 
% hasUncle(X1,X3):- hasParent(X1,X2),hasBrother(X2,X3)
%==
% 
% These are translated to SWRL Rules
% 
% complex atoms still require wrapping:
%==
% description(maxCardinality(1,'style/period'),Z) :-
%   artist(X),
%   description(maxCardinality(1,artistStyle),X),
%   creator(Z,X).
%==

prolog_term_to_swrl_rule(Term,SWRL):-
        numbervars(Term,1,_,[functor_name(v)]),
        prolog_term_to_swrl_rule2(Term,SWRL).

prolog_term_to_swrl_rule2( (C:-A), implies(Ax,Cx) ):-
        !,
        prolog_term_to_swrl_atom(C,Cx),
        prolog_term_to_swrl_atom(A,Ax).
prolog_term_to_swrl_rule2( ('->'(A,C)), implies(Ax,Cx) ):-
        !,
        prolog_term_to_swrl_atom(C,Cx),
        prolog_term_to_swrl_atom(A,Ax).
prolog_term_to_swrl_rule2(C, implies([],Cx) ):- % fact
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
prolog_term_to_swrl_atom( Goal, builtin(B,[Return|ArgsX]) ):-
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
prolog_term_to_swrl_atom( A, description(F,BX) ):-
        A=..[F,B],
        !,
        prolog_term_to_swrl_atom( B, BX).
prolog_term_to_swrl_atom( A, A ):-
        A=..[F,B,C],
        !,
        prolog_term_to_swrl_atom( B, BX),
        prolog_term_to_swrl_atom( C, CX),
        AX=..[F,BX,CX].
prolog_term_to_swrl_atom( A, A) :-
        atom(A),
        !.
prolog_term_to_swrl_atom( A, A) :-
        number(A),
        !.
        
%% prolog_source_to_swrl_rules(+File,?Rules)
%
% TODO: use prolog source in pldoc style to generate annotation axioms
prolog_source_to_swrl_rules(File,Rules) :-
        read_file_to_terms(File,Terms,[]),
        prolog_terms_to_swrl_rules(Terms,Rules).

prolog_terms_to_swrl_rules([],[]).
prolog_terms_to_swrl_rules([T|Terms],[R|Rules]) :-
        prolog_term_to_swrl_rule(T,R),
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


%% goal_swrl(+Goal,?Swrl)
% TODO: incomplete
goal_swrlb(concat_atom(L,A),B):-
        G=..[stringConcat,A|L].
goal_swrlb(X is A+B,add(A,B)).
goal_swrlb(X is A-B,subtract(A,B)).
goal_swrlb(X is A*B,multiply(A,B)).
goal_swrlb(X is A/B,divide(A,B)).

pred_swrlb(=,stringEqualIgnoreCase). % TODO: detect
pred_swrlb(atom_length,stringLength).
pred_swrlb(upcase_atom,upperCase).
pred_swrlb(downcase_atom,lowerCase).


/** <module> Semantic Web Rules Language

  ---+ Synopsis

  Example SWRL Rule set in prolog:
==
implies([hasParent(v(x),v(y)),hasBrother(v(y),v(z))],[hasUncle(v(x),v(z))]).
==

---+ Details

    This extends the owl2_model.pl collection of allowed axioms (see axiom/1) with the implies/2 axiom.
    
http://www.w3.org/Submission/SWRL/

This module also intends to allow for easy conversion between a natural prolog style and SWRL axioms.
 See for example prolog_term_to_swrl_rule/2

---+ Additional Information

@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/
