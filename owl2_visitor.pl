/* -*- Mode: Prolog -*- */

:- module(owl2_visitor,
          [
           visit_all/2,
           visit_axioms/3,
           axiom_rewrite_list/3,
           rewrite_axiom/3,
           smatch/2 % only exported for testing purposes..
           ]).

:- use_module(owl2_model).

%% visit_all_axioms(+Visitor,Ts)
visit_all(Visitor,Ts) :-
        findall(Axiom,axiom(Axiom),Axioms),
        visit_axioms(Axioms,Visitor,Ts).

visit_ontology(Ontology,Visitor) :-
        visit_ontology(Ontology,Visitor,_).
visit_ontology(Ontology,Visitor,Ts) :-
        findall(Axiom,ontologyAxiom(Ontology,Axiom),Axioms),
        visit_axioms(Axioms,Visitor,Ts).

%% visit_axioms(+Axioms:list,+Visitor,?Terms)
visit_axioms([],_,[]).
visit_axioms([Axiom|Axioms],Visitor,Ts_All) :-
        visit_axiom(Axiom,Visitor,Ts),
        visit_axioms(Axioms,Visitor,Ts2),
        append(Ts,Ts2,Ts_All).

%% visit_axiom(+Axiom,+Visitor,?Results:list)
%
% Visitor = visitor(Goal,AxiomTemplate,ExpressionTemplate,Result)
%
% Goal is applied for all axioms matching AxiomTemplate, the results are collected,
%  then all sub-expressions are visited, with the results appended
% A simple example collects all subclasses:
% ==
% visitor(true,subClassOf(X,Y),_,X)
% ==
% leaf classes:
% ==
% visitor(\+subClassOf(_,X),subClassOf(X,Y),_,X)
% ==
visit_axiom(Axiom,Visitor,Ts_All) :-
        visitor_axiom_template(Visitor,VisitGoal,AxiomTemplate,T),
        findall(T,(Axiom=AxiomTemplate,VisitGoal),Ts),
        Axiom =.. [_|Args],
        visit_args(Axiom,Args,Visitor,Ts2),
        append(Ts,Ts2,Ts_All).

visit_args(_,[],_,[]).
visit_args(Axiom,[Arg|Args],Visitor,Ts_All) :-
        visit_expression(Axiom,Arg,Visitor,Ts),
        visit_args(Axiom,Args,Visitor,Ts2),
        append(Ts,Ts2,Ts_All).

%% visit_expression(+SourceAxiom,+Expression,+Visitor,?Results:list)
%
% Visitor = visitor(Goal,AxiomTemplate,ExpressionTemplate,Result)
%
% Goal is called if the input expression matches the expression template
visit_expression(Axiom,Ex,Visitor,Ts_All) :-
        visitor_expression_template(Visitor,VisitGoal,AxiomTemplate,ExTemplate,T),
        !,
        findall(T,(Axiom=AxiomTemplate,Ex=ExTemplate,VisitGoal),Ts),
        !,
        Ex =.. [_|Args],
        visit_args(Axiom,Args,Visitor,Ts2),
        append(Ts,Ts2,Ts_All).
visit_expression(_,_,_,[]).

visitor_axiom_template(visitor(G,AT,_,R),G,AT,R) :- !.
visitor_axiom_template(axiom_visitor(G,AT,R),G,AT,R) :- !.
visitor_axiom_template(_,fail,_,_) :- !.
visitor_expression_template(visitor(G,AT,ET,R),G,AT,ET,R) :- !.
visitor_expression_template(expression_visitor(G,ET,R),G,_,ET,R) :- !.
visitor_expression_template(expression_visitor(G,AT,ET,R),G,AT,ET,R) :- !.

% ----------------------------------------
% axiom rewriting
% ----------------------------------------

axiom_rewrite_list(Axiom,Rule,NewAxioms) :-
        setof(NewAxiom,rewrite_axiom(Axiom,Rule,NewAxiom),NewAxioms).

%% rewrite_axiom(+Axiom,+Rule,?NewAxiom) is nondet
%
% fails if Axiom does not match rule.
% succeeds once if Rule is deterministic.
% succeeds one or more times if Rule is non-deterministic.
% Rules are non-deterministic if 
rewrite_axiom(Axiom,Rules,NewAxiom) :-
        is_list(Rules),
        !,
        rewrite_axiom_multirule(Axiom,Rules,NewAxiom).
rewrite_axiom(Axiom,Rule,NewAxiom) :-
        debug(visitor,'testing ~w using ~w',[Axiom,Rule]),
        rule_axiom_template(Rule,ConditionalGoal,AxiomTemplate,TrAxiom),
        findall(TrAxiom,(smatch(Axiom,AxiomTemplate),once(ConditionalGoal)),[A1]),
        !,
        member_or_identity(A1x,A1),
        debug(visitor,'  rewriting ~q ===> ~q',[Axiom,A1x]),
        (   Axiom=zequivalentClasses(['http://purl.obolibrary.org/obo/MP_0000057',intersectionOf([someValuesFrom('http://purl.obolibrary.org/obo/BFO_0000052',unionOf(['http://purl.obolibrary.org/obo/GO_0001503',someValuesFrom('http://purl.obolibrary.org/obo/BFO_0000050','http://purl.obolibrary.org/obo/GO_0001503')])),'http://purl.obolibrary.org/obo/PATO_0000001',someValuesFrom('http://purl.obolibrary.org/obo/test_qualifier','http://purl.obolibrary.org/obo/PATO_0000460')])])
        ->  trace
        ;   true),
        A1x =.. [Pred|Args],
        rewrite_args(Axiom,Args,Rule,Args2),
        debug(visitor,'  rewrote ~w ===> ~w',[Args,Args2]),
        NewAxiom =.. [Pred|Args2].


rewrite_axiom_multirule(Axiom,[],Axiom) :- !.
rewrite_axiom_multirule(Axiom,[Rule|Rules],NewAxiom) :-
        rewrite_axiom(Axiom,Rule,Axiom_2), % todo - nd
        (   Axiom\=Axiom_2
        ->  
            debug(v2,'  rewriting ~w ***===>*** ~w',[Axiom,Axiom_2]),
            debug(v2,'     TO GO: ~w',[Rules])
        ;
            true),
        rewrite_axiom_multirule(Axiom_2,Rules,NewAxiom).

rewrite_args(_,[],_,[]) :- !.
rewrite_args(Axiom,[Arg|Args],Rule,[NewArg|NewArgs]) :-
        debug(v2,'  XX rewriting ~w',[Arg]),
        rewrite_expression(Axiom,Arg,Rule,NewArg),
        %debug(v2,'  ===> XX  ~w',[NewArg]),
        rewrite_args(Axiom,Args,Rule,NewArgs).


%% rewrite_expression(+SourceAxiom,+Expression,+Rule,?NewExpression) is nondet
%
% Rule = visitor(Goal,AxiomTemplate,ExpressionTemplate,Result)
%
% Goal is called if the input expression matches the expression template
rewrite_expression(Axiom,Ex,Rule,NewEx) :-
        nonvar(Ex),
        is_list(Ex),
        !,
        rewrite_args(Axiom,Ex,Rule,NewEx).
rewrite_expression(Axiom,Ex,Rule,NewEx) :-
        rule_expression_template(Rule,ConditionalGoal,ExTemplate,TrEx),
        findall(TrEx,(smatch(Ex,ExTemplate),once(ConditionalGoal)),[Ex1]),
        !,
        debug(v2,'  ** tr ~q',[Ex1]),
        member_or_identity(Ex1_Single,Ex1),
        Ex1_Single =.. [Pred|Args],
        rewrite_args(Axiom,Args,Rule,NewArgs),
        NewEx =.. [Pred|NewArgs].
rewrite_expression(Axiom,Ex,Rule,NewEx) :-
        !,
        Ex =.. [Pred|Args],
        rewrite_args(Axiom,Args,Rule,NewArgs),
        NewEx =.. [Pred|NewArgs].


rule_axiom_template(tr(axiom,In,Out,G,_),G,In,Out).
rule_axiom_template(tr(_,_,_,_,_),true,Ax,Ax). % pass-through
rule_axiom_template(Rule,_,_,_) :- Rule\=tr(_,_,_,_,_),throw(error(invalid(Rule))).
rule_expression_template(tr(expression,In,Out,G,_),G,In,Out).
%rule_expression_template(tr(_,_,_,_,_),true,Ex,Ex). % pass-through

member_or_identity(X,L) :-
        (   L=(A,B)
        *-> (   member_or_identity(X,A)
            ;   member_or_identity(X,B))
        ;   X=L).

% structural match
smatch(Term,Term) :- !.
smatch(QTerm,MTerm) :-
        QTerm =.. [Pred|QArgs],
        MTerm =.. [Pred|MArgs],
        smatch_args(QArgs,MArgs),
        !.

smatch_args(X,X) :- !.
smatch_args(propertyChain(L1),propertyChain(L2)) :-
        !,
        % property chains are the only expressions where order is important
        L1=L2.
smatch_args([Set1],[Set2]) :- % order of args is unimportant e.g. intersectionOf(Set)
        is_list(Set1),
        !,
        smatch_args_unordered(Set1,Set2),
        !.

smatch_args([],[]).
smatch_args([A1|Args1],[A2|Args2]) :-
        smatch(A1,A2),
        smatch_args(Args1,Args2).

smatch_args_unordered([],[]) :- !.
smatch_args_unordered(Tail,[X]) :- nonvar(X),X=tail(Tail),!.
smatch_args_unordered(_,[]) :- !,fail.
smatch_args_unordered([],_) :- !,fail.
smatch_args_unordered(L1,[X,A2|L2_Tail]) :-
        nonvar(X),
        X=tail(_),
        !,
        select(A1,L1,L1_Tail),
        smatch(A1,A2),
        smatch_args_unordered(L1_Tail,[X|L2_Tail]).
smatch_args_unordered(L1,[A2|L2_Tail]) :-
        select(A1,L1,L1_Tail),
        smatch(A1,A2),
        smatch_args_unordered(L1_Tail,L2_Tail).

/*
smatch_args_unordered([A1|L1_Tail],L2) :-
        select(A2,L2,L2_Tail),
        smatch(A1,A2),
        smatch_args_unordered(L1_Tail,L2_Tail).
*/
        



list_subsumed_by([],_) :- !.
list_subsumed_by(X,_) :- var(X),!.
list_subsumed_by(_,[]) :- !, fail.
list_subsumed_by([X|L],L2) :-
        select(X,L2,L2_Rest),
        !,
        list_subsumed_by(L,L2_Rest).

% rsort(L,S) :- writeln(sorting(L)),sort(L,S1),reverse(S1,S).

% ----------------------------------------
% test
% ----------------------------------------

t :-
        ontology(Ont),
        visit_ontology( Ont, visitor(check_av(Axiom,Expr,T), Axiom,Expr,T), L ),
        maplist(writeln,L).
                      
check_av(Axiom,Expr,bad(Expr,Axiom)) :-
        nonvar(Expr),
        Expr=unionOf(_),
        !.

        

        
