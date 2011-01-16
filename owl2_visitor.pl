/* -*- Mode: Prolog -*- */

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

%cv(visitor(VisitGoal,Axiom,Expr,_,L), visitor(VisitGoal,Axiom,Expr,L,_)).

visitor_axiom_template(visitor(G,AT,_,R),G,AT,R) :- !.
visitor_axiom_template(axiom_visitor(G,AT,R),G,AT,R) :- !.
visitor_axiom_template(_,fail,_,_) :- !.
visitor_expression_template(visitor(G,AT,ET,R),G,AT,ET,R) :- !.
visitor_expression_template(expression_visitor(G,ET,R),G,_,ET,R) :- !.
visitor_expression_template(expression_visitor(G,AT,ET,R),G,AT,ET,R) :- !.

% ---

%% rewrite_axiom(+Axiom,+Rule,?NewAxiom) is nondet
%
% fails if Axiom does not match rule.
% succeeds once if Rule is deterministic.
% succeeds one or more times if Rule is non-deterministic.
% Rules are non-deterministic if 
rewrite_axiom(Axiom,Rule,NewAxiom) :-
        rule_axiom_template(Rule,ConditionalGoal,AxiomTemplate,TrAxiom),
        findall(TrAxiom,(Axiom=AxiomTemplate,ConditionalGoal),[A1]),
        !,
        member_or_identity(A1x,A1),
        A1x =.. [Pred|Args],
        rewrite_args(Axiom,Args,Rule,Args2),
        NewAxiom =.. [Pred|Args2].


rewrite_args(_,[],_,[]) :- !.
rewrite_args(Axiom,[Arg|Args],Rule,[NewArg|NewArgs]) :-
        rewrite_expression(Axiom,Arg,Rule,NewArg),
        rewrite_args(Axiom,Args,Rule,NewArgs).



%% rewrite_expression(+SourceAxiom,+Expression,+Rule,?NewExpression) is nondet
%
% Rule = visitor(Goal,AxiomTemplate,ExpressionTemplate,Result)
%
% Goal is called if the input expression matches the expression template
rewrite_expression(Axiom,Ex,Rule,NewEx) :-
        rule_expression_template(Rule,ConditionalGoal,ExTemplate,TrEx),
        findall(TrEx,(Ex=ExTemplate,ConditionalGoal),[Ex1]),
        !,
        member_or_identity(Ex1_Single,Ex1),
        Ex1_Single =.. [Pred|Args],
        rewrite_args(Axiom,Args,Rule,NewArgs),
        NewEx =.. [Pred|NewArgs].
rewrite_expression(_,Ex,_,Ex) :- !.

/*
rule_axiom_template(axiom_tr(In,Out),true,In,Out).
rule_axiom_template(tr(In,Out),true,In,Out). % treat as axiom
rule_axiom_template(tr(_,_),true,Ax,Ax). % pass-through
rule_axiom_template(expression_tr(_,_),true,Ax,Ax). % pass-through

rule_expression_template(tr(In,Out),true,In,Out).
rule_expression_template(expression_tr(In,Out),true,In,Out).
*/

rule_axiom_template(tr(axiom,In,Out,G,_),G,In,Out).
rule_axiom_template(tr(_,_,_,G,_),G,Ax,Ax). % pass-through

rule_expression_template(tr(expression,In,Out,G,_),G,In,Out).

member_or_identity(X,L) :-
        (   L=(A,B)
        *-> (   member_or_identity(X,A)
            ;   member_or_identity(X,B))
        ;   X=L).

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

        

        
