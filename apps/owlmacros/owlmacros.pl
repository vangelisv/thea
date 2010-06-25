:- module(owlmacros,
	  [expand_all/0]).

:- use_module(library('thea2/owl2_model')).
:- use_module(library('thea2/owl2_io')).
:- use_module(library('thea2/owl2_popl')).
:- use_module(library('thea2/owl2_manchester_parser')).

literal_atom(literal(A),A).


annotation_property_template(P,T) :-
	annotationAssertion('http://www.geneontology.org/formats/oboInOwl#expandAssertionTo',P,V),
	literal_atom(V,T).
annotation_property_template(P,T) :- % new
	annotationAssertion('http://purl.obolibrary.org/obo/IAO_0000425',P,V),
	literal_atom(V,T).

object_property_template(P,T) :-
	annotationAssertion('http://www.geneontology.org/formats/oboInOwl#expandExpressionTo',P,V),
	literal_atom(V,T).
object_property_template(P,T) :- % new
	annotationAssertion('http://purl.obolibrary.org/obo/IAO_0000424',P,V),
	literal_atom(V,T).

parse_template(T,PT,[X,Y]) :-
	debug(owlmacros_details,'parsing: ~w',[T]),        
	owl_parse_manchester_expression(T,PT_1),
	!,
	deep_replace(PT_1,['?X'-X,'?Y'-Y],PT).
parse_template(T,PT,[X,Y]) :-
	owl_parse_manchester_frame(T,Axioms),
	!,
	member(Axiom,Axioms),
	deep_replace(Axiom,['?X'-X,'?Y'-Y],PT).
parse_template(T,_,_) :-
	throw(error(no_parse(T))).

%% collect_mapping(?Mapping) is nondet
% Mapping = ExpressionTemplate-ReplacementExpression
% e.g. someValuesFrom(lacks_part,X)-exactCardinality(has_part,X)
collect_mapping(annotationAssertion(P,X,Y)-PT) :-
	annotation_property_template(P,T),
	parse_template(T,PT,[X,Y]).
collect_mapping(subClassOf(X,someValuesFrom(P,Y))-PT) :-
	annotation_property_template(P,T),
	parse_template(T,PT,[X,Y]).
collect_mapping(someValuesFrom(P,Y)-PT) :-
	object_property_template(P,T),
	parse_template(T,PT,[_,Y]).
collect_mapping(hasValue(P,Y)-PT) :-
	object_property_template(P,T),
	parse_template(T,PT,[_,Y]).

%collect_gci(Axiom,Mappings,GCI) :-
%	copy_term(Mappings,M2),
%	member(Axiom-Repl

%% expand_all/0
%
% expands all macros in the loaded ontology
expand_all :-
	!,
	debug(owlmacros,'expand_all...',[]),
        forall(collect_mapping(T1-T2),
               replace_expression_in_all_axioms(T1,T2)),
        debug(owlmacros,'done',[]).


%% deep_replace(+Expr, +Map:list, ?ExprReplaced) is det
% Map=[In1-Out1,In2-Out2,...]
% replaces all Ins with Outs in Expr

% keep vars
deep_replace(Expr,_,Expr) :- var(Expr),!.

% lists:
deep_replace([],_,[]) :- !.
deep_replace([H|T],Map,[H2|T2]) :-
	!,
	deep_replace(H,Map,H2),
	deep_replace(T,Map,T2).

% non-list expressions:
% //there can be multiple replacements in a single expression;
% //recursively replace all
%deep_replace(Expr,[],Expr) :- !.  % Map exhausted
deep_replace(Expr,Map,Expr3) :-
	% todo - better way - in some cases we
	% want to preserve the vars, e.g. when collecting mappings...
	(   Map=[_-V1|_],
	    var(V1)
	->  Map2=Map
	;   copy_term(Map,Map2)),
	member(Expr-Var,Map2),
	%member(Expr-Var,Map),
	!,
	deep_replace(Var,Map,Expr2),
	deep_replace(Expr2,Map,Expr3).

% replace arguments of complex term:
deep_replace(Expr,Map,Expr2) :-
	Expr=..[F|Args],
	Args=[_|_],
	!,
	deep_replace(Args,Map,Args2),
	Expr2=..[F|Args2].
deep_replace(Expr,_,Expr) :- !. % atom




	
