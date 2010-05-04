:- module(owlmacros,
	  [expand_all/0]).

:- use_module(library('thea2/owl2_model')).
:- use_module(library('thea2/owl2_io')).
:- use_module(library('thea2/owl2_manchester_parser')).

literal_atom(literal(A),A).


annotation_property_template(P,T) :-
	annotationAssertion('http://www.geneontology.org/formats/oboInOwl#expandAssertionTo',P,V),
	literal_atom(V,T).

object_property_template(P,T) :-
	annotationAssertion('http://www.geneontology.org/formats/oboInOwl#expandExpressionTo',P,V),
	literal_atom(V,T).

parse_template(T,PT,[X,Y]) :-
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
% Mapping = AxiomTemplate-ReplacementAxiom
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

collect_mappings(Maps) :-
	debug(owlmacros,'collecting mappings...',[]),
	findall(Mapping,collect_mapping(Mapping),Maps).

%% expand_all/0
%
% expands all macros in the loaded ontology
expand_all :-
	!,
	debug(owlmacros,'expand_all...',[]),
	collect_mappings(Maps),
	debug(owlmacros,'collected mappings: ~w',[Maps]),
	debug(owlmacros,'expanding all axioms...',[]),
	forall(axiom(A),
	       expand_axiom(Maps,A)).

%% expand_axiom(+Maps:list,+Axiom)
% if Axiom matches any of the patterns in Maps, then it is replaced with the template.
% Maps is list of Pattern-Replacement key-value pairs, where both Pattern and Replacement
% are prolog terms corresponding to OWL axioms (see owl2_model), with variables in some of the
% terminal argument positions. E.g.
% ==
%  annotationAssertion(X,no_overlaps,Y)-disjointFrom(someValuesFrom(partOf,X),someValuesFrom(partOf,Y))
% ==
expand_axiom(Maps,A) :-
	deep_replace(A,Maps,A2),
	replace_axiom(A,A2).

replace_axiom(A,A) :- !.
replace_axiom(A,A2) :-
	debug(owlmacros,'replacing ~q ==> ~q',[A,A2]),
	retract_axiom(A),
	assert_axiom(A2).

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
/*
deep_replace(Expr,Map,Expr3) :- % TODO
	findall(Expr-Var,member(Expr-Var,Map),Replacements), 
	%debug(owlmacros,'   pattern match ~q => ~q',[Expr,Var]),
	debug(owlmacros,'   pattern match ~q ',[Replacements]),
	replace_all(Expr,Replacements,Expr2),
	Expr2\=Expr,
	!,
%deep_replace(Var,Map,Expr2),
	deep_replace(Expr2,Map,Expr3).
*/

% replace arguments of complex term:
deep_replace(Expr,Map,Expr2) :-
	Expr=..[F|Args],
	Args=[_|_],
	!,
	deep_replace(Args,Map,Args2),
	Expr2=..[F|Args2].
deep_replace(Expr,_,Expr) :- !. % atom

replace_all(Expr,[],Expr).
replace_all(Expr,[Expr-V|L],Expr2) :-
	replace_all(V,L,Expr2).
replace_all(Expr,[_|L],Expr2) :-
	replace_all(Expr,L,Expr2).



	