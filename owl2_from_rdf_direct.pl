% * -*- Mode: Prolog -*- */

:- module(owl2_from_rdf_direct,
          [
           ]).

:- use_module(owl2_model).
:- use_module(library(semweb/rdf_db)).

/** <module> RDF direct mapping to owl2_model

  This provides dynamic/direct access to owl2_model facts over an RDF database.

  This module is experimental / proof of concept. The idea is to explore an
  alternative approach to owl2_from_rdf.pl, which follows the OWL2 mapping to rdf document,
  and implements the following procedure:
  
  * collect all rdf facts using a dynamic owl/4 predicate
  * perform operations on the graph according to the spec (including removal of triples)
  * materialize owl2_model (i.e. assert dynamic predicates) using mapping

  One problem is that this is quite slow for larger ontologies. It's also not suitable for
  dynamically changing RDF stores.

  An alternative approach epxlored here is to *dynamically* project owl2_model facts
  from an RDF database. This takes advantage of the efficient rdf/3 calls in the semweb
  package. The main challenge here is that the mapping is monotonic - triples are removed
  from the graph. It remains to be seen how best to deal with this. One option is to have the direct
  mapping have an OWL2-Full flavour, and include all triples as propertyAssertion/3 facts.
  Another option is to use the same code for both materialization and dynamic projection.
  See materialize_owl_model/0 for materialization, as in the current Thea rdf module.

  To obtain a more declarative flavour, this module makes use of term expansion.
  See inline docs for more details.

  We also use DCGs in the mapping. This is useful because for materialization we want
  a record of triples used to obtain an axiom so we can consume them. Using DCGs we can
  hide the mechanics of collecting triples, making the code more declarative. The basic pattern is:

  ==
  AXIOM( X(P2,S2,O2) ) --> triple(S,P,O),EXPRESSION(S,S2),EXPRESSION(P,P2),EXPRESSION(O,O2).
  EXPRESSION( BNode, Expr ) --> ...
  ==

  See inline docs for details (e.g. triple//3).
  
  */

% ----------------------------------------
% HOOKS
% ----------------------------------------
% see owl2_io.pl for details.

:- multifile owl_parse_axiom_hook/3.

owl2_io:load_axioms_hook(File,rdf,Opts) :-
        debug(owl,'loading: ~w',[File]),
        rdf_load(File,Opts).

% ----------------------------------------
% BRIDGE TO SEMWEB
% ----------------------------------------

:- rdf_meta
	triple(-,r,r,o,-).

% keep a record of 'consumed' triples. this is only used during materialization;
% in dynamic projection, triples are never consumed
:- dynamic consumed/3.

%% triple//3
% 
% generates a single triple, if that triple is in the database
triple(S,P,O) --> {rdf(S,P,O),\+consumed(S,P,O)},[rdf(S,P,O,_)].

% ----------------------------------------
% MATERIALIZATION
% ----------------------------------------


%% materialize_owl_model
%
% consume all triples and populate an owl2_model.
% an rdf_db instance must be loaded.
% analagous to current behavior in Thea.
% TODO - optionally abolish preds before materializing
materialize_owl_model :-
        forall(owl_ontology_axiom_triples(Ont,Ax,Triples),
               materialize_owl_axiom(Ont,Ax,Triples)).

materialize_owl_axiom(Ont,Ax,Triples) :-
        assert_axiom(Ax,Ont),
        mark_consumed(Triples).

mark_consumed([]) :- !.
mark_consumed([rdf(S,P,O)|T]) :-
        mark_consumed(S,P,O),
        mark_consumed(T).
mark_consumed(S,P,O) :- assert(consumed(S,P,O)).

% ----------------------------------------
% MAPPING OF AXIOMS
% ----------------------------------------

:- discontiguous owl_axiom//1.
:- discontiguous owl_axiom_main_triple//2.

% -- Mapping DCGs --
% 
% Each DCG rule unifies the head argument with an axiom or an expression,
% and generates the complete list of triples required to make that expression;
% this includes all bNodes required to make sub-expressions.
% the list is of the form [rdf(S,P,O), ...]
% 
% -- TERM EXPANSION --
%
% these expansions take DCG axiom rules and uses them to additionally generate:
%
% * bridge axioms;
%  e.g
%  ==
%  owl_axiom(subClassOf(A,B)) --> ...
%      ===>
%  owl2_model:subClassOf(A,B) :- phrase(owl_axiom(subClassOf(A,B),_Triples)).
%  ==
%
% * axiom annotation phrase clauses
% e.g.
%  ==
%  owl_axiom(subClassOf(A,B)) --> triple(S,rdfs:subClassOf,O),....
%      ===>
%  owl_axiom_main_triple(subClassOf(A,B),triple(S,rdfs:subClassOf,O)) --> triple(S,rdfs:subClassOf,O),....
%  ==
% these are used to defined axiomAnnotation/3 - the 2nd argument is the main triple
% TODO - axioms that don't have a main triple
term_expansion((owl_axiom(Ax) --> Goal),
               [Rule,
                AnnotRule,
                (   owl2_model:Ax :- phrase(owl_axiom(Ax),_))]
               ):-
        dcg_translate_rule(owl_axiom(Ax) --> Goal, Rule),
        (   Goal=(HeadTriple,_),
            !
        ;   Goal=HeadTriple),
        dcg_translate_rule(owl_axiom_main_triple(Ax,HeadTriple) --> Goal, AnnotRule).


% -- DECLARATIONS --
% each of these matches a single triple and generates a single triple
owl_axiom(datatyoe(A)) --> triple(A,rdf:type,rdfs:'Datatype').
owl_axiom(class(A)) --> triple(A,rdf:type,owl:'Class').
owl_axiom(objectProperty(A)) --> triple(A,rdf:type,owl:'ObjectProperty').
owl_axiom(dataProperty(A)) --> triple(A,rdf:type,owl:'DataProperty').
owl_axiom(annotationProperty(A)) --> triple(A,rdf:type,owl:'AnnotationProperty').
owl_axiom(namedIndividual(A)) --> triple(A,rdf:type,owl:'NamedIndividual').

% -- CLASS AXIOMS --
% these typically generate multiple triples, where arguments are compound terms generated from bNodes
owl_axiom(subClassOf(A,B)) --> triple(Ax,rdfs:subClassOf,Bx),owl_description(Ax,A),owl_description(Bx,B).
owl_axiom(equivalentClasses(L)) -->
        asserted_equivalent_to(A,B), % TODO - DOES NOT HAVE MAIN TRIPLE!! NEED A GENERIC WAY OF HANDLING THESE
        maximal_equivalence_set(L,[A,B]). % todo - eliminate duplicates
owl_axiom(disjointClasses(A,B)) --> triple(Ax,owl:disjointWith,Bx),owl_description(Ax,A),owl_description(Bx,B).
% TODO owl:AllDisjointClasses
% TODO owl:disjointUnion

% -- PROPERTY AXIOMS --
owl_axiom(subPropertyOf(A,B)) --> triple(A,rdfs:subPropertyOf,B).
% TODO owl:propertyChainAxiom
% TODO - everything else

% -- ANNOTATION AXIOMS --
owl_axiom(annotationAssertion(P,A,B)) --> triple(A,P,B),{annotationProperty(P)}.

asserted_equivalent_to(A,B) --> triple(Ax,owl:equivalentTo,Bx),owl_description(Ax,A),owl_description(Bx,B).
maximal_equivalence_set(EqL,SeedL) --> {member(A,SeedL)},(asserted_equivalent_to(A,B);asserted_equivalent_to(B,A)),{\+member(B,SeedL)},!,maximal_equivalence_set(EqL,[B|SeedL]).
maximal_equivalence_set(EqSet,EqL) --> {sort(EqL,EqSet)},[]. % impossible to add more members; this is the maximal set

owl_ontology_axiom_triples(Ont,A,Triples) :-
        phrase(owl_axiom(A),Triples),
        Triples=[rdf(S,P,O)|_],
        rdf(S,P,O,Src:_),
        rdf(Ont,rdf:type,owl:'Ontology',Src:_).

owl2_model:ontologyAxiom(Ont,A) :-
        owl_ontology_axiom_triples(Ont,A,_).

owl_axiom_annotations(Ax,Anns) -->
        triple(ReifAx,rdf:type,owl:'Axiom'),
        triple(ReifAx,owl:annotatedProperty,Px),
        triple(ReifAx,owl:annotatedSource,Sx),
        triple(ReifAx,owl:annotatedTarget,Tx),
        owl_axiom_main_triple(Ax,triple(Sx,Px,Tx)),
        owl_annotations_for(ReifAx,[],Anns).

owl_annotations_for(A,Done,Anns) -->
        owl_axiom(annotationAssertion(P,A,V)),
        {\+member(P-V,Done)},
        !,
        owl_annotations_for(A,[P-V|Done],Anns).
owl_annotations_for(_,Anns,Anns) --> [].

:- abolish(owl2_model:axiomAnnotation/3).
owl2_model:axiomAnnotation(Ax,AP,AV) :-
        phrase(owl_axiom_annotations(Ax,Anns),_),
        member(AP-AV,Anns).

%owl_annotations(ReifAx,[Ann|Anns]) -->
%        owl_annotation(ReifAx,Ann),



% ----------------------------------------
% EXPRESSIONS
% ----------------------------------------

% all DGC rules here have two arguments; the first argument is the
% input bNode or URI, which should be ground. The second argument is
% the expression generated from that bNode or simply the URI.
% the rule generates the list of triples used to construct that expression.

% NOT FULLY IMPLEMENTED
% code partially copied and pasted from owl2_from_rdf.pl

owl_description(X,X) --> {\+rdf_is_bnode(X)},!,[].

owl_description(D,intersectionOf(L)) -->
	triple(D,owl:intersectionOf,L1),
	owl_description_list(L1,L),
	{\+L = []}.

owl_description(D,Restriction) -->
	owl_restriction(D, Restriction).

owl_restriction(Element,Restriction) -->
	triple(Element,rdf:type,owl:'Restriction'),
	triple(Element,owl:onProperty,PropertyID),
	owl_restriction_type(Element,PropertyID, Restriction).

owl_restriction_type(E, P, someValuesFrom(PX, DX)) -->
	triple(E, owl:someValuesFrom,D),
	owl_description(D, DX),
        owl_property_expression(P, PX).

owl_restriction_type(E, P, allValuesFrom(PX,DX)) -->
	triple(E, owl:allValuesFrom,D),
	owl_description(D, DX),
        owl_property_expression(P, PX).


% changed from thea value-->hasValue
owl_restriction_type(E, P, hasValue(PX,Value)) -->
	triple(E, owl:hasValue,Value),
        owl_property_expression(P, PX).

% VV:check if RDF parser returns a triple with O=true for
% "true"^^xsd:boolean
owl_restriction_type(E, P, hasSelf(PX)) -->
	triple(E, owl:hasSelf, true),
        owl_property_expression(P, PX).

owl_restriction_type(E, P,exactCardinality(N,PX)) -->
	triple(E, owl:cardinality,Lit),
        {literal_integer(Lit,N)},
        owl_property_expression(P, PX).

owl_restriction_type(E, P,exactCardinality(N,PX,DX)) -->
	triple(E, owl:qualifiedCardinality,Lit),
        {literal_integer(Lit,N)},
	(   onClass(E,D),owl_description(D, DX)
        ;   onDataRange(E,D), owl_datarange(D,DX)
	),
        owl_property_expression(P, PX).


owl_restriction_type(E, P, minCardinality(N,PX)) -->
	triple(E, owl:minCardinality,Lit),
        {literal_integer(Lit,N)},
        owl_property_expression(P, PX).

owl_restriction_type(E, P, minCardinality(N,PX,DX)) -->
	triple(E, owl:minQualifiedCardinality,Lit),
        {literal_integer(Lit,N)},
	(   onClass(E,D),owl_description(D, DX)
        ;   
	    onDataRange(E,D), owl_datarange(D,DX)
	),
        owl_property_expression(P, PX).


owl_restriction_type(E, P, maxCardinality(N,PX)) -->
	triple(E, owl:maxCardinality,Lit),
        {literal_integer(Lit,N)},
        owl_property_expression(P, PX).

owl_restriction_type(E, P, maxCardinality(N,PX,DX)) -->
	triple(E, owl:maxQualifiedCardinality,Lit),
	{literal_integer(Lit,N)},
	(   onClass(E,D),owl_description(D, DX);
	    onDataRange(E,D), owl_datarange(D,DX)),
        owl_property_expression(P, PX).

owl_restriction_type(E, P, maxCardinality(N,PX,DX)) -->
	triple(E, owl:maxQualifiedCardinality,Lit),
	{literal_integer(Lit,N)},
	(   onClass(E,D),owl_description(D, DX);
	    onDataRange(E,D), owl_datarange(D,DX)),
        owl_property_expression(P, PX).


% support older deprecated versions of OWL2 spec. See for example hydrology.owl
onClass(E,D) --> triple(E,'http://www.w3.org/2006/12/owl2#onClass',D).
onClass(E,D) --> triple(E,owl:onClass,D).

onDataRange(E,D) --> triple(E, owl:onDataRange,D).

owl_property_expression(P,inverseOf(Q)) -->
        triple(P,owl:inverseOf,Q),
        !.
owl_property_expression(P,P) --> [].


owl_description_list(rdf:nil,[]) --> [],!.

owl_description_list(X,[F|R]) -->
	% triple(X,'rdf:type','rdf:List',list), % this is now removed from graph - TODO - MAKE OPTIONAL
	triple(X,rdf:first,Element),
	owl_description(Element,F),
	triple(X,rdf:rest,Y),
	!,
        owl_description_list(Y,R).

% ----------------------------------------
% UTILS
% ----------------------------------------

literal_integer(literal(type,A),N) :- atom_number(A,N).
literal_integer(literal(type(_,A)),N) :- atom_number(A,N).
