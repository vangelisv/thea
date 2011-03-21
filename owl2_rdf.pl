% * -*- Mode: Prolog -*- */

:- module(owl2_rdf,
          [
           owl_axiom_to_triples/2,
           materialize_owl_from_rdf,

           % TESTING:
           nc2owl/2,
           transitive_nc2owl/2,
           nc2owl_closure/2
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
  See materialize_owl_from_rdf/0 for materialization, as in the current Thea rdf module.

  To obtain a more declarative flavour, this module makes use of term expansion.
  See inline docs for more details.

  We also use DCGs in the mapping. This is useful because for
  materialization we want a record of triples used to obtain an axiom
  so we can consume them. Using DCGs we can hide the mechanics of
  collecting triples, making the code more declarative. We can also
  use the same DCG for reading and generating RDF.

  The basic pattern is:

  ==
  <AXIOM>( <X>(P2,S2,O2,Mode) ) -->
         triple(S,P,O,Mode),<EXPRESSION>(S,S2,Mode),<EXPRESSION>(P,P2,Mode),<EXPRESSION>(O,O2,Mode).
  <EXPRESSION>( BNode, Expr, Mode ) --> ...
  ==

  Mode = in | out(Source)

  The former is for translating into an owl2_model from rdf_db, the
  latter is for generating triples from owl2 axioms and expressions
  

  See inline docs for details (e.g. triple//3).

  ---+ GLOBALS

  the nb variable rdf_result_set is used to store a list of rdf(S,P,O) terms.
  this is used when constructing the model.
  useful when generating OWL axioms from SPARQL DESCRIBE queries
  
  */

% ----------------------------------------
% HOOKS
% ----------------------------------------
% see owl2_io.pl for details.

:- multifile owl_parse_axiom_hook/3.

owl2_io:load_axioms_hook(File,rdf,_Opts) :-
        debug(owl,'loading (rdf direct): ~w',[File]),
        rdf_load(File,[]),
        debug(owl,'loaded: ~w',[File]),
        !.

owl2_io:load_axioms_hook(File,rdf_direct,_Opts) :-
        debug(owl,'loading (rdf_direct): ~w',[File]),
        rdf_load(File,[]),
        debug(owl,'loaded: ~w',[File]),
        !.

owl2_io:save_axioms_hook(File,rdf_direct,Opts) :-
        debug(owl,'asserting triples to ~w [rdf_direct] Opts: ~w',[File,Opts]),
        rdf_transaction(assert_triples(Opts,DB)),
        debug(owl,'saving to ~w [rdf_direct] DB: ~w',[File,DB]),
        (   var(File)
        ->  tmp_file(owl,File),
            IsTemp=true
        ;   IsTemp=false),
        rdf_save(File,[graph(DB)]),
        debug(owl,'saved: ~w',[File]),
                % hack to allow 'saving' to standard output
        (   IsTemp
        ->  sformat(Cmd,'cat ~w',[File]),
            shell(Cmd)
        ;   true),
        !.

% ----------------------------------------
% CONVENIENCE PREDICATES
% ----------------------------------------

owl_axiom_to_triples(Axiom,Triples) :-
        owl_axiom_to_triples_withvars(Axiom,Triples),
        !,
        term_variables(Triples,BNodeVars),
        unify_bnode_vars(BNodeVars).

unify_bnode_vars([]).
unify_bnode_vars([N|Ns]) :-
        rdf_bnode(N),
        unify_bnode_vars(Ns).




% bnodes are variables
owl_axiom_to_triples_withvars(Axiom,Triples) :- phrase(owl_axiom(Axiom,out(x)),Triples),!.

assert_triples(Opts,Ont) :-
        member(ontology(Ont),Opts),
        !,
        % todo - only do this if owl axioms are asserted
        rdf_retractall(_,_,_,Ont),
        forall((ontologyAxiom(Ont,Ax),
                owl_axiom_to_triples(Ax,Triples),
                member(rdf(S,P,O),Triples)),
               rdf_assert(S,P,O,Ont)),
        rdf_retractall(_,_,_,Ont).


assert_triples(_Opts,Ont) :-
        Ont=user,
        rdf_retractall(_,_,_,Ont),
        forall((axiom(Ax),
                owl_axiom_to_triples(Ax,Triples),
                member(rdf(S,P,O),Triples),
                debug(owl,'assert: (~w,~w,~w)',[S,P,O])
                ),
               rdf_assert(S,P,O,Ont)),
        rdf_retractall(_,_,_,Ont).



% ----------------------------------------
% BRIDGE TO SEMWEB
% ----------------------------------------

:- rdf_meta
	triple(-,r,r,o,-,-).

% keep a record of 'consumed' triples. this is only used during materialization;
% in dynamic projection, triples are never consumed
:- dynamic consumed/3.

%% triple//4
% 
% generates a single triple, if that triple is in the database
triple(S,P,O,in) --> {\+compound(S),\+compound(P),\+compound(O),rdf(S,P,O),\+consumed(S,P,O)},[rdf(S,P,O)].
triple(S,P,O,out(_Src)) --> [rdf(S,P,O)].

% hacky way to temporily project triples - e.g. from sparql results
triple(S,P,O,in,Triples,_) :- nb_current(rdf_result_set,Triples),member(rdf(S,P,O),Triples).

optional_triple(S,P,O,M) --> triple(S,P,O,M),!.
optional_triple(_,_,_,_) --> [].


% ----------------------------------------
% MATERIALIZATION
% ----------------------------------------

%% materialize_owl_from_rdf
%
% consume all triples and populate an owl2_model.
% an rdf_db instance must be loaded.
% analagous to current behavior in Thea.
% TODO - optionally abolish preds before materializing
materialize_owl_from_rdf :-
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

:- discontiguous owl_axiom//2.
:- discontiguous owl_axiom_main_triple//3.

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
term_expansion((owl_axiom(Ax,M) --> Goal),
               [Rule,
                AnnotRule,
                (   owl2_model:Ax :- phrase(owl_axiom(Ax,in),_))]
               ):-
        dcg_translate_rule(owl_axiom(Ax,M) --> Goal, Rule),
        (   Goal=(HeadTriple,_),
            !
        ;   Goal=HeadTriple),
        dcg_translate_rule(owl_axiom_main_triple(Ax,M,HeadTriple) --> Goal, AnnotRule).


% -- DECLARATIONS --
% each of these matches a single triple and generates a single triple
owl_axiom(datatype(A),M) --> triple(A,rdf:type,rdfs:'Datatype',M).
owl_axiom(class(A),M) --> triple(A,rdf:type,owl:'Class',M).
owl_axiom(objectProperty(A),M) --> triple(A,rdf:type,owl:'ObjectProperty',M).
owl_axiom(dataProperty(A),M) --> triple(A,rdf:type,owl:'DataProperty',M).
owl_axiom(annotationProperty(A),M) --> triple(A,rdf:type,owl:'AnnotationProperty',M).
owl_axiom(namedIndividual(A),M) --> triple(A,rdf:type,owl:'NamedIndividual',M).

% 3.1 Extracting Declarations and the IRIs of the Directly Imported Ontology Documents
% This section specifies the result of step CP-2.2 of the canonical parsing process on an RDF graph G


% 3.1.2 Parsing of the Ontology Header and Declarations

%  Table 4.
owl_axiom(ontology(O),M) -->                triple(O,rdf:type,owl:'Ontology',M).
owl_axiom(ontologyImport(O,IRI),M) -->      triple(O,owl:imports,IRI,M).
owl_axiom(ontologyVersionInfo(O,IRI),M) --> triple(O,owl:versionInfo,IRI,M).



% -- CLASS AXIOMS --
% these typically generate multiple triples, where arguments are compound terms generated from bNodes
owl_axiom(subClassOf(AX,BX),M) --> triple(A,rdfs:subClassOf,B,M),owl_description(A,AX,M),owl_description(B,BX,M).

owl_axiom(equivalentClasses([AX,BX]),M) -->
        % always make pairwise axioms: in future we can add a collection capability
        triple(A,owl:equivalentClass,B,M),owl_description(A,AX,M),owl_description(B,BX,M). % TODO

owl_axiom(disjointClasses([AX,BX]),M) -->
        % TODO - collapse
        triple(A,owl:disjointWith,B,M),owl_description(A,AX,M),owl_description(B,BX,M).
owl_axiom(disjointUnion(AX,LX),M) -->
        triple(A,owl:disjointUnionOf,L,M),
        owl_description(A,AX,M),owl_description_list(L,LX,M).

% -- PROPERTY AXIOMS --
owl_axiom(subPropertyOf(PX,QX),M) -->
        triple(P,rdfs:subPropertyOf,Q,M),
        owl_property_expression(P,PX,M),
        owl_property_expression(Q,QX,M).

owl_axiom(subPropertyOf(propertyChain(PL),QX),M) -->
	triple(Q,owl:propertyChainAxiom,L1,M),
	owl_property_list(L1,PL,M),
        owl_property_expression(Q,QX,M).

owl_axiom(disjointProperties([PX,QX]),M) -->
        % TODO - check previous module
        triple(P,owl:propertyDisjointWith,Q,M),
        owl_property_expression(P,PX,M),
        owl_property_expression(Q,QX,M).

owl_axiom(disjointProperties(L),M) -->
        triple(P,rdf:type,owl:'AllDisjointProperties',M),
        triple(P,owl:members,L1,M),
        owl_property_list(L1,L,M).

owl_axiom(propertyDomain(PX,CX),M) -->
        triple(P,rdfs:domain,C,M),
        owl_property_expression(P,PX,M),
        owl_description(C,CX,M).

owl_axiom(propertyRange(PX,CX),M) -->
        triple(P,rdfs:range,C,M),
        owl_property_expression(P,PX,M),
        owl_description(C,CX,M).

owl_axiom(inverseProperties(PX,QX),M) -->
        % TODO - check previous module
        triple(P,owl:inverseOf,Q,M),
        owl_property_expression(P,PX,M),
        owl_property_expression(Q,QX,M).

owl_axiom(functionalProperty(P),M) --> triple(P,rdf:type,owl:'FunctionalProperty',M).
owl_axiom(inverseFunctionalProperty(P),M) --> triple(P,rdf:type,owl:'InverseFunctionalProperty',M).
owl_axiom(reflexiveProperty(P),M) --> triple(P,rdf:type,owl:'ReflexiveProperty',M).
owl_axiom(irreflexiveProperty(P),M) --> triple(P,rdf:type,owl:'IrreflexiveProperty',M).
owl_axiom(symmetricProperty(P),M) --> triple(P,rdf:type,owl:'SymmetricProperty',M).
owl_axiom(asymmetricProperty(P),M) --> triple(P,rdf:type,owl:'AsymmetricProperty',M).
owl_axiom(transitiveProperty(P),M) --> triple(P,rdf:type,owl:'TransitiveProperty',M).

owl_axiom(hasKey(CX,L),M) -->
	triple(C,owl:hasKey,L1,M),
	owl_description(C,CX,M),
        L1 = [_,_|_],           % length >= 2
        owl_property_list(L1,L,M).

% --- Individual Axioms ---

owl_axiom(sameIndividual([X,Y]),M) -->        triple(X,owl:sameAs,Y,M).


owl_axiom(differentIndividuals([X,Y]),M) --> triple(X,owl:differentFrom,Y,M).

owl_axiom(differentIndividuals(L),M) -->
	triple(X,rdf:type,owl:'AllDifferent',M),
	triple(X,owl:distinctMembers,L1,M), % todo - check if variant deprecated
        owl_individual_list(L1,L,M).

owl_axiom(differentIndividuals(L),M) -->
	triple(X,rdf:type,owl:'AllDifferent',M),
	triple(X,owl:members,L1,M), % todo - check if variant deprecated
        owl_individual_list(L1,L,M).



% TODO - everything else

% ----------------------------------------
% Table 17. Parsing of Annotated Axioms
% ----------------------------------------

owl_axiom(annotationAssertion(P,A,B),M) --> triple(A,P,B,M),{is_annotationProperty(P)}.

% TODO
owl_axiom(propertyAssertion(PX,A,B),M) -->
        triple(A,P,B,M),{\+annotationProperty(P),\+rdf_is_bnode(A),\+rdf_is_bnode(B)},
        owl_property_expression(P, PX,M),
        {\+ builtin(PX)}.

owl_axiom(negativePropertyAssertion(PX,A,B),M) -->
        triple(X,rdf:type,owl:'NegativePropertyAssertion',M),
        triple(X,owl:sourceIndividual,A,M),
        triple(X,owl:assertionProperty,P,M),
        triple(X,owl:targetValue,B,M),
        owl_property_expression(P,PX,M).

owl_axiom(classAssertion(CX,I),M) -->
        triple(I,rdf:type,C,M),
        owl_description(C,CX,M),
        {\+ builtin(CX)}.

:- rdf_meta builtin(r).
builtin(rdfs:subClassOf).
builtin(rdf:type).
builtin(X) :- rdf_global_id(rdf:_,X).
builtin(X) :- rdf_global_id(rdfs:_,X).
builtin(X) :- rdf_global_id(owl:_,X).


:- rdf_meta is_annotationProperty(r).
is_annotationProperty(rdfs:label) :- !.
is_annotationProperty(rdfs:comment) :- !.
is_annotationProperty(P) :- annotationProperty(P),!.

% Parsing annotationAssertions - TODO

% Table 18. Parsing of Axioms for Compatibility with OWL DL - TODO

asserted_equivalent_to(AX,BX,M) --> triple(A,owl:equivalentClass,B,M),owl_description(A,AX,M),owl_description(B,BX,M).
maximal_equivalence_set(EqL,SeedL,in) --> {member(A,SeedL)},(asserted_equivalent_to(A,B,M);asserted_equivalent_to(B,A,M)),{\+member(B,SeedL)},!,maximal_equivalence_set(EqL,[B|SeedL]).
maximal_equivalence_set(EqSet,EqL,in) --> {sort(EqL,EqSet)},[]. % impossible to add more members; this is the maximal set

% v. slow for bound Ont..
owl_ontology_axiom_triples(Ont,A,Triples) :-
        nonvar(Ont),
        rdf(Ont,rdf:type,owl:'Ontology',Src:_),
        rdf(S,P,O,Src:_),
        Triples=[rdf(S,P,O)|_],
        phrase(owl_axiom(A,in),Triples).
owl_ontology_axiom_triples(Ont,A,Triples) :-
        var(Ont),
        phrase(owl_axiom(A,in),Triples),
        Triples=[rdf(S,P,O)|_],
        rdf(S,P,O,Src:_),
        rdf(Ont,rdf:type,owl:'Ontology',Src:_).

owl2_model:ontologyAxiom(Ont,A) :-
        owl_ontology_axiom_triples(Ont,A,_).

owl_axiom_annotations(Ax,Anns,M) -->
        triple(ReifAx,rdf:type,owl:'Axiom',M),
        triple(ReifAx,owl:annotatedProperty,Px,M),
        triple(ReifAx,owl:annotatedSource,Sx,M),
        triple(ReifAx,owl:annotatedTarget,Tx,M),
        owl_axiom_main_triple(Ax,M,triple(Sx,Px,Tx),M),
        owl_annotations_for(ReifAx,[],Anns,M).

owl_annotations_for(A,Done,Anns,M) -->
        owl_axiom(annotationAssertion(P,A,V),M),
        {\+member(P-V,Done)},
        !,
        owl_annotations_for(A,[P-V|Done],Anns,M).
owl_annotations_for(_,Anns,Anns,_) --> [].

:- abolish(owl2_model:axiomAnnotation/3).
owl2_model:axiomAnnotation(Ax,AP,AV) :-
        phrase(owl_axiom_annotations(Ax,Anns,in),_),
        member(AP-AV,Anns).

%owl_annotations(ReifAx,[Ann|Anns],M) -->
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

owl_description(X,X,M) --> {\+is_anonymous(X,M)},!,[].




% Table 12. Parsing of Data Ranges
% TODO

% ----------------------------------------
% Table 13. Parsing of Class Expressions
% ----------------------------------------

owl_description(D,intersectionOf(L),M) -->
	triple(D,owl:intersectionOf,L1,M),
	owl_description_list(L1,L,M),
	{\+L = []}.
owl_description(D,unionOf(L),M) -->
	triple(D,owl:unionOf,L1,M),
	owl_description_list(L1,L,M),
	{\+L = []}.
owl_description(D,complementOf(Y),M) -->
	triple(D,owl:complementOf,X,M),
	owl_description(X,Y,M).
owl_description(D,oneOf(L),M) -->
	triple(D,owl:oneOf,L1,M),
	optional_triple(D,rdf:type,owl:'Class',M),
	owl_individual_list(L1,L,M).

owl_description(D,datatypeRestriction(DY,L),M) -->
	triple(D,rdf:type,rdfs:'Datatype',M),
	triple(D,owl:onDatatype,Y,M),
	owl_datarange(Y,DY,M),
	triple(D,owl:withRestrictions,L1,M),
	owl_datatype_restriction_list(L1,L,M).

% Restrictions
owl_description(D,Restriction,M) -->
	owl_restriction(D, Restriction,M).

owl_restriction(Element,Restriction,M) -->
	triple(Element,rdf:type,owl:'Restriction',M),
	triple(Element,owl:onProperty,PropertyID,M),
        % todo: need onProperties? see from_rdf.pl
	owl_restriction_type(Element,PropertyID, Restriction,M).

owl_restriction_type(E, P, someValuesFrom(PX, DX),M) -->
	triple(E, owl:someValuesFrom,D,M),
	owl_description(D, DX,M),
        owl_property_expression(P, PX,M).

owl_restriction_type(E, P, allValuesFrom(PX,DX),M) -->
	triple(E, owl:allValuesFrom,D,M),
	owl_description(D, DX,M),
        owl_property_expression(P, PX,M).

% changed from thea value-->hasValue
owl_restriction_type(E, P, hasValue(PX,Value),M) -->
	triple(E, owl:hasValue,Value,M),
        owl_property_expression(P, PX,M).

% VV:check if RDF parser returns a triple with O=true for
% "true"^^xsd:boolean
owl_restriction_type(E, P, hasSelf(PX),M) -->
	triple(E, owl:hasSelf, true,M),
        owl_property_expression(P, PX,M).

% --- Cardinality
% TODO: check if we still need unsupported QCRs
owl_restriction_type(E, P,exactCardinality(N,PX),M) -->
	triple(E, owl:cardinality,Lit,M),
        {literal_integer(Lit,N)},
        owl_property_expression(P, PX,M).

owl_restriction_type(E, P,exactCardinality(N,PX,DX),M) -->
	triple(E, owl:qualifiedCardinality,Lit,M),
        {literal_integer(Lit,N)},
	(   onClass(E,D,M),
            owl_description(D, DX,M)
        ;   triple(E, owl:onDataRange,D,M),
            owl_datarange(D,DX,M)
	),
        owl_property_expression(P, PX,M).


owl_restriction_type(E, P, minCardinality(N,PX),M) -->
	triple(E, owl:minCardinality,Lit,M),
        {literal_integer(Lit,N)},
        owl_property_expression(P, PX,M).

owl_restriction_type(E, P, minCardinality(N,PX,DX),M) -->
	triple(E, owl:minQualifiedCardinality,Lit,M),
        {literal_integer(Lit,N)},
	(   onClass(E,D,M),
            owl_description(D, DX,M)
        ;   
	    triple(E, owl:onDataRange,D,M),
            owl_datarange(D,DX,M)
	),
        owl_property_expression(P, PX,M).


owl_restriction_type(E, P, maxCardinality(N,PX),M) -->
	triple(E, owl:maxCardinality,Lit,M),
        {literal_integer(Lit,N)},
        owl_property_expression(P, PX,M).

owl_restriction_type(E, P, maxCardinality(N,PX,DX),M) -->
	triple(E, owl:maxQualifiedCardinality,Lit,M),
	{literal_integer(Lit,N)},
	(   onClass(E,D,M),
            owl_description(D, DX,M)
        ;   triple(E, owl:onDataRange,D,M),
            owl_datarange(D,DX,M)),
        owl_property_expression(P, PX,M).

owl_restriction_type(E, P, maxCardinality(N,PX,DX),M) -->
	triple(E, owl:maxQualifiedCardinality,Lit,M),
	{literal_integer(Lit,N)},
	(   onClass(E,D,M),
            owl_description(D, DX,M)
        ;   triple(E, owl:onDataRange,D,M),
            owl_datarange(D,DX,M)),
        owl_property_expression(P, PX,M).


% support older deprecated versions of OWL2 spec. See for example hydrology.owl
% TODO: drop support for these?
onClass(E,D,M) --> triple(E,'http://www.w3.org/2006/12/owl2#onClass',D,M).
onClass(E,D,M) --> triple(E,owl:onClass,D,M).

onDataRange(E,D,M) --> triple(E, owl:onDataRange,D,M).

% --- End of Cardinality ---


owl_property_expression(P,inverseOf(Q),M) -->
        is_anonymous(P,M),
        triple(P,owl:inverseOf,Q,M),
        !.
owl_property_expression(P,P,_) --> [].

% ----------------------------------------
% LISTS
% ----------------------------------------

is_rdf_nil('http://www.w3.org/1999/02/22-rdf-syntax-ns#nil').
owl_description_list(X,[],_) --> {is_rdf_nil(X)},[],!.

owl_description_list(X,[F|R],M) -->
	% triple(X,'rdf:type','rdf:List',list), % this is now removed from graph - TODO - MAKE OPTIONAL
	triple(X,rdf:first,Element,M),
	owl_description(Element,F,M),
	triple(X,rdf:rest,Y,M),
	!,
        owl_description_list(Y,R,M).

owl_individual_list(X,[],_) --> {is_rdf_nil(X)},[],!.

owl_individual_list(X,[F|R],M) -->
	% use_owl(X,'rdf:type','rdf:List',list), % this is now removed from graph - TODO - MAKE OPTIONAL
	triple(X,rdf:first,F,M),
	triple(X,rdf:rest,Y,M),
	!,
        owl_individual_list(Y,R,M).

owl_property_list(X,[],_) --> {is_rdf_nil(X)},[],!.

owl_property_list(X,[F|R],M) -->
	% use_owl(X,'rdf:type','rdf:List',list), % this is now removed from graph - TODO - MAKE OPTIONAL
	triple(X,rdf:first,Element,M),
	owl_property_expression(Element,F,M),
	triple(X,rdf:rest,Y,M),
	!,
        owl_property_list(Y,R,M).

owl_datarange_list(X,[],_) --> {is_rdf_nil(X)},[],!.

owl_datarange_list(X,[F|R],M) -->
	% use_owl(X,'rdf:type','rdf:List',list), % this is now removed from graph
	triple(X,rdf:first,Element,M),
	owl_datarange(Element,F,M),
	triple(X,rdf:rest,Y,M),
	!,
        owl_datarange_list(Y,R,M).

owl_datatype_restriction_list(X,[],_) --> {is_rdf_nil(X)},[],!.

owl_datatype_restriction_list(X,[facetRestriction(W2,L)|R],M) -->
	% use_owl(X,'rdf:type','rdf:List'), % this is now removed from graph
	rdf(X,rdf:first,Element,M),
	rdf(Element,W,L,M),
	{(   concat_atom([_,W2],'#',W)
         ->  true
         ;   W2=W)},
	triple(X,rdf:rest,Y,M),
	!,
        owl_datatype_restriction_list(Y,R,M).

% ----------------------------------------
% UTILS
% ----------------------------------------

literal_integer(literal(type,A),N) :- atom_number(A,N).
literal_integer(literal(type(_,A)),N) :- atom_number(A,N).

is_anonymous(X,in) :- rdf_is_bnode(X).
is_anonymous(X,out(_)) :- \+ atom(X).


% ----------------------------------------
% TEST
% ----------------------------------------

% this will move into another module - just for demo purposes
% we can dynamically project owl axioms from an external triplestore.

% query NC Virtuoso triplestore and turn DESCRIBE triples into OWL ontology
% e.g. nc2owl('GO_0009245',Axiom)
nc2owl(ID,Axiom) :-
        ensure_loaded(semweb(sparql_client)),
        (   sub_atom(ID,_,_,_,:)
        ->  URI=ID
        ;   atom_concat('http://purl.obolibrary.org/obo/',ID,URI)),
        sparql_set_server([host('sparql.obodev.neurocommons.org'),port(80),path('/sparql/')]),
        concat_atom(['DEFINE sql:describe-mode "CBD"\nDESCRIBE',' ','<',URI,'>'],Q),
        debug(sparql,'QUERY: ~w',[Q]),
        findall(R,
                sparql_query(Q,R,[search([format='application/rdf+xml'])]),
                Triples),
        triples_to_owl_axiom(Triples,Axiom).

transitive_nc2owl(URI,Axiom) :-
        transitive_nc2owl([URI],Axioms,[]),
        member(Axiom,Axioms).
transitive_nc2owl([URI|URIs],AllAxioms,Visited) :-
        findall(Axiom,nc2owl(URI,Axiom),Axioms),
        findall(P,(member(subClassOf(_,P),Axioms),
                   \+ member(P,Visited)),
                Ps),
        append(Ps,URIs,NextURIs),
        sort(NextURIs,NextURIsUnique),
        transitive_nc2owl(NextURIsUnique,Axioms2,[URI|Visited]),
        append(Axioms,Axioms2,AllAxioms).
transitive_nc2owl([],[],_).

% assume closure of subclassof has been calculated in triplestore
nc2owl_closure(ID,Axiom) :-
        nc2owl(ID,Axiom).
nc2owl_closure(ID,Axiom) :-
        ensure_loaded(semweb(sparql_client)),
        (   sub_atom(ID,_,_,_,:)
        ->  URI=ID
        ;   atom_concat('http://purl.obolibrary.org/obo/',ID,URI)),
        sparql_set_server([host('sparql.obodev.neurocommons.org'),port(80),path('/sparql/')]),
        concat_atom(['DEFINE sql:describe-mode "CBD"\nDESCRIBE ?x WHERE {<',URI,'> rdfs:subClassOf ?x } '],Q),
        debug(sparql,'QUERY: ~w',[Q]),
        findall(R,
                sparql_query(Q,R,[search([format='application/rdf+xml'])]),
                Triples),
        add_triples_to_owl_axiom(Triples,Axiom).



triples_to_owl_axiom(Triples,A) :-
        nb_setval(rdf_result_set,Triples),
        %writeln(triples=Triples),
        phrase(owl_axiom(A),_).

add_triples_to_owl_axiom(Triples1,A) :-
        nb_getval(rdf_result_set,Triples2),
        append(Triples1,Triples2,Triples),
        nb_setval(rdf_result_set,Triples),
        %writeln(triples=Triples),
        phrase(owl_axiom(A),_).


