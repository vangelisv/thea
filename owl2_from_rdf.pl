% * -*- Mode: Prolog -*- */
% **********************************************************************
%                                OWL PARSER
% Author: Vangelis Vassiliadis
% Change Log:
%             May 04: Initial release 0.1
%             Jan 05: Version 0.2 - Some code optimisation.
%             Feb 05: DisjointClass, EquivalentClass
%             Mar 05: DifferentFrom
%             Feb 09: OWL2 (CJM)
%
% Version 0.3 First release, March 22, 2005
%
% Version 0.4 Changes (use of SWI's rdf parser 5.5.12)
%             * rdf_load/2 options to noshare of blank nodes and use a
%               convert function to hold datatyped values.
%             * removed fix_owl. Apparently is not needed with SWI 5.5.x
%	      * Implemented equivalentProperties by re-using the logic
%	        of equivalentClass and sameAs.
%	      * Implementation of owl:imports (handled at RDF/triple
%		level) by importing all RDF triples prior to building
%		the the OWL abstract syntax terms. Flag to select if
%		imports will be handled.
%	      * OWL parser can parse now either local files or URLs (in-
%		line with Semweb's package RDF parser). Use of SWI's
%		http package.
%	      * owl_pack_ontology, owl_report
% Version 0.5.5: March 07: Changes to the use_module and definitions for Thea 0.5.5 release.
% To do        Check and report for purely Internal (DL errors)
%              Inferences
%  Changes for GIT
% **********************************************************************



:- module(owl2_from_rdf,
	  [
           owl_parse_rdf/1,
           owl_parse_rdf/2,
           translate_rdf_db/1,
           owl_parse/4,
           rdf_db_to_owl/0,
            convert/3,
	    expand_ns/2,                  %  ?NS_URL, ?Full_URL
	    collapse_ns/4,

            valid_axiom_annotation_mode/5,
	    uri_split/4,

            owl_description/2,
	    blanknode/3,
	    use_owl/4,
	    test_use_owl/3 % expose them to allow external handling of triples, e.g. for rdfs support
	    % owl_parser_log/2 -- deprecated, use debug instead.
	  ]).

/** <module> Translates an RDF database to OWL2 axioms
  ---+ Synopsis 2
==
:- use_module(bio(owl2_from_rdf)).
%
==
---+ Details
---++ Hooks
* owl_parse_axiom_hook/3
---+ See Also
The file owl2_from_rdf.plt has some examples
*/


:- use_module(owl2_model).

:- use_module(library(debug)).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdf_edit')).
:- use_module(library('semweb/rdfs')).
:- use_module(library('url')).
:- use_module(library('http/http_open')).

:- dynamic(owl/4).
%% blanknode(Node,Description,Used)
% see owl_get_bnode/2
% Node - bNodeId
% Description - prolog term corresponding to owl Description
% Used - used | shared
:- dynamic(blanknode/3).
:- dynamic(outstream/1).

:- dynamic(aNN/3). % implements the ANN(X) function.
:- dynamic(annotation_r_node/4).  % annotation_r_node(S,P,O,Node)
:- dynamic(axiom_r_node/4).       % axiom_r_node(S,P,O,Node)
:- dynamic(owl_repository/2). % implements a simple OWL repository: if URL not found, Ontology is read from a repository (local) RURL
:- multifile(owl_repository/2).

% we make this discontiguous so that the code can follow the structure of the document as much as possible

:- discontiguous owl_parse_axiom/3.
:- discontiguous dothislater/1.

% hookable
:- multifile owl_parse_axiom_hook/3.

:- include('owl2_from_rdf_utils.pl').

% -----------------------------------------------------------------------
%                                Top Level  Predicates
% -----------------------------------------------------------------------

:- multifile owl2_io:load_axioms_hook/3.

owl2_io:load_axioms_hook(File,owl,Opts) :-
        owl_parse_rdf(File,Opts).

owl2_io:load_axioms_hook(File,ttl,Opts) :-
        ensure_loaded(library('semweb/rdf_turtle')),
        owl_parse_rdf(File,Opts).

%% owl_parse_rdf(+File)
% as owl_parse_rdf/1 with empty Opts
owl_parse_rdf(F):-
	owl_parse_rdf(F,[]).

%% owl_parse_rdf(+File,+Opts:list)
% @param Opts
%  * imports(ImportFlag:Boolean) if true, follow imports
%  * clear(Clear) if Clear=complete, clears all axioms in owl2_model
owl_parse_rdf(F,Opts):-
	(   member(imports(Imports),Opts)
	->  true
	;   Imports=false),
	(   member(clear(Clear),Opts)
	->  true
	;   Clear=false),
	owl_parse(F,Clear,Clear,Imports),
	debug(owl_parser,'parsed ~w',[F]).




%% owl_parse(+URL, +RDF_Load_Mode, +OWL_Parse_Mode, +ImportFlag:boolean)
%
%  Top level: parse a set of RDF triples and produce an
%  AS representation of an OWL ontology.
%
%	Calls the rdf_load_stream predicate to parse RDF stream in URL.
%       If RDF_Load_Mode = complete it first retacts all rdf triples.
%       If ImportFlag = true it handles owl:import clause at RDF level.
%
% This implements the mapping defined here:
% http://www.w3.org/TR/2008/WD-owl2-mapping-to-rdf-20081202/
owl_parse(URL, RDF_Load_Mode, OWL_Parse_Mode,ImportFlag) :-
	(   RDF_Load_Mode=complete
	->  rdf_retractall(_,_,_), retractall(rdf_db:rdf_source(_,_,_,_))
        ;   true),
	(   OWL_Parse_Mode=complete
        ->  owl_clear_as,retractall(blanknode(_,_,_)), retractall(owl(_,_,_,_))
        ;   true),
        !,
        debug(owl_parser,'Loading stream ~w',[URL]),
	owl_canonical_parse_2([URL],URL,ImportFlag,[],ProcessedIRIs),
        debug(owl_parser,'rdf_db populated, the following IRIs were processed: ~w',[ProcessedIRIs]),
	owl2_model_init,
	owl_canonical_parse_3(ProcessedIRIs).

%% owl_canonical_parse_2(+IRIs:list,+ParentIRI,+ImportFlag:boolean,+ProcessedURIsIn:list,?ProcessedURIsOut:list) is det
% recursively parses all ontologies in IRIs into rdf_db, ensuring none are processed twice.
owl_canonical_parse_2([],_,_,Processed,Processed) :- !.

owl_canonical_parse_2([IRI|ToProcessRest],Parent,ImportFlag,ProcessedIn,ProcessedOut) :-
	member(IRI,ProcessedIn),
        !,
	owl_canonical_parse_2(ToProcessRest,Parent,ImportFlag,ProcessedIn,ProcessedOut).

owl_canonical_parse_2([IRI|ToProcessRest],Parent,ImportFlag,ProcessedIn,ProcessedOut) :-
	% Get rdf triples, *Ontology* and Imports
	rdf_load_stream(IRI,O,BaseURI,Imports),
	(   nonvar(O)
        ->  Ont = O
        ;   Ont = Parent), % in the include case we may need to remove the import...
        debug(owl_parser,'Commencing rdf_2_owl. Generating owl/4',[]),
	rdf_2_owl(BaseURI,Ont),  	% move the RDF triples into the owl-Ont/4 facts
	(   ImportFlag = true
        ->  owl_canonical_parse_2(Imports,Ont,ImportFlag,[Ont|ProcessedIn],ProcessedIn1)
        ;   ProcessedIn1=[Ont|ProcessedIn]),
	owl_canonical_parse_2(ToProcessRest,Parent,ImportFlag,ProcessedIn1,ProcessedOut).


%% owl_canonical_parse_3(+IRIs:list) is det
% translate the current rdf_db into owl2_model axioms.
% First owl/4 facts are populated, and then these are translated
% according to:
% http://www.w3.org/TR/2008/WD-owl2-mapping-to-rdf-20081202/
% (table references refer to this document).
% we use an intermediate owl/4 database because the mapping
% is non-monotonic, and triples are 'consumed'
owl_canonical_parse_3([]).

owl_canonical_parse_3([IRI|Rest]) :-
	% Remove any existing not used owl fact
	retractall(owl(_,_,_,not_used)),
	% Copy the owl facts of the IRI document to the 'not_used'
	forall(owl(S,P,O,IRI),assert(owl(S,P,O,not_used))),

        debug(owl_parser,'Anon individuals in reification [see table 8]',[]),


	collect_r_nodes,

	% First parse the Ontology axiom
        owl_parse_annotated_axioms(ontology/1),

        debug(owl_parser,'Replacing patterns [see table 5]',[]),
	% remove triples based on pattern match (Table 5)
	(   forall((triple_remove(Pattern,Remove), test_use_owl(Pattern)),
	        forall(member(owl(S,P,O),Remove),use_owl(S,P,O,removed))) -> true ; true),


        % temporary fix to make up for bug in rdf parsing
        % see email to JanW July-1-2009
        forall((test_use_owl(S,P,BNode),
                atom(BNode),
                sub_atom(BNode,0,2,_,'__'),
                test_use_owl(BNode,'http://www.w3.org/1999/02/22-rdf-syntax-ns#datatype',literal(_))),
               (   use_owl(S,P,BNode,datatype_fix),
                   use_owl(BNode,'http://www.w3.org/1999/02/22-rdf-syntax-ns#datatype',literal(_)),
                   expand_and_assert(S,P,literal('')))),

	% replace matched patterns (Table 6)
        debug(owl_parser,'Replacing patterns [see table 6]',[]),
	(   setof(ReplaceWith,
                  Pattern^(   triple_replace(Pattern,ReplaceWith), % +Triples:list, ?Triples:list
                              use_owl(Pattern),
                              debug(owl_parser,'Replacing ~w ==> ~w [see table 6]',[Pattern,ReplaceWith])),
                  ReplacementSetList)
        ->  forall((member(ReplacementSet,ReplacementSetList),member(owl(S,P,O),ReplacementSet)),
                   expand_and_assert(S,P,O))
        ;   debug(owl_parser,'No replacements required',[])),

        /*
	forall(triple_replace(Pattern,ReplaceWith),
               forall(use_owl(Pattern),
                      forall(member(owl(S,P,O),ReplaceWith),
                             (   expand_and_assert(S,P,O),
                                 debug(owl_parser,'Replacing ~w ==> ~w [see table 6]',[Pattern,owl(S,P,O)]))))),
        */

	% continue with parsing using the rules...
	% Table 8, get the set of RIND - anonymous individuals in reification
	findall(X, (member(Y,['owl:Axiom','owl:Annotation',
			      'owl:AllDisjointClasses','owl:AllDisljointProperties',
			      'owl:AllDifferent','owl:NegativePropertyAssertion']),
                    test_use_owl(X,'rdf:type',Y)
                   ),
                RIND),
	nb_setval(rind,RIND),

        % Table 9, row 5
	% VV 10/3/2010 get the annotation properties before collecting the annotations.
        debug(owl_parser,'asserting annotationProperty/1 for all APs',[]),
	forall( test_use_owl(D,'rdf:type','owl:AnnotationProperty'),
		assert_axiom(annotationProperty(D))),

        % TODO - make this faster
        debug(owl_parser,'Implements function ANN(x) 3.2.2 Table 10.',[]),
	findall(_,ann(_,_),_), % find all annotations, assert annotation(X,AP,AV) axioms.

        debug(owl_parser,'Commencing parse of annotated axioms',[]),
        forall((axiompred(PredSpec),\+dothislater(PredSpec),\+omitthis(PredSpec)),
               owl_parse_annotated_axioms(PredSpec)),
        forall((axiompred(PredSpec),dothislater(PredSpec),\+omitthis(PredSpec)),
               owl_parse_annotated_axioms(PredSpec)),

        debug(owl_parser_detail,'Commencing parse of unannotated axioms',[]),
	forall((axiompred(PredSpec),\+dothislater(PredSpec),\+omitthis(PredSpec)),
               owl_parse_nonannotated_axioms(PredSpec)),
        forall((axiompred(PredSpec),dothislater(PredSpec),\+omitthis(PredSpec)),
               owl_parse_nonannotated_axioms(PredSpec)),!,
	% annotation Assertion
	parse_annotation_assertions,
	forall(owl_parse_compatibility_DL(Axiom),assert_axiom(Axiom)),
	owl_canonical_parse_3(Rest).

rdf_db_to_owl :-
	owl2_model_init,
        findall(BaseURI,
                (   rdf(Ont,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/2002/07/owl#Ontology',BaseURI:_),
                    rdf_2_owl(BaseURI,Ont),
                    owl_canonical_parse_3(IRIs)),
                IRIs).

%% translate_rdf_db(+IRI)
% translates a graph in current rdf_db instance into an owl2_model.pl set of facts.
% assumes that IRI has already been loaded using the semweb package
translate_rdf_db(BaseURI) :-
        rdf(Ont,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/2002/07/owl#Ontology',BaseURI:_),
        !,
        rdf_2_owl(BaseURI,Ont),
        owl2_model_init,
        owl_canonical_parse_3(BaseURI).


omitthis(ontology/1).


owl_parse_annotated_axioms(Pred/Arity) :-
        debug(owl_parser_detail,'[ann] Parsing all of type: ~w',[Pred]),
        functor(Head,Pred,Arity),
%        forall(owl_parse_axiom(Mod:Head),
%               (   debug(owl_parser_detail,' parsed: [~w] ~w',[Mod,Head]),
%                   assert(Mod:Head))).
	forall(owl_parse_axiom(Head,true,Annotations),
	       (   assert_axiom(Head),
                   debug(owl_parser_detail_anns,' parsed: ~w : anns: ~w',[Head,Annotations]),
		   forall(member(X,Annotations),
			  forall(aNN(X,AP,AV),
				 assert_axiom(annotation(Head,AP,AV)))
			 )
	       )
	      ),
        debug(owl_parser_detail,'[ann] Done parsing all of type: ~w',[Pred]).

owl_parse_nonannotated_axioms(Pred/Arity) :-
        debug(owl_parser_detail,'[unann] Parsing all of type: ~w',[Pred]),
        functor(Head,Pred,Arity),
	forall(owl_parse_axiom(Head,false,_),
	       assert_axiom(Head)
	      ).



%%       rdf_load_stream(+URL, -Ontology, -BaseURI, -Imports:list) is det
%
%	This predicate calls the rdf parser to parse the RDF/XML URL
%	into RDF triples. URL can be a local file or a URL.
%	The predicate returns all Imports based on the 	owl:imports predicate.
%	Also the Ontology of the URL if an owl:Ontology exists, var
%	otherise.
%
%       If owl_repository/2 is defined, then this is used to map URLs
%       prior to loading.


rdf_load_stream(URL,Ontology,BaseURI,Imports) :-
        owl_repository(URL,RURL),
        !,
        % note: users responsibility to avoid infinite loops by avoid cycles in repository mappings!
        rdf_load_stream(RURL,Ontology,BaseURI,Imports).

rdf_load_stream(URL,Ontology,BaseURI,Imports) :-
	BaseURI = URL,
  	(   sub_atom(URL,0,4,_,'http')
        ->  catch((http_open(URL,RDF_Stream,[]),
	      rdf_load(RDF_Stream,[if(true),base_uri(BaseURI),blank_nodes(noshare),
				   result(Action, Triples, MD5),register_namespaces(true)]),
		   debug(owl_parser,' Loaded ~w stream: ~w Action: ~w Triples:~w MD5: ~w',[URL,RDF_Stream,Action,Triples,MD5]),
                   close(RDF_Stream)),
                  Message,
                  throw(io_error(URL,'rdf_load/2 failed',Message))) % re-throw with more information
        ;  RDF_Stream = URL, rdf_load(RDF_Stream,[blank_nodes(noshare),if(true),base_uri(BaseURI),register_namespaces(true)])
	),
        % collect all imports directives
	(   rdf(Ontology,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.w3.org/2002/07/owl#Ontology',BaseURI:_)
        ->  findall(I,rdf(Ontology,'http://www.w3.org/2002/07/owl#imports',I,BaseURI:_),Imports)
	;   Imports = []
	).



% ----------------------------------------------------------------
% 3 Mapping from RDF Graphs to the Structural Specification
% ----------------------------------------------------------------

/*

  This section specifies the results of steps CP-2.2 and CP-3.3 of the
  canonical parsing process from Section 3.6 of the OWL 2
  Specification [OWL 2 Specification] on an ontology document D that
  can be parsed into an RDF graph G. ...

  */

%       owl_description_list(+Node, -List)
%
%       If +Node is defined as rdf:type rdf:List, then List returns
%       a prolog list of descriptions for this Node.

owl_description_list('http://www.w3.org/1999/02/22-rdf-syntax-ns#nil',[]) :- !.

owl_description_list(X,[F|R]) :-
	% use_owl(X,'rdf:type','rdf:List',list), % this is now removed from graph
	use_owl(X,'rdf:first',Element,first),
	owl_description(Element,F),
	use_owl(X,'rdf:rest',Y,rest),
	!,owl_description_list(Y,R).


%       owl_individual_list(+Node, -List)
%
%       If +Node is defined as rdf:type rdf:List, then List returns
%       a prolog list of individuals for this Node.

owl_individual_list('http://www.w3.org/1999/02/22-rdf-syntax-ns#nil',[]) :- !.

owl_individual_list(X,[F|R]) :-
	% use_owl(X,'rdf:type','rdf:List',list), % this is now removed from graph
	use_owl(X,'rdf:first',F,first),
	use_owl(X,'rdf:rest',Y,rest),
	!,owl_individual_list(Y,R).

%       owl_property_list(+Node, -List)
%
%       If +Node is defined as rdf:type rdf:List, then List returns
%       a prolog list of properties for this Node.

owl_property_list('http://www.w3.org/1999/02/22-rdf-syntax-ns#nil',[]) :- !.

owl_property_list(X,[F|R]) :-
	% use_owl(X,'rdf:type','rdf:List',list), % this is now removed from graph
	use_owl(X,'rdf:first',Element,first),
	owl_property_expression(Element,F),
	use_owl(X,'rdf:rest',Y,rest),
	!,owl_property_list(Y,R).

%       owl_datarange_list(+Node, -List)
%
%       If +Node is defined as rdf:type rdf:List, then List returns
%       a prolog list of dataranges for this Node.

owl_datarange_list('http://www.w3.org/1999/02/22-rdf-syntax-ns#nil',[]) :- !.

owl_datarange_list(X,[F|R]) :-
	% use_owl(X,'rdf:type','rdf:List',list), % this is now removed from graph
	use_owl(X,'rdf:first',Element,first),
	owl_datarange(Element,F),
	use_owl(X,'rdf:rest',Y,rest),
	!,owl_datarange_list(Y,R).

%       owl_datatype_restriction_list(+Node, -List)
%
%       If +Node is defined as rdf:type rdf:List, then List returns
%       a prolog list of datatype restrictions for this Node.

owl_datatype_restriction_list('http://www.w3.org/1999/02/22-rdf-syntax-ns#nil',[]) :- !.

owl_datatype_restriction_list(X,[facetRestriction(W2,L)|R]) :-
	% use_owl(X,'rdf:type','rdf:List'), % this is now removed from graph
	use_owl(X,'rdf:first',Element,first_datatype_restr),
	use_owl(Element,W,L,datatype_restr),
	(   concat_atom([_,W2],'#',W)
	->  true
	;   W2=W),
	use_owl(X,'rdf:rest',Y,rest_datatype_restr),
	!,owl_datatype_restriction_list(Y,R).


% 3.1 Extracting Declarations and the IRIs of the Directly Imported Ontology Documents
% This section specifies the result of step CP-2.2 of the canonical parsing process on an RDF graph G


% 3.1.2 Parsing of the Ontology Header and Declarations

%  Table 4.
owl_parse_axiom(ontology(O),AnnMode,List) :-
        test_use_owl(O,'rdf:type','owl:Ontology'),
	\+ test_use_owl([owl(U,_W,O),owl(U,'rdf:type','owl:Ontology')]),
	valid_axiom_annotation_mode(AnnMode,O,'rdf:type','owl:Ontology',List),
        use_owl(O,'rdf:type','owl:Ontology',ontology),
        nb_setval(current_ontology,O),
	forall(use_owl(O,'owl:imports',IRI,ontology_import), assert_axiom(ontologyImport(O,IRI))),
	forall(use_owl(O,'owl:versionInfo',IRI2,ontology_version_info), assert_axiom(ontologyVersionInfo(O,IRI2))),!. % Do Once


% See table 5.
% triple_remove(Pattern:list,Remove:list)
% if Pattern is present, remove triples in Remove
triple_remove([owl(X,'rdf:type','owl:Ontology')],[owl(X,'rdf:type','owl:Ontology')]).
triple_remove([owl(X,'rdf:type','owl:Class'),owl(X,'rdf:type','rdfs:Class')],[owl(X,'rdf:type','rdfs:Class')]).
triple_remove([owl(X,'rdf:type','rdfs:Datatype'),owl(X,'rdf:type','rdfs:Class')],[owl(X,'rdf:type','rdfs:Class')]).
triple_remove([owl(X,'rdf:type','owl:DataRange'),owl(X,'rdf:type','rdfs:Class')],[owl(X,'rdf:type','rdfs:Class')]).
triple_remove([owl(X,'rdf:type','owl:Restriction'),owl(X,'rdf:type','rdfs:Class')],[owl(X,'rdf:type','rdfs:Class')]).
triple_remove([owl(X,'rdf:type','owl:Restriction'),owl(X,'rdf:type','owl:Class')],[owl(X,'rdf:type','owl:Class')]).
triple_remove([owl(X,'rdf:type','owl:ObjectProperty'),owl(X,'rdf:type','rdf:Property')],[owl(X,'rdf:type','rdf:Property')]).
triple_remove([owl(X,'rdf:type','owl:FunctionalProperty'),owl(X,'rdf:type','rdf:Property')],[owl(X,'rdf:type','rdf:Property')]).
triple_remove([owl(X,'rdf:type','owl:InverseFunctionalProperty'),owl(X,'rdf:type','rdf:Property')],[owl(X,'rdf:type','rdf:Property')]).
triple_remove([owl(X,'rdf:type','owl:TransitiveProperty'),owl(X,'rdf:type','rdf:Property')],[owl(X,'rdf:type','rdf:Property')]).
triple_remove([owl(X,'rdf:type','owl:DatatypeProperty'),owl(X,'rdf:type','rdf:Property')],[owl(X,'rdf:type','rdf:Property')]).
triple_remove([owl(X,'rdf:type','owl:AnnotationProperty'),owl(X,'rdf:type','rdf:Property')],[owl(X,'rdf:type','rdf:Property')]).
triple_remove([owl(X,'rdf:type','owl:OntologyProperty'),owl(X,'rdf:type','rdf:Property')],[owl(X,'rdf:type','rdf:Property')]).
triple_remove([owl(X,'rdf:type','rdf:List'),owl(X,'rdf:first',_Y),owl(X,'rdf:rest',_Z)],[owl(X,'rdf:type','rdf:List')]).
/*
   triple_remove([owl(X,'rdf:type','owl:Thing')],[owl(X,'rdf:type','owl:Thing')]).
*/
% See table 6.
% http://www.w3.org/TR/2008/WD-owl2-mapping-to-rdf-20081202/
triple_replace([owl(X,'rdf:type','owl:OntologyProperty')],[owl(X,'rdf:type','owl:AnnotationProperty')]).
triple_replace([owl(X,'rdf:type','owl:InverseFunctionalProperty')],[owl(X,'rdf:type','owl:ObjectProperty'),owl(X,'rdf:type','owl:InverseFunctionalProperty')]).
triple_replace([owl(X,'rdf:type','owl:TransitiveProperty')],[owl(X,'rdf:type','owl:ObjectProperty'),owl(X,'rdf:type','owl:TransitiveProperty')]).
triple_replace([owl(X,'rdf:type','owl:SymmetricProperty')],[owl(X,'rdf:type','owl:ObjectProperty'),owl(X,'rdf:type','owl:SymmetricProperty')]).

% NOTE: this is not specified in table 6. However, we treat rdfs:Classes as equivalent to owl:Classes
triple_replace([owl(X,'rdf:type','rdfs:Class')],[owl(X,'rdf:type','owl:Class')]).

% DECLARATIONS
%
% See table 7.
% http://www.w3.org/TR/2008/WD-owl2-mapping-to-rdf-20081202/

%% owl_parse_axiom(+AxiomSpec,+AnnMode:boolean,?AnnList:list) is det
%
% None
%
owl_parse_axiom(class(C),AnnMode,List) :-
	test_use_owl(C,'rdf:type','owl:Class'),
	valid_axiom_annotation_mode(AnnMode,C,'rdf:type','owl:Class',List),
        (   use_owl(C,'rdf:type','owl:Class',named,class(C)) -> true ; use_owl(C,'rdf:type','rdfs:Class',named,class(C))),
	not(class(C)).


owl_parse_axiom(datatype(D), AnnMode, List) :-
        test_use_owl(D,'rdf:type','rdf:Datatype'),
        valid_axiom_annotation_mode(AnnMode,D,'rdf:type','rdf:Datatype',List),
        use_owl(D,'rdf:type','rdf:Datatype',datatype(D)).


owl_parse_axiom(objectProperty(D), AnnMode, List) :-
        test_use_owl(D,'rdf:type','owl:ObjectProperty'),
        valid_axiom_annotation_mode(AnnMode,D,'rdf:type','owl:ObjectProperty',List),
        use_owl(D,'rdf:type','owl:ObjectProperty',objectProperty(D)),
	not(objectProperty(D)).


% note the difference in names between syntax and rdf
owl_parse_axiom(dataProperty(D), AnnMode, List) :-
        test_use_owl(D,'rdf:type','owl:DatatypeProperty'),
        valid_axiom_annotation_mode(AnnMode,D,'rdf:type','rdf:DatatypeProperty',List),
        use_owl(D,'rdf:type','owl:DatatypeProperty',dataProperty(D)),
	not(dataProperty(D)).

owl_parse_axiom(annotationProperty(D), AnnMode, List) :-
        test_use_owl(D,'rdf:type','owl:AnnotationProperty'),
        valid_axiom_annotation_mode(AnnMode,D,'rdf:type','rdf:AnnotationProperty',List),
        use_owl(D,'rdf:type','owl:AnnotationProperty',annotationProperty(D)),
	not(annotationProperty(D)).


% TODO: check this. do we need to assert individual axioms if all we have is an rdf:type?
owl_parse_axiom(namedIndividual(D), AnnMode, List) :-
        test_use_owl(D,'rdf:type','owl:NamedIndividual'),
        valid_axiom_annotation_mode(AnnMode,D,'rdf:type','rdf:NamedIndividual',List),
        use_owl(D,'rdf:type','owl:NamedIndividual',namedIndividual(D)).


% Table 8. Identifying Anonymous Individuals in Reification
% TODO


% 3.2 Populating an Ontology


% 3.2.1 Analyzing Declarations

% 3.2.2 Parsing of Annotations

%
%       ann(?X, -Extension List)
%
%       Implements function ANN(x) 3.2.2 Table 10
%
%     The annotations in G are parsed next. The function ANN assigns a
%     set of annotations ANN(x) to each IRI or blank node x. This
%     function is initialized by setting ANN(x) = ∅ for each each IRI
%     or blank node x. Next, the triple patterns from Table 10 are
%     matched in G and, for each matched pattern, ANN(x) is extended
%     with an annotation from the right column. Each time one of these
%     triple patterns is matched, the matched triples are removed from
%     G. This process is repeated until no further matches are
%     possible

ann(X,Y) :-
	ann(X,X,Y).



ann(X,X1, annotation(X1,Y,Z)) :-
	annotationProperty(Y),
        debug(owl_parser_detail,'annotation property: ~w',[Y]),
        owl(X,Y,Z,not_used),
        use_owl(X,Y,Z,annotationProperty(Y)),
	u_assert(aNN(X1,Y,Z)),
	ann2(X,Y,Z,X1).


ann2(X,Y,Z,X1) :-
	annotation_r_node(X,Y,Z,W),
	ann(W,annotation(X1,Y,Z),Term),
        u_assert(Term).

ann2(X,Y,Z,X1) :-
	axiom_r_node(X,Y,Z,W),
	ann(W,annotation(X1,Y,Z),Term),
        u_assert(Term).


ann2(_,_,_,_).


% 3.2.4 Parsing of Expressions

is_bnode(C) :-
	atom(C),
	sub_atom(C,0,2,_,'__').


	% Table 11. Parsing Object Property Expressions
owl_property_expression(C,C) :-
	not(is_bnode(C)), % better: IRI(C).
	% VV added 10/3/2011
	not(C='http://www.w3.org/1999/02/22-rdf-syntax-ns#first'),
	not(C='http://www.w3.org/1999/02/22-rdf-syntax-ns#rest'),
        !.

owl_property_expression(C,D) :-
	blanknode(C,D,Use),
	(   Use = used,
	    retractall(blanknode(C,D,used)),
	    assert(blanknode(C,D,shared))
	;
	    true).

owl_property_expression(P,inverseOf(Q)) :-
        use_owl(P,'owl:inverseOf',Q,inverseof(P,Q)),
        owl_get_bnode(P,inverseOf(Q)).


% Table 12. Parsing of Data Ranges

owl_datarange(D,D) :-
	not(is_bnode(D)),!.  % better: IRI(C).

owl_datarange(C,D) :-
	blanknode(C,D,Use),
	(   Use = used,
	    retractall(blanknode(C,D,used)),
	    assert(blanknode(C,D,shared))
	;
	true).

owl_datarange(D,intersectionOf(L)) :-
	use_owl(D,'rdf:type','rdfs:Datatype',datarange(D)),
	use_owl(D,'owl:intersectionOf',Y,datarange(D)),
	%print(D-inter-Y),nl,
        owl_datarange_list(Y,L),
	owl_get_bnode(D,intersectionOf(L)).

owl_datarange(D,unionOf(L)) :-
	use_owl(D,'rdf:type','rdfs:Datatype',datarange(D)),
	use_owl(D,'owl:unionOf',Y,datarange(D)),
        owl_datarange_list(Y,L),
	owl_get_bnode(D,unionOf(L)).


owl_datarange(D,complementOf(DY)) :-
	use_owl(D,'rdf:type','rdfs:Datatype',dataRange(D)),
	use_owl(D,'owl:datatypeComplementOf',Y,datacomplement(D)),
        owl_datarange(Y,DY),
	owl_get_bnode(D,complementOf(DY)).

% Table 14, case 2
 owl_datarange(D,complementOf('rdfs:Literal')) :-
	use_owl(D,'rdf:type','rdfs:DataRange',dataRange(D)),
	use_owl(D,'owl:oneOf',[],oneOf(D)),
	owl_get_bnode(D,complementOf('rdfs:Literal')).

owl_datarange(D,oneOf(L)) :-
	use_owl(D,'rdf:type','rdfs:Datatype',dataType(D)),
	use_owl(D,'owl:oneOf',L1,oneOf(D)),
	owl_individual_list(L1,L),
	owl_get_bnode(D,oneOf(L)).

% Table 14, case 1
owl_datarange(D,oneOf(L)) :-
	use_owl(D,'rdf:type','rdfs:DataRange',datarange(D)),
	use_owl(D,'owl:oneOf',L1,datarange(D)),
	owl_individual_list(L1,L),
	owl_get_bnode(D,oneOf(L)).


owl_datarange(D,datatypeRestriction(DY,L)) :-
	use_owl(D,'rdf:type','rdfs:Datatype',datarange(D)),
	use_owl(D,'owl:onDatatype',Y,datarange(D)),
	owl_datarange(Y,DY),
	use_owl(D,'owl:withRestrictions',L1,datarange(D)),
	owl_datatype_restriction_list(L1,L),
	owl_get_bnode(D,datatypeRestriction(DY,L)).

% Table 13. Parsing of Class Expressions

% ----------------------------------------------------------------------
%       owl_description(+Node,-Description).
%
%	It implements OWL AS production rules for Descriptions.
%         During the construction of the Description any blank node
%         is recorded for later structure sharing checks.

owl_description(C,C) :-
	not(is_bnode(C)),!. % better: IRI(C).


owl_description(C,D) :-
	blanknode(C,D,Use),
	(   Use = used,
	    retractall(blanknode(C,D,used)),
	    assert(blanknode(C,D,shared))
	;
	    true),!.

% TODO: this leaves behind classAssertions of type owlClass for the bnodes
owl_description(D,intersectionOf(L)) :-
	use_owl(D,'owl:intersectionOf',L1,intersectionOf(D)),
	owl_description_list(L1,L),
	\+L = [],
	owl_get_bnode(D,intersectionOf(L)),!.

owl_description(D,unionOf(L)) :-
	use_owl(D,'owl:unionOf',L1,union(D)),
	owl_description_list(L1,L),
	owl_get_bnode(D,unionOf(L)),!.


owl_description(D,complementOf(Descr)) :-
	use_owl(D,'owl:complementOf',D1,complementOf(D)),
	owl_description(D1,Descr),
	owl_get_bnode(D,complementOf(Descr)),!.

owl_description(D,oneOf(L)) :-
	use_owl(D,'owl:oneOf',L1,oneOf(D)),
	(   use_owl(D,'rdf:type','owl:Class',oneOf(D,L)) ; true),
	owl_individual_list(L1,L),
	owl_get_bnode(D,oneOf(L)),!.

owl_description(D,datatypeRestriction(DY,L)) :-
	use_owl(D,'rdf:type','rdfs:Datatype',datatypeRestr(D)),
	use_owl(D,'owl:onDatatype',Y,dataType(D)),
	owl_datarange(Y,DY),
	use_owl(D,'owl:withRestrictions',L1,withRestrictions(D)),
	owl_datatype_restriction_list(L1,L),
	owl_get_bnode(D,datatypeRestriction(DY,L)).

owl_description(D,Restriction) :-
	owl_restriction(D, Restriction),
	owl_get_bnode(D,Restriction),!.


% Table 15 - OWL DL compatibility class expressions
%
owl_description(D,Result) :-
	not(is_bnode(D)), % better: IRI(C).
	use_owl(D,'rdf:type','owl:Class',description(D)),
	use_owl(D,'owl:unionOf',L,unionOf(L)),
	owl_description_list(L,DL),
	(   DL = [], Result = 'owl:Nothing' ;
	    DL = [D1], Result = D1),
	owl_get_bnode(D,Result),!.

owl_description(D,Result) :-
	not(is_bnode(D)), % better: IRI(C).
	use_owl(D,'rdf:type','owl:Class',dl_compatibility_descr(D)),
	use_owl(D,'owl:intersectionOf',L,intersectionOf(D)),
	owl_description_list(L,DL),
	(   DL = [], Result = 'owl:Thing' ;
	    DL = [D1], Result = D1),
	owl_get_bnode(D,Result),!.

owl_description(D,Result) :-
	not(is_bnode(D)),!, % better: IRI(C).
	use_owl(D,'rdf:type','owl:Class',dl_compatibility_descr(D)),
	use_owl(D,'owl:oneOf',[],oneOf(D)),
	Result = 'owl:Nothing',
	owl_get_bnode(D,Result).

% support older deprecated versions of OWL2 spec. See for example hydrology.owl
onClass(E,D) :- use_owl(E,'http://www.w3.org/2006/12/owl2#onClass',D,onClass(E)).
onClass(E,D) :- use_owl(E,'owl:onClass',D,onClass(E)).

onDataRange(E,D) :- use_owl(E, 'owl:onDataRange',D,onDatarange(E)).


%       owl_restriction(+Element,-Restriction).
%
%       If Element is defined as a owl:Restriction on property P then
%       Restriction binds to a restriction(Property,Type) term,
%	according to OWL Abstract syntax specification.

owl_restriction(Element,Restriction) :-
	use_owl(Element,'rdf:type','owl:Restriction',restriction(Element)),
	(   use_owl(Element, 'owl:onProperty',PropertyID,onProperty(Element,PropertyID)) ;
    	    use_owl(Element, 'owl:onProperties',PropertyID,onProperties(Element,PropertyID))
	),
	owl_restriction_type(Element,PropertyID, Restriction),
        debug(owl_parser_detail,'Restriction: ~w',[Restriction]).



owl_restriction_type(E, P, someValuesFrom(PX, DX)) :-
	use_owl(E, 'owl:someValuesFrom',D,someValuesFrom(E,P)),
	(   owl_description(D, DX) ; owl_datarange(D,DX)),
        (   P = [_|_], owl_property_list(P,PX) ;  owl_property_expression(P, PX)).


owl_restriction_type(E, P, allValuesFrom(PX,DX)) :-
	use_owl(E, 'owl:allValuesFrom',D,allValuesFrom(E,P)),
	(   owl_description(D, DX) ; owl_datarange(D,DX)),
        (   P = [_|_], owl_property_list(P,PX) ;  owl_property_expression(P, PX)).


% changed from thea value-->hasValue
owl_restriction_type(E, P, hasValue(PX,Value)) :-
	use_owl(E, 'owl:hasValue',Value,hasValue(E)),
        owl_property_expression(P, PX).

% VV:check if RDF parser returns a triple with O=true for
% "true"^^xsd:boolean
owl_restriction_type(E, P, hasSelf(PX)) :-
	use_owl(E, 'owl:hasSelf', true,hasSelf(E)),
        owl_property_expression(P, PX).

% Support of deprecated translations:
% in the OWL2 RDF mapping, unqualified CRs use owl:{min,max}Cardinality
% and QCQs use owl:{min,ax}QualifiedCardinality
%
% however, there appear to be some ontologies; e.g. Hydrology.owl.
% that use an older mapping, where the same properties are used
% for QCR and unqCR
%
% it is relatively easy to support this legacy ontologies; however
% we must process these BEFORE unqualified cardinality restrictions.

owl_restriction_type(E, P, exactCardinality(N,PX,DX)) :-
	test_use_owl(E, 'owl:cardinality',Lit),
        onClass(E,D),
	owl_description(D, DX),!,
	use_owl(E, 'owl:cardinality',Lit,cardinality(E)),
        literal_integer(Lit,N),
        owl_property_expression(P, PX).

owl_restriction_type(E, P, minCardinality(N,PX,DX)) :-
	test_use_owl(E, 'owl:minCardinality',Lit),
        (   onClass(E,D),owl_description(D, DX)
        ;   onDataRange(E,D), owl_datarange(D,DX)),
	!,
        % we are sure this is an old-style unqualified CR - now consume triples
	use_owl(E, 'owl:minCardinality',Lit,minCardinality(E)),
        literal_integer(Lit,N),
        owl_property_expression(P, PX).

owl_restriction_type(E, P, maxCardinality(N,PX,DX)) :-
	test_use_owl(E, 'owl:maxCardinality',Lit),
        (   onClass(E,D),owl_description(D, DX)
        ;   onDataRange(E,D), owl_datarange(D,DX)),
	!,
        % we are sure this is an old-style unqualified CR - now consume triples
	use_owl(E, 'owl:maxCardinality',Lit,maxCard(E)),
        literal_integer(Lit,N),
        owl_property_expression(P, PX).

% END OF Support of deprecated translations:

% the following are all in the spec:

% changed from Thea1->2: cardinality->exactCardinality
owl_restriction_type(E, P,exactCardinality(N,PX)) :-
	use_owl(E, 'owl:cardinality',Lit,cardinality(E)),
        literal_integer(Lit,N),
        owl_property_expression(P, PX).

owl_restriction_type(E, P,exactCardinality(N,PX,DX)) :-
	use_owl(E, 'owl:qualifiedCardinality',Lit),literal_integer(Lit,N),
	(   onClass(E,D),owl_description(D, DX) ;
	    onDataRange(E,D), owl_datarange(D,DX)
	),
        owl_property_expression(P, PX).


owl_restriction_type(E, P, minCardinality(N,PX)) :-
	use_owl(E, 'owl:minCardinality',Lit,cardinality(E)),literal_integer(Lit,N),
        owl_property_expression(P, PX).

owl_restriction_type(E, P, minCardinality(N,PX,DX)) :-
	use_owl(E, 'owl:minQualifiedCardinality',Lit,cardinality(E)),literal_integer(Lit,N),
	(   onClass(E,D),owl_description(D, DX);
	    onDataRange(E,D), owl_datarange(D,DX)
	),
        owl_property_expression(P, PX).


owl_restriction_type(E, P, maxCardinality(N,PX)) :-
	use_owl(E, 'owl:maxCardinality',Lit,maxCardinality(E)),literal_integer(Lit,N),
        owl_property_expression(P, PX).

owl_restriction_type(E, P, maxCardinality(N,PX,DX)) :-
	use_owl(E, 'owl:maxQualifiedCardinality',Lit,cardinality(E,Lit)),
	literal_integer(Lit,N),
	(   onClass(E,D),owl_description(D, DX);
	    onDataRange(E,D), owl_datarange(D,DX)),
        owl_property_expression(P, PX).


% Table 14. Parsing of Data Ranges for Compatibility with OWL DL
% Included into owl_datarange clauses above

% Table 15. Parsing of Class Expressions for Compatibility with OWL DL
% Included into owl_dexcription clauses above

% Table 16. Parsing of Axioms without Annotations
% Declarations handled previously
% CLASS AXIOMS
% valid_axiom_annotation_mode: add clauses for the disjoint etc ....

collect_r_nodes :-
	retractall(axiom_r_node(_,_,_,_)),
	forall(( test_use_owl(Node,'rdf:type','owl:Axiom'),
		 test_use_owl(Node,'owl:annotatedSource',S),
		 test_use_owl(Node,'owl:annotatedProperty',P),
		 test_use_owl(Node,'owl:annotatedTarget',O)),
	       (assert(axiom_r_node(S,P,O,Node)),
                debug(owl_parser_detail,'~w',[axiom_r_node(S,P,O,Node)]),
		use_owl([owl(Node,'rdf:type','owl:Axiom'),
			 owl(Node,'owl:annotatedSource',S),
			 owl(Node,'owl:annotatedProperty',P),
			 owl(Node,'owl:annotatedTarget',O)]))),

	retractall(annotation_r_node(_,_,_,_)),
	forall(( test_use_owl(W,'rdf:type','owl:Annotation'),
		 test_use_owl(W,'owl:annotatedSource',S),
		 test_use_owl(W,'owl:annotatedProperty',P),
		 test_use_owl(W,'owl:annotatedTarget',O)),
	       (assert(annotation_r_node(S,P,O,Node)),
                debug(owl_parser_detail,'~w',[annotation_r_node(S,P,O,Node)]),
		use_owl([owl(W,'rdf:type','owl:Annotation'),
			 owl(W,'owl:annotatedSource',S),
			 owl(W,'owl:annotatedProperty',P),
			 owl(W,'owl:annotatedTarget',O)]))).

%% valid_axiom_annotation_mode(+AnnMode,+S,+P,+O,?AnnotationNodes:list) is det
% if AnnMode is true and annotation triples can be found then
% unify AnnotationNodes with the Nodes that annotate the triple,
% otherwise []

valid_axiom_annotation_mode(_Mode,S,P,O,List) :-
	findall(Node,axiom_r_node(S,P,O,Node),List).


owl_parse_axiom(subClassOf(DX,DY),AnnMode,List) :-
	test_use_owl(X,'rdfs:subClassOf',Y),
	valid_axiom_annotation_mode(AnnMode,X,'rdfs:subClassOf',Y,List),
	use_owl(X,'rdfs:subClassOf',Y,subclassOf(X,Y)),
        owl_description(X,DX),
	owl_description(Y,DY).

% Process each equivalentClass pair separately in order to capture
% annotations. Block the maximally connected subgraph.
% TODO. Process the equivalent(L) axioms to generate maximally connected
% equivalentClasses(L) axioms. (but without annotations?)

owl_parse_axiom(equivalentClasses(DL),AnnMode,List) :-
	test_use_owl(X,'owl:equivalentClass',Y),
	valid_axiom_annotation_mode(AnnMode,X,'owl:equivalentClass',Y,List),
	use_owl(X,'owl:equivalentClass',Y,equivalentClass(X,Y)),
        % maximally_connected_subgraph_over('owl:equivalentClass',L),
        maplist(owl_description,[X,Y],DL),
        debug(owl_parser_detail,'equivalentClasses Descs: ~w',[DL]).

% SUPPORT FOR IMPLICT equivalentClass axioms:
% Some OWL RDF encodings look like this:
% the preferred form is to use an equivalentClass axiom, but
% we should support this style too
% <owl:Class rdf:ID="WhiteWine">
%    <owl:intersectionOf rdf:parseType="Collection">
%      <owl:Class rdf:about="#Wine" />% Table 17. Parsing of Annotated Axioms
%      <owl:Restriction>
%        <owl:onProperty rdf:resource="#hasColor" />
%        <owl:hasValue rdf:resource="#White" />
%      </owl:Restriction>
%    </owl:intersectionOf>
%  </owl:Class>

owl_parse_axiom(equivalentClasses([C,intersectionOf(D)]),AnnMode,List) :-
	class(C),
	test_use_owl(C,'owl:intersectionOf',D1),
	debug(owl_parser,'equivalent collection; intersection for ~w',[C]),
	valid_axiom_annotation_mode(AnnMode,C,'owl:intersectionOf',D1,List),
	owl_description(C,intersectionOf(D)).

owl_parse_axiom(equivalentClasses([C,unionOf(D)]),AnnMode,List) :-
	class(C),
	test_use_owl(C,'owl:unionOf',D1),
	debug(owl_parser,'equivalent collection; union for ~w',[C]),
	valid_axiom_annotation_mode(AnnMode,C,'owl:unionOf',D1,List),
	owl_description(C,unionOf(D)).

owl_parse_axiom(equivalentClasses([C,oneOf(D)]),AnnMode,List) :-
	class(C),
	test_use_owl(C,'owl:oneOf',D1),
	debug(owl_parser,'equivalent collection; one of for ~w',[C]),
	valid_axiom_annotation_mode(AnnMode,C,'owl:oneOf',D1,List),
	owl_description(C,oneOf(D)).


owl_parse_axiom(equivalentClasses([C,D])) :-
        % TODO: this could be made more efficient by enforcing order of building
        (   test_use_owl(C,'rdf:type','owl:Class',named)
        ;   test_use_owl(C,'rdf:type','rdfs:Class',named)
        ;   class(C)),
        owl_description(C,D),
        C\=D.

% TODO. Process the disjointClasses(L) axioms to generate
% larger set of disjoint: ie if N classes are pairwise DisJoint
% then we can assert a disjointClasses for all N

owl_parse_axiom(disjointClasses([DX,DY]),AnnMode,List) :-
	test_use_owl(X,'owl:disjointWith',Y),
	valid_axiom_annotation_mode(AnnMode,X,'owl:disjointWith',Y,List),
	use_owl(X,'owl:disjointWith',Y,disjointWith(X,Y)),
        owl_description(X,DX),
	owl_description(Y,DY).

% One of the cases where annotations are those of _x and we do not seek
% for further annotation axioms. Par. 3.2.5.
% Whatever the AnnNode, _x is returned (will be ignored if mode false

owl_parse_axiom(disjointClasses(L),_AnnMode,[X]) :-
        % TODO: X may be referred to in an annotation axiom??
	use_owl(X,'rdf:type','owl:AllDisjointClasses',allDisjointClasses(X)),
        use_owl(X,'owl:members',L1,members(L1)),
        owl_description_list(L1,L).


owl_parse_axiom(disjointUnion(DX,DY),AnnMode,List) :-
	test_use_owl(X,'owl:disjointUnionOf',Y),
	valid_axiom_annotation_mode(AnnMode,X,'owl:disjointUnionOf',Y,List),
	use_owl(X,'owl:disjointUnionOf',Y,disjointUnionOf(X,Y)),
        owl_description(X,DX),
        owl_description_list(Y,DY).


% PROPERTY AXIOMS


% introduces bnode
owl_parse_axiom(subPropertyOf(propertyChain(PL),QX),AnnMode,List) :-
	test_use_owl(Q,'owl:propertyChainAxiom',L1),
	valid_axiom_annotation_mode(AnnMode,Q,'owl:propertyChainAxiom',L1,List),
	use_owl(Q,'owl:propertyChainAxiom',L1,propertyChainAxiom(Q)),
	owl_property_list(L1,PL),
        owl_property_expression(Q,QX).

owl_parse_axiom(subPropertyOf(PX,QX),AnnMode,List) :-
	test_use_owl(P,'rdfs:subPropertyOf',Q),
	valid_axiom_annotation_mode(AnnMode,P,'rdfs:subPropertyOf',Q,List),
	use_owl(P,'rdfs:subPropertyOf',Q,subPropertyOf(P,Q)),
        owl_property_expression(P,PX),
        owl_property_expression(Q,QX).


% Process each equivalentProperty pair separately in order to capture
% annotations. Block the maximally connected subgraph.
% TODO. Process the equivalent(L) axioms to generate maximally connected
% equivalentProperties(L) axioms. (but without annotations?)

owl_parse_axiom(equivalentProperties(OPEL),AnnMode,List) :-
	test_use_owl(X,'owl:equivalentProperty',Y),
	valid_axiom_annotation_mode(AnnMode,X,'owl:equivalentProperty',Y,List),
	use_owl(X,'owl:equivalentProperty',Y,equivProperty(X,Y)),
	% maximally_connected_subgraph_over('owl:equivalentProperty',L),
	maplist(owl_property_expression,[X,Y],OPEL).


% TODO. Process the disjointProperties(L) axioms to generate
% larger set of disjoint: ie if N properties are pairwise DisJoint
% then we can assert a disjointClasses for all N

owl_parse_axiom(disjointProperties([DX,DY]),AnnMode,List) :-
	test_use_owl(X,'owl:propertyDisjointWith',Y),
	valid_axiom_annotation_mode(AnnMode,X,'owl:propertyDisjointWith',Y,List),
	use_owl(X,'owl:propertyDisjointWith',Y,propertyDisjointWith(X,Y)),
        owl_description(X,DX),
	owl_description(Y,DY).

% One more of the cases where annotations are those of _x and we do not
% seek for further annotation axioms. Par. 3.2.5. Whatever the AnnNode,
% _x is returned (will be ignored if mode false)

owl_parse_axiom(disjointProperties(L),_AnnMode,[X]) :-
        % TODO: X may be referred to in an annotation axiom??
	use_owl(X,'rdf:type','owl:AllDisjointProperties',allDisjointProps(X,L1)),
        use_owl(X,'owl:members',L1,members(L1)),
        L1 = [_,_|_],           % length >= 2
        owl_property_list(L1,L).


owl_parse_axiom(propertyDomain(PX,CX),AnnMode,List) :-
	test_use_owl(P,'rdfs:domain',C),
	valid_axiom_annotation_mode(AnnMode,P,'rdfs:domain',C,List),
        use_owl(P,'rdfs:domain',C,domain(P,C)),
	(   annotationProperty(P),CX = C ;
	    owl_property_expression(P,PX),
	    owl_description(C,CX)
	).

% We need to distinguish here between object and data property
% Currently we first test if the range is a class, this means OPE
% otherwise if it is a datarange it means a DPE.
% Ideally we should also check possible declarations of OPE or DPE.

owl_parse_axiom(propertyRange(PX,CX),AnnMode,List) :-
	test_use_owl(P,'rdfs:range',C),
	valid_axiom_annotation_mode(AnnMode,P,'rdfs:range',C,List),
        use_owl(P,'rdfs:range',C,range(P,C)),
	(   annotationProperty(P) -> PX = P, CX = C ;
	    owl_property_expression(P,PX),
            (   owl_description(C,CX) -> true ; owl_datarange(C,CX))
	).

owl_parse_axiom(inverseProperties(PX,QX),AnnMode,List) :-
	test_use_owl(P,'owl:inverseOf',Q),
	valid_axiom_annotation_mode(AnnMode,P,'owl:inverseOf',Q,List),
	use_owl(P,'owl:inverseOf',Q,inverseOf(P,Q)),
        owl_property_expression(P,PX),
        owl_property_expression(Q,QX).

owl_parse_axiom(functionalProperty(P),AnnMode,List) :-
	test_use_owl(P,'rdf:type','owl:FunctionalProperty'),
	valid_axiom_annotation_mode(AnnMode,P,'rdf:type','owl:FunctionalProperty',List),
        use_owl(P,'rdf:type','owl:FunctionalProperty',functionalProperty(P)).

owl_parse_axiom(inverseFunctionalProperty(P),AnnMode,List) :-
	test_use_owl(P,'rdf:type','owl:InverseFunctionalProperty'),
	valid_axiom_annotation_mode(AnnMode,P,'rdf:type','owl:InverseFunctionalProperty',List),
        use_owl(P,'rdf:type','owl:InverseFunctionalProperty',inverseFunctionalProperty(P)).

owl_parse_axiom(reflexiveProperty(P),AnnMode,List) :-
	test_use_owl(P,'rdf:type','owl:ReflexiveProperty'),
	valid_axiom_annotation_mode(AnnMode,P,'rdf:type','owl:ReflexiveProperty',List),
        use_owl(P,'rdf:type','owl:ReflexiveProperty',reflexiveProperty(P)).

owl_parse_axiom(irreflexiveProperty(P),AnnMode,List) :-
	test_use_owl(P,'rdf:type','owl:IrreflexiveProperty'),
	valid_axiom_annotation_mode(AnnMode,P,'rdf:type','owl:IrreflexiveProperty',List),
        use_owl(P,'rdf:type','owl:IrreflexiveProperty',irreflexiveProperty(P)).

owl_parse_axiom(symmetricProperty(P),AnnMode,List) :-
	test_use_owl(P,'rdf:type','owl:SymmetricProperty'),
	valid_axiom_annotation_mode(AnnMode,P,'rdf:type','owl:SymmetricProperty',List),
        use_owl(P,'rdf:type','owl:SymmetricProperty',symmetricProperty(P)).

owl_parse_axiom(asymmetricProperty(P),AnnMode,List) :-
	test_use_owl(P,'rdf:type','owl:AsymmetricProperty'),
	valid_axiom_annotation_mode(AnnMode,P,'rdf:type','owl:AsymmetricProperty',List),
        use_owl(P,'rdf:type','owl:AsymmetricProperty',assymetricProperty(P)).

owl_parse_axiom(transitiveProperty(P),AnnMode,List) :-
	test_use_owl(P,'rdf:type','owl:TransitiveProperty'),
	valid_axiom_annotation_mode(AnnMode,P,'rdf:type','owl:TransitiveProperty',List),
	use_owl(P,'rdf:type','owl:TransitiveProperty',transitiveProperty(P)).

owl_parse_axiom(hasKey(CX,L),AnnMode,List) :-
	test_use_owl(C,'owl:hasKey',L1),
	valid_axiom_annotation_mode(AnnMode,C,'owl:hasKey',L1,List),
	use_owl(C,'owl:hasKey',L1,hasKey(C)),
	owl_description(C,CX),
        L1 = [_,_|_],           % length >= 2
        owl_property_list(L1,L).

% INDIVIDUALS

owl_parse_axiom(sameIndividual([X,Y]),AnnMode,List) :-
	test_use_owl(X,'owl:sameAs',Y),
	valid_axiom_annotation_mode(AnnMode,X,'owl:sameAs',Y,List),
	use_owl(X,'owl:sameAs',Y,sameAs(X,Y)).

owl_parse_axiom(differentIndividuals([X,Y]),AnnMode,List) :-
	test_use_owl(X,'owl:differentFrom',Y),
	valid_axiom_annotation_mode(AnnMode,X,'owl:differentFrom',Y,List),
	use_owl(X,'owl:differentFrom',Y,differentFrom(X,Y)).

owl_parse_axiom(differentIndividuals(L),_AnnMode,[X]) :-
	use_owl(X,'rdf:type','owl:AllDifferent',allDifferent(L)),
	use_owl(X,'owl:distinctMembers',L1,distinctMembers(L)),
        owl_individual_list(L1,L).

owl_parse_axiom(differentIndividuals(L),_AnnMode,[X]) :-
	use_owl(X,'rdf:type','owl:AllDifferent',allDifferent(X)),
	use_owl(X,'owl:members',L1,members(L)),
        owl_individual_list(L1,L).

% make sure this is done before fetching classAssertion/2;
% -- the annotationAssertion matching clause should preceded the classAssertion/2 matching clause
owl_parse_axiom(annotationAssertion('owl:deprecated', X, true),AnnMode,List) :-
	test_use_owl(X, 'rdf:type', 'owl:DeprecatedClass'),
	valid_axiom_annotation_mode(AnnMode,X,'rdf:type','owl:DeprecatedClass',List),
	use_owl(X, 'rdf:type', 'owl:DeprecatedClass',deprecatedClass(X)).

% make sure this is done before fetching propertyAssertion/3
% this clause should precede it
owl_parse_axiom(annotationAssertion('owl:deprecated', X, true),AnnMode,List) :-
	test_use_owl(X, 'rdf:type', 'owl:DeprecatedProperty'),
	valid_axiom_annotation_mode(AnnMode,X,'rdf:type','owl:DeprecatedProperty',List),
	use_owl(X, 'rdf:type', 'owl:DeprecatedProperty',deprecatedProperty(X)).

% Table 17. Parsing of Annotated Axioms

dothislater(annotationAssertion/3).
% TODO - only on unnannotated pass?
%

owl_parse_axiom(annotationAssertion(P,A,B),AnnMode,List) :-
        annotationProperty(P),
        test_use_owl(A,P,B),         % B can be literal or individual
        valid_axiom_annotation_mode(AnnMode,A,P,B,List),
        use_owl(A,P,B,annotationProperty(P)).


dothislater(classAssertion/2).
owl_parse_axiom(classAssertion(CX,X),AnnMode,List) :-
	test_use_owl(X,'rdf:type',C),
        C\='http://www.w3.org/2002/07/owl#DeprecatedClass',
	% note: some ontologies may include a rdf:type with no
	%  explicit class declaration. See testfiles/test_undeclared.owl
	%class(C),
	valid_axiom_annotation_mode(AnnMode,X,'rdf:type',C,List),
	use_owl(X,'rdf:type',C,classAssertion(CX,X)),
        % I added this to avoid class assertions for bNodes. Perhaps a better
        % way is to simply consume the owl4/ triple at the time of translating
        % the description? --CJM
        C\='http://www.w3.org/2002/07/owl#Class',
        %
        C\='http://www.w3.org/1999/02/22-rdf-syntax-ns#Property',
        owl_description(C,CX).

dothislater(propertyAssertion/3).
owl_parse_axiom(propertyAssertion(PX,A,BX),AnnMode,List) :-
        test_use_owl(A,P,B), % B can be literal or individual
        P\='http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
	% note: some ontologies may include a triples with no
	%  explicit property declaration. See testfiles/test_undeclared.owl
	%property(P),
	valid_axiom_annotation_mode(AnnMode,A,P,B,List),
        \+ annotationProperty(P), % these triples should have been removed before, during ann parsing
	owl_property_expression(P,PX), % can also be inverse
	% next line added by VV 9/3/2011 for Jochem Liem to support ID-lists as PA objects
	(   owl_individual_list(B,BX) -> true ; BX = B),
        use_owl(A,P,B,propertyAssertion(PX,A,BX)).


owl_parse_axiom(negativePropertyAssertion(PX,A,B),_,X) :-
        use_owl(X,'rdf:type','owl:NegativePropertyAssertion',negPropertyAssertion(PX,A,B)),
        use_owl(X,'owl:sourceIndividual',A,negPropertyAssertion(PX,A,B)),
        use_owl(X,'owl:assertionProperty',P,negPropertyAssertion(PX,A,B)),
        use_owl(X,'owl:targetValue',B,negPropertyAssertion(PX,A,B)),
        owl_property_expression(P,PX).


% process hooks; SWRL etc
owl_parse_axiom(A,AnnMode,List) :-
        owl_parse_axiom_hook(A,AnnMode,List).

% Parsing annotationAssertions
%

parse_annotation_assertions :-
	( nb_current(rind,RIND) -> true ; RIND = []),!,
	forall((aNN(X,AP,AV),findall( aNN(annotation(X,AP,AV),AP1,AV1),
				      aNN(annotation(X,AP,AV),AP1,AV1),ANN), \+member(X,RIND)),
	       (   assert_axiom(annotationAssertion(AP,X,AV)),
		  %  VV 10/3/2010 keep annotation/3
		  % retract(annotation(X,AP,AV)),
		   forall(member(aNN(_,AP1,AV1),ANN),
			    assert_axiom(annotation(annotationAssertion(AP,X,AV),AP1,AV1))
			 )
	       )
	      ),
	% forall(aNN(X,Y,Z),assert(annotation(X,Y,Z))), VV remove 25/1/11
	% annotation/3 axioms created already during owl_parse_annotated_axioms/1
	retractall(aNN(_,_,_)).

% Table 18. Parsing of Axioms for Compatibility with OWL DL

owl_parse_compatibility_DL(equivalentClasses([CEX,complementOf(CEY)])) :-
	use_owl(X,'owl:complementOf',Y,eq_classes),
	owl_description(X,CEX),
	owl_description(Y,CEY).


owl_parse_compatibility_DL(equivalentClasses([CEX,CEY])) :-
	use_owl(X,'owl:unionOf',Y,eq_classes),
	owl_description(X,CEX),
	owl_description_list(Y,DL),
	(   DL = [] -> CEY = 'owl:Nothing' ; (DL=[CEY]->true;CEY = unionOf(DL))).

owl_parse_compatibility_DL(equivalentClasses([CEX,CEY])) :-
	use_owl(X,'owl:intersectionOf',Y,eq_classes),
	owl_description(X,CEX),
	owl_description_list(Y,DL),
	(   DL = [] -> CEY = 'owl:Thing' ; (DL=[CEY]->true;CEY = intersectionOf(DL))).

owl_parse_compatibility_DL(equivalentClasses([CEX,CEY])) :-
	use_owl(X,'owl:oneOf',Y,eq_classes),
	owl_description(X,CEX),
	owl_description_list(Y,DL),
	(   DL = [] -> CEY = 'owl:Nothing' ; CEY = oneOf(DL)).

% UTIL

%% maximally_connected_subgraph_over(+P,?ConnectedSets) is semidet
maximally_connected_subgraph_over(P,CSet):-
        maximally_connected_subgraph_over(P,[],CSetL),
        member(CSet,CSetL).

%% maximally_connected_subgraph_over(+P,+Used,?ListOfConnectedSets) is det
maximally_connected_subgraph_over(P,Used,[CSet|All]):-
        test_use_owl(X,P,Y), % seed
        \+ member(X,Used),
        \+ member(Y,Used),
        use_owl(X,P,Y,maximally_conected), % seed
        !,
        extend_set_over(P,[X,Y],CSet),
        append(CSet,Used,Used2),
        maximally_connected_subgraph_over(P,Used2,All).
maximally_connected_subgraph_over(_,_,[]).


% det
extend_set_over(P,L,L2):-
        member(X,L),
        test_use_owl(X,P,Y),
        \+ member(Y,L),
        use_owl(X,P,Y,extend_set_over),
        !,extend_set_over(P,[Y|L],L2).
extend_set_over(P,L,L2):-
        member(X,L),
        test_use_owl(Y,P,X),
        \+ member(Y,L),
        use_owl(Y,P,X,extend_set_over),
        !,extend_set_over(P,[Y|L],L2).
extend_set_over(_,L,L):- !.

literal_integer(literal(type,A),N) :- atom_number(A,N).
literal_integer(literal(type(_,A)),N) :- atom_number(A,N).

%% time_goal(+Goal,?Time)
%  calls Goal and unifies Time with the cputime taken
time_goal(Goal,Time):-
        statistics(cputime,T1), Goal,
        statistics(cputime,T2), Time is T2-T1.

timed_forall(Cond,Action) :-
        forall(Cond,
               (   time_goal(Action,Time),
                   debug(owl2_bench,'Goal: ~w Time:~w',[Action,Time]))).


/** <module> Translates an RDF database to OWL2 axioms
  ---+ Synopsis 1
==
:- use_module(bio(owl2_from_rdf)).
%
==
---+ Details
---++ Hooks
* owl_parse_axiom_hook/3
---+ See Also
The file owl2_from_rdf.plt has some examples
*/


































