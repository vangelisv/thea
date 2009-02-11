/* -*- Mode: Prolog -*- */
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
%
% **********************************************************************

:- module(owl2_from_rdf,
	  [ 
            owl_parse_rdf/1,
            owl_parse_rdf/2,
	    owl_parse/4,
            convert/3,
	    expand_ns/2,                  %  ?NS_URL, ?Full_URL
	    collapse_ns/4,
	   
	    blanknode/3,
	    blanknode_gen/2,

	    owl_parser_log/2
	  ]).

:- use_module(owl2_model).

:- use_module(library('semweb/rdf_db.pl')).
:- use_module(library('semweb/rdf_edit.pl')).
:- use_module(library('semweb/rdfs.pl')).
:- use_module(library('url.pl')).
:- use_module(library('http/http_open.pl')).

:- dynamic(owl/4).
:- dynamic(blanknode/3).
:- dynamic(owl_parser_log/2).
:- dynamic(blanknode_gen/2).
:- dynamic(outstream/1).

% we make this discontiguous so that the code can follow the structure of the document as much as possible
:- discontiguous owl_parse_axiom/1.


% -----------------------------------------------------------------------		    
%                                Top Level  Predicates
% -----------------------------------------------------------------------

owl_parse_rdf(F):-
	owl_parse_rdf(F,[]).
owl_parse_rdf(F,Opts):-
	(   member(imports(Imports),Opts)
	->  true
	;   Imports=false),
	(   member(clear(Clear),Opts)
	->  true
	;   Clear=false),
	owl_parse(F,Clear,Clear,Imports),
	debug(owl_parser,'parsed ~w',[F]).


%% owl_parse(+URL, +RDF_Load_Mode, +OWL_Parse_Mode, +Imports)
%
%  Top level predicate to parse a set of RDF triples and produce an
%  Abstract Syntax representation of an OWL ontology.
%		    
%	Calls the rdf_load_stream predicate to parse RDF stream in URL. 
%       If RDF_Load_Mode = complete it first retacts all rdf triples.
%       If Imports = true it handles owl:import clause at RDF level.
%
% owl_parse(+OWL_Parse_Mode).
            					       

owl_parse(URL, RDF_Load_Mode, OWL_Parse_Mode,Imports) :-
%	(   RDF_Load_Mode=complete,!,rdf_retractall(_,_,_); true),
	(   RDF_Load_Mode=complete -> rdf_retractall(_,_,_) ; true),
	retractall(rdf_db:rdf_source(_,_,_,_)),
        debug(owl_parser,'Loading stream ~w',[URL]),
	rdf_load_stream(URL,[URL],Imports),
	owl_parse_2(OWL_Parse_Mode).
	        
    
owl_parse_2(OWL_Parse_Mode) :-
        debug(owl_parser,'Commencing rdf_2_owl. Generating owl/4',[]),
	rdf_2_owl,
%	(   OWL_Parse_Mode=complete,!,owl_clear_as; true),
	(   OWL_Parse_Mode=complete -> owl_clear_as ; true),
        owl_parse_all_axioms(ontology/1),
        forall((axiompred(PredSpec),\+dothislater(PredSpec),\+omitthis(PredSpec)),
               owl_parse_all_axioms(PredSpec)),
        forall((axiompred(PredSpec),dothislater(PredSpec),\+omitthis(PredSpec)),
               owl_parse_all_axioms(PredSpec)).


omitthis(ontology/1).

:- discontiguous dothislater/1.


owl_parse_all_axioms(Pred/Arity) :-
        debug(owl_parser_detail,'Parsing all of type: ~w',[Pred]),
        functor(Head,Pred,Arity),
        forall(owl_parse_axiom(Head),
               assert(Head)).


% -----------------------------------------------------------------------		    
%                                UTILITY Predicates
% -----------------------------------------------------------------------

%%       owl_parser_log(+Log)
%
%       Log is a list; together with a timestamp it is asserted as
%       an owl_parser_log/2 term.

owl_parser_log(Log) :-
	debug(owl_parser,'~w',[Log]),
	get_time(T),convert_time(T,TS),
	assertz(owl_parser_log(TS, Log)).


%%       owl_clear_as.
%
%       Clears the prolog terms that store the Abstract Syntax
%       implementation of the OWL ontology.

owl_clear_as :- 
        debug(owl_parser,'Clearing abstract syntax',[]),
        forall((axiompred(PredSpec),predspec_head(PredSpec,Head)),
               retractall(Head)).

predspec_head(Pred/A,Head) :- functor(Head,Pred,A).

convert(T,V,typed_value(T,V)).     


%%	rdf_2_owl.     
%       
%       Converts RDF triples to OWL/4 triples so that
%	their use can tracked by the OWL parser.


rdf_2_owl :-
	owl_parser_log(['Removing existing owl triples']),
	retractall(owl(_,_,_,_)),
	owl_parser_log('Copying RDF triples to OWL triples'), 
	rdf(X,Y,Z), 
%	owl_fix_no(X,X1), owl_fix_no(Y,Y1), owl_fix_no(Z,Z1),
	assert(owl(X,Y,Z,not_used)), fail.

rdf_2_owl :-
	owl_count(Z),
	owl_parser_log(['Number of owl triples copied: ',Z]).


%%       rdf_load_stream(+URL, +ImportedList)
%	
%	This predicate calls the rdf parser to parse the RDF/XML URL
%	into RDF triples. URL can be a local file or a URL.
%	The predicate recursively calls itself for all URLs that need to 
%	be imported, ie. are objects to an owl:imports predicate. 
%	The ImportedList argument contains the imported so far URLs,
%	to avoid re-visiting the same URLs. (Empty List in 1st call).

rdf_load_stream(URL,Imported,Imports) :- 
  	(sub_string(URL,0,4,_,'http'), !,
	 http_open(URL,RDF_Stream,[]), 
         % rdf_load(RDF_Stream,[blank_nodes(noshare),convert_typed_literal(convert)]), 
	 rdf_load(RDF_Stream,[if(true),blank_nodes(noshare),result(Action, Triples, MD5)]),
         debug(owl_parser,' Loaded ~w stream: ~w Action: ~w Triples:~w MD5: ~w',[URL,RDF_Stream,Action,Triples,MD5]),
	 close(RDF_Stream) 
	 ;
	 RDF_Stream = URL, rdf_load(RDF_Stream,[blank_nodes(noshare)])
	 ),
	(   Imports = true,
	    rdf(_,'http://www.w3.org/2002/07/owl#imports',Import_URL),
	    not( member(Import_URL, Imported)),!,
	    debug(owl_parser,'Imports: ~w',[Import_URL]),
            rdf_load_stream(Import_URL,[Import_URL|Imported],Imports)
	  ; true).


%%	fix_no(+A,-B)  
%
%	This is used to correct an RDF parser error: 
%       To remove duplicate ## from a URL.

/*	Obsolete with version 5.5.x of SWI's RDF parser
owl_fix_no(literal(X), literal(X)) :- !.

owl_fix_no(A,B) :- 
	sub_atom(A,Start,_,After,'##'),
	sub_atom(A,0,Start,_,New_A),
	Start1 is Start + 2,
	sub_atom(A,Start1,After,_,B1),
	atom_concat(New_A,'#',A1), 
	atom_concat(A1,B1,B),!,print(A-B).

owl_fix_no(A,A).
*/
 

%%	owl_count(?U). 
%       Returns/Checks the number of unused OWL triples. 

owl_count(U) :- 
	findall(1,owl(_,_,_,not_used),X), length(X,U).


%%       test_use_owl(?S,?P,?O)   
%	As use_owl/3, but does not consume the triple

test_use_owl(X1,Y1,Z1) :- 
	expand_ns(X1,X),
	expand_ns(Y1,Y),
	expand_ns(Z1,Z),
	owl(X,Y,Z, not_used).

test_use_owl(X1,Y1,Z1,named) :- 
	expand_ns(X1,X),
	expand_ns(Y1,Y),
	expand_ns(Z1,Z),
	owl(X,Y,Z, not_used),
	not(sub_string(X,0,2,_,'__')).

%%       use_owl(+Triples:list)   
%	Marks a list of OWL triples as used, but only if all match. Expands the S,P,O.

use_owl(Triples) :-
        forall(member(owl(S,P,O),Triples),
               test_use_owl(S,P,O)),
        forall(member(owl(S,P,O),Triples),
               use_owl(S,P,O)).

%%       use_owl(?S,?P,?O)   
%	Marks an OWL triple as used. Expands the S,P,O.

use_owl(X1,Y1,Z1) :- 
	expand_ns(X1,X),
	expand_ns(Y1,Y),
	expand_ns(Z1,Z),
	owl(X,Y,Z, not_used),
	debug(owl_parser_detail,'using ~w ~w ~w',[X,Y,Z]),
	retract(owl(X,Y,Z, not_used)),
	assert(owl(X,Y,Z,used)).


%%	use_owl(?S,?P,?O,named). 
%
%       Same as use_owl/3, but marks only if S 	is Named URI (i.e. non blank node).

use_owl(X1,Y1,Z1,named) :- 
	expand_ns(X1,X),
	expand_ns(Y1,Y),
	expand_ns(Z1,Z),
	owl(X,Y,Z, not_used),
	not(sub_string(X,0,2,_,'__')), 
	retract(owl(X,Y,Z, not_used)),
	assert(owl(X,Y,Z,used)).


%%       expand_ns(+NS_URL, ?Full_URL)
%
%       Expands a 'namespaced' URI of the form ns:fragment to a full URI
%       substituting the full expansion for ns from the ns/2 facts
expand_ns(NS_URL, Full_URL) :-
	nonvar(NS_URL), 
	not(NS_URL = literal(_)),
	uri_split(NS_URL,Short_NS,Term, ':'), 
	rdf_db:ns(Short_NS,Long_NS),!,
	concat_atom([Long_NS,Term],Full_URL).

expand_ns(URL, URL).


%%       collapse_ns(+FullURL, ?NSURL, +Options)
%
%	Collapses a full URI of the form Path#fragment to a Namespaced
%	URI NS:fragment substituting the full expansion for ns from
%	the ns/2 facts
%	Char is either ':' for normal ns notation or '_' for builing
%	prolog terms.
%	Options supported: no_base(ShortNs): Use only term!


collapse_ns(FullURL, NSURL,Char,Options) :-
	nonvar(FullURL), 
	not(FullURL = literal(_)),
	uri_split(FullURL,LongNS, Term, '#'), 
	concat(LongNS,'#',LongNS1),
	rdf_db:ns(ShortNS,LongNS1),
	(   member(no_base(ShortNS),Options), ! , NSURL = Term 
	; 
	concat_atom([ShortNS,Char,Term],NSURL)
	),!.
% CJM
collapse_ns(FullURL, NSURL,_Char,Options) :-
	nonvar(FullURL), 
	not(FullURL = literal(_)),
	uri_split(FullURL,LongNS, Term, '#'), 
	member(no_base(LongNS),Options),
        !,
        NSURL = Term.


collapse_ns(URL, URL,_,_).



%%       uri_split(+URI,-Namespace,-Term,+Split_Char) :-
%
%       Splits a URI into the Namespace and the Term parts 
%       separated by the Split_Char character.
%       It supposes URI = concat(Namespace,Split_Char,Term)

uri_split(URI,Namespace,Term,Split_Char) :-
	sub_atom(URI,Start,_,After,Split_Char),
	sub_atom(URI,0,Start,_,Namespace),
	Start1 is Start + 1, 
	sub_atom(URI,Start1,After,_,Term). 

	
%%       owl_collect_linked_nodes(+Node,+Predicate, +InList,-OutList)

%	Appends Node to the InList, and recursively, all other
%	Nodes that are linked with the Predicate to the Node. The
%	result is returned to OutList.

owl_collect_linked_nodes(Node,Predicate,InList,OutList) :-
	use_owl(Node,Predicate,A),!,
	owl_collect_linked_nodes(Node,Predicate,InList,List1),
	owl_collect_linked_nodes(A,Predicate,List1,OutList).

owl_collect_linked_nodes(Node,Predicate,InList,OutList) :-
	use_owl(A,Predicate,Node),!,
	owl_collect_linked_nodes(Node,Predicate,InList,List1),
	owl_collect_linked_nodes(A,Predicate,List1,OutList).
	
owl_collect_linked_nodes(Node,_,List, [Node|List]) :- 
	not(memberchk(Node, List)),!.

owl_collect_linked_nodes(_,_,List, List) :- !.


% ----------------------------------------------------------------
%                OWL Parser implementation predicates	       
% ----------------------------------------------------------------

%%       owl_deprecated_class(+CID,-Deprecated).
%
%       Deprecated is set to true if Class CID is defined as deprecated.
%       false otherwise.

owl_deprecated_class(CID,Deprecated) :-
	use_owl(CID,'rdf:type','owl:DeprecatedClass'), Deprecated = true,!; 
	Deprecated = false.

%%       owl_deprecated_property(+PID,-Deprecated).
%
%	Deprecated is set to true if Property PID is defined as
%	deprecated; false otherwise.

owl_deprecated_property(PID,Deprecated) :-
	use_owl(PID,'rdf:type','owl:DeprecatedProperty'), Deprecated = true,!; 
	Deprecated = false.

%%       owl_get_bnode(+Node,+Description)
%
%	if Node is a blank (not named) node, then it is asserted in
%	the database as a blanknode(Node,Description,used) term.
%	The purpose is to record when a blank node has been used, so
%	subsequent uses of it will result in structure sharing. 

owl_get_bnode(Node,Description) :-
	sub_string(Node,0,2,_,'__'),!,
	not( blanknode(Node,_,_)),
	assert(blanknode(Node,Description, used)).

owl_get_bnode(_,_).

%%       owl_optional_type(+D).
%
%	It simply consumes any optional owl:Class or
%	rdfs:Class type triples for description D

use_optional_type(D) :- 
  use_owl(D,'rdf:type','owl:Class'), use_owl(D,'rdf:type','rdfs:Class'),!;
  use_owl(D,'rdf:type','rdfs:Class'),!;
  true.



% ----------------------------------------------------------------
% 3 Mapping from RDF Graphs to the Structural Specification
% ----------------------------------------------------------------

/*

  This section specifies the results of steps CP-2.2 and CP-3.3 of the
  canonical parsing process from Section 3.6 of the OWL 2
  Specification [OWL 2 Specification] on an ontology document D that
  can be parsed into an RDF graph G. An OWL 2 tool MAY implement these
  steps in any way it chooses; however, the results MUST be
  structurally equivalent to the ones defined in the following
  sections. These steps do not depend on the RDF syntax used to encode
  the RDF graph in D; therefore, the ontology document D is identified
  in this section with the corresponding RDF graph G.

An RDF syntax ontology document is any sequence of octets accessible
from some given IRI that can be parsed into an RDF graph, and that
then be transformed into an OWL 2 ontology by the canonical parsing
process instantiated as specified in this section.

The following sections contain rules in which triple patterns are
matched to G. If a triple pattern contains a variable number of
triples, the maximal possible subset of G MUST be matched. The
following notation is used in the patterns:

    * The notation NN_INT(n) can be matched to any literal whose value n is a nonnegative integer.
    * Possible conditions on the pattern are enclosed in curly braces { }.
    * Some patterns use optional parts, which are enclosed in square brackets '[ ]'.
    * The abbreviation T(SEQ y1 ... yn) denotes the pattern corresponding to RDF lists, as shown in Table 3.

  */

%       owl_description_list(+Node, -List) 
%
%       If +Node is defined as rdf:type rdf:List, then List returns
%       a prolog list of descriptions for this Node.

owl_description_list('http://www.w3.org/1999/02/22-rdf-syntax-ns#nil',[]) :- !.
	     
owl_description_list(X,[F|R]) :- 
	use_owl(X,'rdf:type','rdf:List'),
	use_owl(X,'rdf:first',Element),
	owl_description(Element,F),
	use_owl(X,'rdf:rest',Y),
	!,owl_description_list(Y,R).


%       owl_individual_list(+Node, -List) 
%
%       If +Node is defined as rdf:type rdf:List, then List returns
%       a prolog list of individuals for this Node.

owl_individual_list('http://www.w3.org/1999/02/22-rdf-syntax-ns#nil',[]) :- !.

owl_individual_list(X,[F|R]) :- 
	use_owl(X,'rdf:type','rdf:List'),
	use_owl(X,'rdf:first',F),
	use_owl(X,'rdf:rest',Y),
	!,owl_individual_list(Y,R).

%       owl_property_list(+Node, -List) 
%
%       If +Node is defined as rdf:type rdf:List, then List returns
%       a prolog list of propertys for this Node.

owl_property_list('http://www.w3.org/1999/02/22-rdf-syntax-ns#nil',[]) :- !.
	     
owl_property_list(X,[F|R]) :- 
	use_owl(X,'rdf:type','rdf:List'),
	use_owl(X,'rdf:first',Element),
	owl_property_expression(Element,F),
	use_owl(X,'rdf:rest',Y),
	!,owl_property_list(Y,R).

  
% 3.1 Extracting Declarations and the IRIs of the Directly Imported Ontology Documents
% This section specifies the result of step CP-2.2 of the canonical parsing process on an RDF graph G

% 3.1.2 Parsing of the Ontology Header and Declarations

owl_parse_axiom(ontology(O)) :-
        use_owl(O,'rdf:type','owl:Ontology'),
        nb_setval(current_ontology,O).

owl_parse_axiom(ontologyImport(O,IRI)) :-
        use_owl(O,'owl:imports',IRI).

owl_parse_axiom(ontologyVersionInfo(O,IRI)) :-
        use_owl(O,'owl:versionInfo',IRI).

% See table 5.
% TODO

%% triple_remove(L,T)

% See table 6.
% http://www.w3.org/TR/2008/WD-owl2-mapping-to-rdf-20081202/
triple_replacements(owl(X,'rdf:type','owl:OntologyProperty'),[owl(X,'rdf:type','owl:AnnotationProperty')]).
triple_replacements(owl(X,'rdf:type','owl:InverseFunctionalProperty'),[owl(X,'rdf:type','owl:ObjectProperty'),owl(X,'rdf:type','owl:InverseFunctionalProperty')]).
triple_replacements(owl(X,'rdf:type','owl:TransitiveProperty'),[owl(X,'rdf:type','owl:ObjectProperty'),owl(X,'rdf:type','owl:TransitiveProperty')]).
triple_replacements(owl(X,'rdf:type','owl:SymmetricProperty'),[owl(X,'rdf:type','owl:ObjectProperty'),owl(X,'rdf:type','owl:SymmetricProperty')]).


% DECLARATIONS

% See table 7.
% http://www.w3.org/TR/2008/WD-owl2-mapping-to-rdf-20081202/

owl_parse_axiom(class(C)) :-
        (   use_owl(C,'rdf:type','owl:Class',named) ; use_owl(C,'rdf:type','rdfs:Class',named)).
owl_parse_axiom(class(C)) :-
        use_owl([owl(X,'rdf:type','owl:Axiom'),
                 owl(X,'owl:subject',C),
                 owl(X,'owl:predicate','rdf:type'),
                 owl(X,'owl:object','owl:Class')]).

owl_parse_axiom(datatype(D)) :-
        use_owl(D,'rdf:type','rdf:Datatype').
owl_parse_axiom(datatype(C)) :-
        use_owl([owl(X,'rdf:type','owl:Axiom'),
                 owl(X,'owl:subject',C),
                 owl(X,'owl:predicate','rdf:type'),
                 owl(X,'owl:object','owl:Datatype')]).

owl_parse_axiom(objectProperty(D)) :-
        use_owl(D,'rdf:type','owl:ObjectProperty').
owl_parse_axiom(objectProperty(C)) :-
        use_owl([owl(X,'rdf:type','owl:Axiom'),
                 owl(X,'owl:subject',C),
                 owl(X,'owl:predicate','rdf:type'),
                 owl(X,'owl:object','owl:ObjectProperty')]).

% note the difference in names between syntax and rdf
owl_parse_axiom(dataProperty(D)) :-
        use_owl(D,'rdf:type','owl:DatatypeProperty').
owl_parse_axiom(dataProperty(C)) :-
        use_owl([owl(X,'rdf:type','owl:Axiom'),
                 owl(X,'owl:subject',C),
                 owl(X,'owl:predicate','rdf:type'),
                 owl(X,'owl:object','owl:DatatypeProperty')]).

owl_parse_axiom(annotationProperty(D)) :-
        use_owl(D,'rdf:type','owl:AnnotationProperty').
owl_parse_axiom(annotationProperty(C)) :-
        use_owl([owl(X,'rdf:type','owl:Axiom'),
                 owl(X,'owl:subject',C),
                 owl(X,'owl:predicate','rdf:type'),
                 owl(X,'owl:object','owl:AnnotationProperty')]).

owl_parse_axiom(namedIndividual(D)) :-
        use_owl(D,'rdf:type','owl:NamedIndividual').
owl_parse_axiom(namedIndividual(C)) :-
        use_owl([owl(X,'rdf:type','owl:Axiom'),
                 owl(X,'owl:subject',C),
                 owl(X,'owl:predicate','rdf:type'),
                 owl(X,'owl:object','owl:NamedIndividual')]).

% Table 8. Identifying Anonymous Individuals in Reification
% TODO


% 3.2 Populating an Ontology


% 3.2.1 Analyzing Declarations

% 3.2.2 Parsing of Annotations

% TODO

% 3.2.4 Parsing of Expressions

% Table 11. Parsing Object Property Expressions

owl_property_expression(C,C) :- 
	not(sub_string(C,0,2,_,'__')).

owl_property_expression(C,D) :- 
	blanknode(C,D,Use),
	(   Use = used, owl_parser_log(C-D), 
	    retractall(blanknode(C,D,used)),
	    assert(blanknode(C,D,shared))
	;
	    true).

owl_property_expression(P,inverseOf(Q)) :-
        use_owl(P,'owl:inverseOf',Q),
        owl_get_bnode(P,inverseOf(Q)).


% Table 12. Parsing of Data Ranges

% TODO

% Table 13. Parsing of Class Expressions

% ----------------------------------------------------------------------
%       owl_description(+Node,-Description).
%
%	It implements OWL AS production rules for Descriptions.
%       I.e. a Description can be any of
%         - a Class ID 
%	  - an existing blank node (in which case we have structure
%	    sharing),
%         - a unionOf(DescriptionList) term. 
%         - a intersectionOf(DescriptionList) term.
%         - a complementOf(Description) term.
%         - a oneOf(IndividualList) term.
%
%         During the construction of the Description any blank node 
%         is recorded for later structure sharing checks. 

owl_description(C,C) :- 
	not(sub_string(C,0,2,_,'__')).

owl_description(C,D) :- 
	blanknode(C,D,Use),
	(   Use = used, owl_parser_log(C-D), 
	    retractall(blanknode(C,D,used)),
	    assert(blanknode(C,D,shared))
	;
	    true).


owl_description(D,unionOf(L)) :- 
	use_owl(D,'owl:unionOf',L1),
	use_optional_type(D),
	owl_description_list(L1,L),
	owl_get_bnode(D,unionOf(L)).

owl_description(D,intersectionOf(L)) :- 
	use_owl(D,'owl:intersectionOf',L1),
	use_optional_type(D),
	owl_description_list(L1,L),
	owl_get_bnode(D,intersectionOf(L)).

owl_description(D,complementOf(Descr)) :- 
	use_owl(D,'owl:complementOf',D1),
	use_optional_type(D),	
	owl_description(D1,Descr),
	owl_get_bnode(D,complementOf(Descr)).

owl_description(D,oneOf(L)) :- 
	use_owl(D,'owl:oneOf',L1),
	use_optional_type(D),
	owl_individual_list(L1,L),
	owl_get_bnode(D,oneOf(L)).

owl_description(D,Restriction) :- 
	owl_restriction(D, Restriction),
	use_optional_type(D),
	owl_get_bnode(D,Restriction).

% TODO: datatype restriction


%       owl_restriction(+Element,-Restriction).
%
%       If Element is defined as a owl:Restriction on property P then
%       Restriction binds to a restriction(Property,Type) term, 
%	according to OWL Abstract syntax specification.

owl_restriction(Element,Restriction) :- 
	use_owl(Element,'rdf:type','owl:Restriction'),
	use_owl(Element, 'owl:onProperty',PropertyID),
	owl_restriction_type(Element,PropertyID, Restriction),!,
        debug(owl_parser_detail,'Restriction: ~w',[Restriction]).


owl_restriction_type(E, P, someValuesFrom(PX, Descr)) :- 
	use_owl(E, 'owl:someValuesFrom',D),
	owl_description(D, Descr),!,
        owl_property_expression(P, PX).

owl_restriction_type(E, P, allValuesFrom(PX,Descr)) :- 
	use_owl(E, 'owl:allValuesFrom',D),
	owl_description(D, Descr),!,
        owl_property_expression(P, PX).

owl_restriction_type(E, P, hasSelf(PX)) :- 
	use_owl(E, 'owl:hasSelf', true),
        owl_property_expression(P, PX).

% changed from Thea: cardinality->exactCardinality
owl_restriction_type(E, P,exactCardinality(C,PX)) :- 
	use_owl(E, 'owl:cardinality',C),
        owl_property_expression(P, PX).

owl_restriction_type(E, P,exactCardinality(C,PX,DX)) :- 
	use_owl(E, 'owl:qualifiedCardinality',C),
	use_owl(E, 'owl:onClass',D),
	owl_description(D, DX),!,
        owl_property_expression(P, PX).

owl_restriction_type(E, P, minCardinality(C,PX)) :- 
	use_owl(E, 'owl:minCardinality',C),
        owl_property_expression(P, PX).

owl_restriction_type(E, P, minCardinality(C,PX,DX)) :- 
	use_owl(E, 'owl:minQualifiedCardinality',C),
	use_owl(E, 'owl:onClass',D),
	owl_description(D, DX),!,
        owl_property_expression(P, PX).

owl_restriction_type(E, P, maxCardinality(C,PX)) :- 
	use_owl(E, 'owl:maxCardinality',C),
        owl_property_expression(P, PX).

owl_restriction_type(E, P, maxCardinality(C,PX,DX)) :- 
	use_owl(E, 'owl:maxQualifiedCardinality',C),
	use_owl(E, 'owl:onClass',D),
	owl_description(D, DX),!,
        owl_property_expression(P, PX).

% changed from thea value-->hasValue
owl_restriction_type(E, P, hasValue(PX,Value)) :- 
	use_owl(E, 'owl:hasValue',Value),
        owl_property_expression(P, PX).

% Table 14. Parsing of Data Ranges for Compatibility with OWL DL
% TODO

% Table 15. Parsing of Class Expressions for Compatibility with OWL DL
% TODO


% Table 16. Parsing of Axioms without Annotations

% Declarations handled previously

% CLASS AXIOMS

owl_parse_axiom(subClassOf(DX,DY)) :- 
	use_owl(X,'rdfs:subClassOf',Y),	
        owl_description(X,DX),
	owl_description(Y,DY).

owl_parse_axiom(equivalentClasses(DL)) :- 
        maximally_connected_subgraph_over('owl:equivalentClass',L),
        maplist(owl_description,L,DL),
        debug(owl_parser_detail,'equivalentClasses Descs: ~w',[DL]).
owl_parse_axiom(equivalentClasses([C,D])) :-
        % TODO: this could be made more efficient by enforcing order of building
        (   test_use_owl(C,'rdf:type','owl:Class',named)
        ;   test_use_owl(C,'rdf:type','rdfs:Class',named)
        ;   class(C)),
        owl_description(C,D),
        C\=D.

owl_parse_axiom(disjointClasses([DX,DY])) :- 
	use_owl(X,'owl:disjointClass',Y),	
        owl_description(X,DX),
	owl_description(Y,DY).

owl_parse_axiom(disjointClasses(L)) :-
        % TODO: X may be referred to in an annotation axiom??
	use_owl(X,'rdf:type','owl:AllDisjointClasses'),
        use_owl(X,'owl:members',L1),
        owl_description_list(L1,L).

owl_parse_axiom(disjointUnion(DX,L)) :- 
	use_owl(X,'owl:disjointUnionOf',L1),
        owl_description(X,DX),
        owl_description_list(L1,L).



% PROPERTY AXIOMS



% introduces bnode
owl_parse_axiom(subPropertyOf(propertyChain(PL),QX)) :-
        use_owl(X,'owl:propertyChain',L1),
        use_owl(X,'rdfs:subPropertyOf',Q),
        owl_property_list(L1,PL),
        owl_property_expression(Q,QX).

owl_parse_axiom(subPropertyOf(PX,QX)) :-
        use_owl(P,'rdfs:subPropertyOf',Q),
        owl_property_expression(P,PX),
        owl_property_expression(Q,QX).

owl_parse_axiom(equivalentProperties(DL)) :- 
        maximally_connected_subgraph_over('owl:equivalentProperty',L),
        maplist(owl_property_expression,L,DL).

owl_parse_axiom(disjointProperties([DX,DY])) :- 
	use_owl(X,'owl:disjointClass',Y),	
        owl_description(X,DX),
	owl_description(Y,DY).

owl_parse_axiom(disjointProperties(L)) :-
        % TODO: X may be referred to in an annotation axiom??
	use_owl(X,'rdf:type','owl:AllDisjointProperties'),
        use_owl(X,'owl:members',L1),
        L1 = [_,_|_],           % length >= 2
        owl_property_list(L1,L).

owl_parse_axiom(propertyDomain(PX,CX)) :-
        use_owl(P,'rdfs:domain',C),
        owl_property_expression(P,PX),
        owl_description(C,CX).

owl_parse_axiom(propertyRange(PX,CX)) :-
        use_owl(P,'rdfs:range',C),
        owl_property_expression(P,PX),
        owl_description(C,CX).

owl_parse_axiom(inverseProperties(PX,QX)) :-
        use_owl(P,'owl:inverseOf',Q),
        owl_property_expression(P,PX),
        owl_property_expression(Q,QX).

owl_parse_axiom(functionalProperty(P)) :-
        use_owl(P,'rdf:type','owl:FunctionalProperty').
owl_parse_axiom(inverseFunctionalProperty(P)) :-
        use_owl(P,'rdf:type','owl:InverseFunctionalProperty').
owl_parse_axiom(reflexiveProperty(P)) :-
        use_owl(P,'rdf:type','owl:ReflexiveProperty').
owl_parse_axiom(irreflexiveProperty(P)) :-
        use_owl(P,'rdf:type','owl:IrreflexiveProperty').
owl_parse_axiom(symmetricProperty(P)) :-
        use_owl(P,'rdf:type','owl:SymmetricProperty').
owl_parse_axiom(asymmetricProperty(P)) :-
        use_owl(P,'rdf:type','owl:AsymmetricProperty').
owl_parse_axiom(transitiveProperty(P)) :-
        use_owl(P,'rdf:type','owl:TransitiveProperty').


% INDIVIDUALS

owl_parse_axiom(sameIndividual(IL)) :- 
        maximally_connected_subgraph_over('owl:equivalentIndividual',L),
        maplist(owl_individual,L,IL).

owl_parse_axiom(differentIndividuals([DX,DY])) :- 
	use_owl(X,'owl:disjointClass',Y),	
        owl_description(X,DX),
	owl_description(Y,DY).

owl_parse_axiom(differentIndividuals(L)) :-
        % TODO: X may be referred to in an annotation axiom??
	use_owl(X,'rdf:type','owl:AllDifferent'),
        use_owl(X,'owl:distinctMembers',L1),
        owl_individual_list(L1,L).

dothislater(classAssertion/3).
owl_parse_axiom(classAssertion(CX,I)) :-
        use_owl(I,'rdf:type',C),
        owl_description(C,CX).

dothislater(propertyAssertion/3).
owl_parse_axiom(propertyAssertion(PX,A,B)) :-
        test_use_owl(A,P,B), % B can be literal or individual
        \+ annotationProperty(P),
        use_owl(A,P,B), % consume now
        owl_property_expression(P,PX). % can also be inverse

owl_parse_axiom(negativePropertyAssertion(PX,A,B)) :-
        use_owl(X,'rdf:type','owl:NegativePropertyAssertion'),
        use_owl(X,'owl:sourceIndividual',A),
        use_owl(X,'owl:assertionProperty',P),
        use_owl(X,'owl:targetValue',B),
        owl_property_expression(P,PX). % can also be inverse

dothislater(annotationAssertion/3).
owl_parse_axiom(annotationAssertion(P,A,B)) :-
        use_owl(A,P,B), % B can be literal or individual
        annotationProperty(P).

% Table 17. Parsing of Annotated Axioms


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
        use_owl(X,P,Y), % seed
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
        use_owl(X,P,Y),
        !,
        extend_set_over(P,[Y|L],L2).
extend_set_over(P,L,L2):-
        member(X,L),
        test_use_owl(Y,P,X),
        \+ member(Y,L),
        use_owl(Y,P,X),
        !,
        extend_set_over(P,[Y|L],L2).
extend_set_over(_,L,L):- !.


	

%	owl_annotation(+C,annotation(-APID,-Value)
%  
%	For a given name id (C) it returns an annotation construct.
%       APID is either an existing annotation Property, or it is a new
%       one. 
%       Predefined annotation properties are rdfs:comment, rdfs:label, 
%	rdfs:seeAlso.

owl_annotation(C,annotation(APID,Value)) :- 
	annotationProperty(APID),
	use_owl(C,APID,Value).

owl_annotation(C,annotation(APID,Value)) :-
	use_owl(APID,'rdf:type','owl:AnnotationProperty'),
	(   use_owl(APID,'rdf:type','rdf:Property'),! ; true),
	not(sub_string(APID,0,2,_,'__')), 
	not(annotationProperty(APID)),  
	assert(annotationProperty(APID)),
	use_owl(C,APID,Value).

owl_annotation(C, annotation('rdfs:comment',CA)) :-
  	use_owl(C,'rdfs:comment',CA).	

owl_annotation(O, annotation('rdfs:label',OA)) :-
  	use_owl(O,'rdfs:label',OA).		     

owl_annotation(O, annotation('rdfs:seeAlso',OA)) :-
  	use_owl(O,'rdfs:seeAlso',OA).	


%	owl_parse_annotationPropery.
%  
%	It creates an annotationProperty term for each occurence of an  
%	owl:AnnotationProperty typed ID.
%	Range properies for annotation are not processed yet.
 
owl_parse_annotationProperty :- 
	use_owl(PID, 'rdf:type', 'owl:AnnotationProperty'),
	not(sub_string(PID,0,2,_,'__')), 
	not(annotationProperty(PID)),  
	assert(annotationProperty(PID)),
	% get all range clauses for annotation but don't do anything at the moment
	findall(Dr, (use_owl(PID,'rdfs:range',Xr),owl_description(Xr,Dr)), _),

	% use_owl(PID, 'rdf:type','rdf:Property'); true,
	fail.

owl_parse_annotationProperty.



% --------------------------------------------------------------------
%                             Individuals
%
%       owl_parse_named_individuals
%  
%	Any named node not defined as an individual is sserted into the
%	database as a individual/5 term with all types, properties and
%	annotations defined with this named individual as a subject. 
%	Note that the construction of an individual term cannot
%	be done incrementally, i.e. we cannot add types,
%	properties or annotations to an existing individual.


owl_parse_named_individuals :- 
	owl(I,_,_,not_used),
	not(sub_string(I,0,2,_,'__')), not(individual(I,_,_,_)),
	findall(T, (use_owl(I,'rdf:type',T1),owl_description(T1,T)),ITList),
	findall(value(P,V), ((property(P,_,_,_,_,_,_);			    
			     annotationProperty(P)),use_owl(I,P,V)), IVList),
 	findall(A,(owl_annotation(I,A)), AnnotationList), 
	assert(individual(I,AnnotationList,ITList,IVList)),fail.

owl_parse_named_individuals.


%       owl_parse_unnamed_individuals
%  
%	Same as above for unnamed individuals.

owl_parse_unnamed_individuals:-
	owl(I,_,_,not_used),not(individual(I,_,_,_)),
	findall(T, (use_owl(I,'rdf:type',T1),owl_description(T1,T)),ITList),
	findall(value(P,V), ((property(P,_,_,_,_,_,_);			    
			     annotationProperty(P)),use_owl(I,P,V)), IVList),
	findall(A,(owl_annotation(I,A)), AnnotationList), 
	assert(individual(I,AnnotationList,ITList,IVList)),fail.

owl_parse_unnamed_individuals.














