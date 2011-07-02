/* -*- Mode: Prolog -*- */

:- module(owl2_xml,
          [
           owl_parse_xml/1,
           owl_parse_xml/2,

	   axiom_xml/3,
	   xml_axiom/3,
	   desc_xml/3,
	   axioms_elts/3,
	   xml_desc/3
          ]).

:- use_module(owl2_model).
:- use_module(owl2_xml).
:- use_module(owl2_metamodel).
:- use_module(owl2_io).
:- use_module(library(sgml)).
:- use_module(library(semweb/rdf_db)).

% DEPENDENCY: SWI-Prolog or Yap
%:- use_module(library('semweb/rdf_db')).


% ----------------------------------------
% TRANSLATING FROM XML
% ----------------------------------------

:- multifile owl2_io:load_axioms_hook/3.
owl2_io:load_axioms_hook(File,owlx,Opts) :-
        owl_parse_xml(File,Opts).

owl_parse_xml(File) :-
        owl_parse_xml(File,[]).

owl_parse_xml(File,_Opts) :-
        load_structure(File,[XML],[dialect(xmlns),space(remove)]),
        xml_ontology(XML,Ont,Axioms),
        assert_axiom(Ont),
        forall(member(Axiom,Axioms),
               assert_axiom(Axiom)),
        !.
owl_parse_xml(File,_) :-
        throw(no_parse(File)).

xsb_owl_parse_xml(File,_Opts) :-
        load_structure(file(File),[XML],[dialect(xmlns),space(remove)],_Warn),
        xml_ontology(XML,Ont,Axioms),
        assert_axiom(Ont),
        forall(member(Axiom,Axioms),
               assert_axiom(Axiom)),
        !.

%% xml_ontology(+XML, ?Ont, ?Axioms:list) is det
% Translate XML term conforming to OWLX to a collecton of owl2_model axioms
xml_ontology(element(_:'Ontology',Atts,Elts),ontology(O),Axioms) :-
        %atts_iri(Atts,O),
        member('ontologyIRI'=O,Atts),
        % TODO: imports
        elts_axioms(O,Elts,Axioms).


%% elts_axioms(+O, +Elts:list, ?Axioms:list) is det
% recursively translate a list of XML elements to OWL axioms,
% use xml_axiom/3 to map an axiom XML term to an owl2_model axiom
elts_axioms(_,[],[]).
elts_axioms(O,[E|Elts],Axioms2):-
        xml_axiom(O,E,A),
        !,
        elts_axioms(O,Elts,Axioms),
        (   is_list(A)
        ->  append(A,Axioms,Axioms2)
        ;   Axioms2=[A|Axioms]).
elts_axioms(O,[E|_],_) :-
        throw(error(cannot_translate_axiom(E,O))).


%% xml_axiom(+O,+XML,?Axiom)
% translate a single element to an axiom

% ANNOTATIONS: TODO: this is P4 style
xml_axiom(Ont,element(_:'EntityAnnotation',_Atts,[Decl|Elts]),[EntityAxiom|Annots]) :-
        !,
        xml_axiom(Ont,Decl,EntityAxiom),
        EntityAxiom=..[_,E],
        findall(annotationAssertion(E,P,V),
                (   member(AX,Elts),
                    xml_annotation(AX,annotation(P,V))),
                Annots).

% TODO: clarify. P4 exports declarations directly
xml_axiom(Ont,element(_:'Declaration',_,[Decl]),Axiom) :-
        !,
        xml_axiom(Ont,Decl,Axiom).

% translate an entity axiom (OWLClass, OWLIndividual, etc)
xml_axiom(_Ont,element(_:Name,Atts,_),Axiom) :-
        xmle_entity(Name,Axiom,[IRI]),
        !,
        atts_iri(Atts,IRI).

% translate a full axiom (SubClassOf etc), translating each
% argument to descriptions
xml_axiom(_Ont,element(_:Name,_Atts,Sub),Axiom) :-
        xmle_axiom(Name,Axiom,Args),
        !,
        maplist(axiom_xml_arg(Name),Sub,Args).

xml_axiom(Ont,Elt,_) :-
        throw(xml_axiom(Ont,Elt)).


% if axiom argument is a class expression then call the
% desc_xml
% VV 18/12/2009 for OWLlink individual support
axiom_xml_arg(ParentAxiom,c(Arg),Element) :-
	desc_xml(ParentAxiom,Arg,Element),!.

% everything else return the argument itself eg. property, individual
%
axiom_xml_arg(_ParentAxiom,element('http://www.w3.org/2006/12/owl2-xml#':'NamedIndividual',['URI'=IRI],[]),i(IRI)) :- !.
axiom_xml_arg(_ParentAxiom,element('http://www.w3.org/2006/12/owl2-xml#':'ObjectProperty',['URI'=IRI],[]),op(IRI)) :- !.
axiom_xml_arg(_ParentAxiom,element('http://www.w3.org/2006/12/owl2-xml#':'DataProperty',['URI'=IRI],[]),dp(IRI)) :- !.
axiom_xml_arg(_ParentAxiom,element('http://www.w3.org/2006/12/owl2-xml#':'AnnotationProperty',['URI'=IRI],[]),ap(IRI)) :- !.
axiom_xml_arg(_ParentAxiom,X,X) :- !.

%% xml_desc(+ParentElementName, +XML, ?Description)
% translate an OWL XML term to an owl2_model description

% eg someValuesFrom(PE,X)
xml_desc(_Parent,element(_:Name,_Atts,[PEX|Elts]),Desc) :-
        xmle_PEdescription(Name,Pred),
        !,
        %xml_propertyExpr(Name,PEX,PE),
        xml_desc(Name,PEX,PE),
        maplist(xml_desc(Name),Elts,Fillers),
        Desc=..[Pred,PE|Fillers].

% eg minCardinality(Card,PE,X)
xml_desc(_Parent,element(_:Name,Atts,[PEX|FillersX]),Desc) :-
        xmle_functor(Name,F),
        !,
        member(cardinality=Card,Atts),
        %xml_propertyExpr(Name,PEX,PE),
        xml_desc(Name,PEX,PE),
        maplist(xml_desc(Name),FillersX,Fillers),
        Desc=..[F,Card,PE|Fillers].

% eg intersectionOf(L)
xml_desc(_Parent,element(_:Name,_Atts,Elts),Desc) :-
        xmle_functor(Name,Pred),
        !,
        maplist(xml_desc(Name),Elts,Fillers),
        Desc=..[Pred,Fillers].

% eg OWLClass
% TODO: counts as declaration?
xml_desc(_Parent,element(_:_Name,Atts,[]),IRI) :-
        % TODO: _Name
        iri_att(A),
        member(A=IRI,Atts),
        !.

%VV add 15/10 for OWLLink support
xml_desc(_Parent,element('owl:Class',Atts,[]),IRI) :-
        % TODO: _Name
        iri_att(A),
        member(A=IRI,Atts),
        !.


xml_desc(Parent,Elt,_) :-
        throw(xml_desc(Elt,in(Parent))).

% TODO: this is P4 style
xml_annotation(element(_:'Annotation',Atts,Elts),annotation(P,V)) :-
        !,
        member(annotationURI=P,Atts),
        Elts=[element(_:'Constant',_,[V])].

atts_iri(Atts,TIRI) :-
  	(   TIRI = i(IRI) ; TIRI = c(IRI) ; TIRI = op(IRI); TIRI = dp(IRI) ; var(IRI)),!,
	iri_att(A),
        member(A=IRI,Atts).


iri_att('URI').  % TODO clarify. P4 uses this
iri_att('abbreviatedIRI').  % VV add 15/10 for OWLLink support
iri_att('IRI'). % VV add 15/10 for OWLLink support


% ----------------------------------------
% GENERATING XML
% ----------------------------------------

%:- rdf_register_ns(owlx,'http://www.w3.org/2006/12/owl2-xml#').


:- multifile owl2_io:save_axioms_hook/3.
owl2_io:save_axioms_hook(File,owlx,Opts) :-
        owl_generate_xml(File,Opts).

owl_generate_xml(File) :-
        owl_generate_xml(File,[]).

owl_generate_xml(File,_Opts) :-
        findall(Axiom,axiom(Axiom),Axioms),
        (   select(ontology(Ont),Axioms,Axioms2)
        ->  true
        ;   Ont='http://example.com',Axioms2=Axioms),
        ontology_xml(Ont,Axioms2,XML),
        debug(owl_exporter,'Writing to ~w',[XML]),
        (   nonvar(File)
        ->  open(File,write,IO,[dialect(xml)])
        ;   open(pipe(cat),write,IO,[dialect(xml)])),
        xml_write(IO,XML,[
%                          nsmap([owlx='http://www.w3.org/2006/12/owl2-xml#'])
                         ]),
        close(IO),
        !.
owl_generate_xml(File,_) :-
        throw(no_generate(File)).

ontology_xml(O,Axioms,element('http://www.w3.org/2006/12/owl2-xml#':'Ontology',['URI'=O],Elts)) :-
        debug(owl_exporter,'Writing ~w',[O]),
        axioms_elts(O,Axioms,Elts).

axioms_elts(_,[],[]).
% cjm 2010-06-24 - assume processed elsewhere
axioms_elts(O,[ontology(_)|Axioms],Elts):-
        !,
        axioms_elts(O,Axioms,Elts).
axioms_elts(O,[A|Axioms],[E|Elts]):-
        axiom_xml(O,A,E),
        !,
        axioms_elts(O,Axioms,Elts).
axioms_elts(O,[A|_],_) :-
        throw(error(axiom_elts(A,O))).

% translate entity axiom to XML
axiom_xml(_Ont,Axiom,XML) :-
                               % TODO: annotations
        xmle_entity(_Name,Axiom,[Arg]),
	axiom_arg_xml(Axiom,Arg,XML),!.

% translate full axiom to XML, translating description arguments also
axiom_xml(_Ont,Axiom,element('http://www.w3.org/2006/12/owl2-xml#':Name,[],Subs)) :-
        xmle_axiom(Name,Axiom,Args),
        !,
        debug(owl_exporter,'Axiom: ~w',[Axiom]),
        maplist(axiom_arg_xml(Name),Args,Subs).
axiom_xml(_Ont,Axiom,_) :-
        throw(error(xmle_axiom(Axiom))).

% if axiom argument is a class expression then call the
% desc_xml
% VV 18/12/2009 for OWLlink individual support
axiom_arg_xml(ParentAxiom,c(Arg),Element) :-
	desc_xml(ParentAxiom,Arg,Element),!.

% everything else return the argument itself eg. property, individual
%
axiom_arg_xml(_ParentAxiom,c(IRI),element('http://www.w3.org/2006/12/owl2-xml#':'Class',['URI'=IRI],[])) :- !.
axiom_arg_xml(_ParentAxiom,i(IRI),element('http://www.w3.org/2006/12/owl2-xml#':'NamedIndividual',['URI'=IRI],[])) :- !.
axiom_arg_xml(_ParentAxiom,i(IRI),element('http://www.w3.org/2006/12/owl2-xml#':'Individual',['URI'=IRI],[])) :- !.
axiom_arg_xml(_ParentAxiom,op(IRI),element('http://www.w3.org/2006/12/owl2-xml#':'ObjectProperty',['URI'=IRI],[])) :- !.
axiom_arg_xml(_ParentAxiom,dp(IRI),element('http://www.w3.org/2006/12/owl2-xml#':'DataProperty',['URI'=IRI],[])) :- !.
axiom_arg_xml(_ParentAxiom,ap(IRI),element('http://www.w3.org/2006/12/owl2-xml#':'AnnotationProperty',['URI'=IRI],[])) :- !.
axiom_arg_xml(_ParentAxiom,X,X) :- !.


% property chains
desc_xml(_Parent,propertyChain(PL),element('http://www.w3.org/2006/12/owl2-xml#':'ObjectPropertyChain',_,Elts)) :-
        !,
        maplist(desc_xml('ObjectPropertyChain'),PL,Elts).

% eg unionOf
desc_xml(_Parent,Desc,element('http://www.w3.org/2006/12/owl2-xml#':Name,_Atts,Elts)) :-
        % e.g. someValuesFrom(P,D)
        Desc=..[Pred,Args],
        is_list(Args),
        owlpredicate_typed(Pred,TypedPred),
        xmle_PEdescription(Name,TypedPred),
        Test=..[TypedPred,Desc],
        Test,
        debug(owl_exporter,'Desc: ~w Typed: ~w',[Desc,TypedPred]),
        !,
        maplist(desc_xml(Name),Args,Elts).

% translate object property expressions, eg
desc_xml(_Parent,Desc,element('http://www.w3.org/2006/12/owl2-xml#':Name,_Atts,Elts)) :-
        % e.g. someValuesFrom(P,D)
        Desc=..[Pred|Args],
        owlpredicate_typed(Pred,TypedPred),
        xmle_PEdescription(Name,TypedPred),
        Test=..[TypedPred,Desc],
        Test,
        debug(owl_exporter,'Desc: ~w Typed: ~w',[Desc,TypedPred]),
        !,
        maplist(desc_xml(Name),Args,Elts).

% eg minCardinality(Card,PE,X)
desc_xml(_Parent,Desc,element('http://www.w3.org/2006/12/owl2-xml#':Name,[cardinality=Card],Elts)) :-
        % e.g. unionOf(L)
        Desc=..[Pred,Card|Args],
        owlpredicate_typed(Pred,TypedPred),
        xmle_functor(Name,TypedPred),
        Test=..[TypedPred,Desc],
        Test,
        !,
        maplist(desc_xml(Name),Args,Elts).

% eg intersectionOf(L)
desc_xml(_Parent,Desc,element('http://www.w3.org/2006/12/owl2-xml#':Name,[],Elts)) :-
        Desc=..[Pred|Fillers],
        xmle_functor(Name,Pred),
        !,
        debug(owl_exporter,'Desc: ~w',[Desc]),
        maplist(desc_xml(Name),Fillers,Elts).

% eg OWLClass
% TODO: counts as declaration?
desc_xml(_Parent,IRI,element('http://www.w3.org/2006/12/owl2-xml#':Name,['URI'=IRI],[])) :-
        atom(IRI),
        xmle_entity(Name,Goal,[IRI]),
        Goal,
        !.

% TODO!!! this is just guessing
% see wine.owl, if we do not follow imports, cannot determine what PotableLiquid is,
% except by guesswork
desc_xml(_Parent,IRI,element('http://www.w3.org/2006/12/owl2-xml#':'Class',['URI'=IRI],[])) :-
        atom(IRI),
        !.

desc_xml(_Ont,Desc,_) :-
        throw(description(Desc)).


% ----------------------------------------
% METAMODEL
% ----------------------------------------

% TODO: use owl2_metamodel
xmle_cardpred('ObjectMinCardinality',minCardinality).
xmle_cardpred('ObjectMaxCardinality',maxCardinality).
xmle_cardpred('ObjectExactCardinality',exactCardinality).

xmle_cardpred('DataMinCardinality',minCardinality).
xmle_cardpred('DataMaxCardinality',maxCardinality).
xmle_cardpred('DataExactCardinality',exactCardinality).

xmle_PEdescription('ObjectIntersectionOf',objectIntersectionOf).
xmle_PEdescription('ObjectUnionOf',objectUnionOf).
xmle_PEdescription('ObjectComplementOf',objectComplementOf).
xmle_PEdescription('ObjectOneOf',objectOneOf).
xmle_PEdescription('ObjectSomeValuesFrom',objectSomeValuesFrom).
xmle_PEdescription('ObjectAllValuesFrom',objectAllValuesFrom).
xmle_PEdescription('ObjectHasValue',objectHasValue).
xmle_PEdescription('ObjectHasSelf',objectHasSelf).

xmle_PEdescription('DataIntersectionOf',dataIntersectionOf).
xmle_PEdescription('DataUnionOf',dataUnionOf).
xmle_PEdescription('DataComplementOf',dataComplementOf).
xmle_PEdescription('DataOneOf',dataOneOf).
xmle_PEdescription('DataSomeValuesFrom',dataSomeValuesFrom).
xmle_PEdescription('DataAllValuesFrom',dataAllValuesFrom).
xmle_PEdescription('DataHasValue',dataHasValue).

xmle_functor('ObjectMinCardinality',objectMinCardinality).
xmle_functor('ObjectMaxCardinality',objectMaxCardinality).
xmle_functor('ObjectExactCardinality',objectExactCardinality).
xmle_functor('DataSomeValuesFrom',dataSomeValuesFrom).
xmle_functor('DataAllValuesFrom',dataAllValuesFrom).
xmle_functor('DataHasValue',dataHasValue).
xmle_functor('DataMinCardinality',dataMinCardinality).
xmle_functor('DataMaxCardinality',dataMaxCardinality).
xmle_functor('DataExactCardinality',dataExactCardinality).


% axiompred(P/A),functor(H,P,A),H=..[P|Args],atom_chars(P,[C|Chars]),upcase_atom(C,C2),atom_chars(P2,[C2|Chars]),formatq('xmle_axiom(~q,~w,~w).~n',[P2,H,Args]),fail.

% declarations
xmle_entity('Class',class(A),[c(A)]).
%xmle_entity('OWLClass',class(A),[A]).  % TODO: clarify. Protege4 exports this
xmle_entity('Datatype',datatype(A),[dt(A)]).
xmle_entity('ObjectProperty',objectProperty(A),[op(A)]).
xmle_entity('DataProperty',dataProperty(A),[dp(A)]).
xmle_entity('AnnotationProperty',annotationProperty(A),[ap(A)]).
xmle_entity('NamedIndividual',namedIndividual(A),[i(A)]). % TODO - check
xmle_entity('Individual',individual(A),[i(A)]).

xmle_axiom('SubClassOf',subClassOf(A, B),[c(A), c(B)]).
xmle_axiom('EquivalentClasses',equivalentClasses(A),A1) :- maplist(iri_type(c),A,A1).
xmle_axiom('DisjointClasses',disjointClasses(A),A1) :- maplist(iri_type(c),A,A1).
xmle_axiom('DisjointUnion',disjointUnion(A, B),[c(A), c(B)]).

xmle_axiom('SubObjectPropertyOf',subPropertyOf(A, B),[op(A), op(B)]).
xmle_axiom('SubDataPropertyOf',subPropertyOf(A, B),[dp(A), dp(B)]).

xmle_axiom('EquivalentObjectProperties',equivalentProperties(A),A1) :- maplist(iri_type(op),A,A1).
xmle_axiom('EquivalentDataProperties',equivalentProperties(A),A1) :- maplist(iri_type(dp),A,A1).

xmle_axiom('DisjointObjectProperties',disjointProperties(A),A1) :- maplist(iri_type(op),A,A1).
xmle_axiom('DisjointDataProperties',disjointProperties(A),A1) :- maplist(iri_type(dp),A,A1).

xmle_axiom('InverseObjectProperties',inverseProperties(A, B),[op(A), op(B)]).

xmle_axiom('ObjectPropertyDomain',propertyDomain(A, B),[op(A), c(B)]).
xmle_axiom('DataPropertyDomain',propertyDomain(A, B),[dp(A), c(B)]).
xmle_axiom('ObjectPropertyRange',propertyRange(A, B),[op(A), c(B)]).
xmle_axiom('DataPropertyRange',propertyRange(A, B),[dp(A), c(B)]).

xmle_axiom('FunctionalObjectProperty',functionalProperty(A),[op(A)]).
xmle_axiom('FunctionalDataProperty',functionalProperty(A),[dp(A)]).

xmle_axiom('InverseFunctionalObjectProperty',inverseFunctionalProperty(A),[op(A)]).

xmle_axiom('ReflexiveObjectProperty',reflexiveProperty(A),[op(A)]).
xmle_axiom('IrreflexiveObjectProperty',irreflexiveProperty(A),[op(A)]).
xmle_axiom('SymmetricObjectProperty',symmetricProperty(A),[op(A)]).
xmle_axiom('AsymmetricObjectProperty',asymmetricProperty(A),[op(A)]).
xmle_axiom('TransitiveObjectProperty',transitiveProperty(A),[op(A)]).

% individual axioms
xmle_axiom('SameIndividual',sameIndividual(A),A1) :- maplist(iri_type(i),A,A1).
xmle_axiom('DifferentIndividuals',differentIndividuals(A),A1) :- maplist(iri_type(i),A,A1).
xmle_axiom('ClassAssertion',classAssertion(A, B),[c(A), i(B)]).
xmle_axiom('ObjectPropertyAssertion',propertyAssertion(A, B, C),[op(A), i(B), i(C)]).
xmle_axiom('DataPropertyAssertion',propertyAssertion(A, B, C),[dp(A), i(B), C]).
xmle_axiom('NegativeObjectPropertyAssertion',negativePropertyAssertion(A, B, C),[op(A), i(B), i(C)]).
xmle_axiom('NegativeDataPropertyAssertion',negativePropertyAssertion(A, B, C),[dp(A), i(B), C]).
xmle_axiom('AnnotationAssertion',annotationAssertion(A, B, C),[ap(A), i(B), C]).
%xmle_axiom('OntologyAnnotation',ontologyAnnotation(A, B, C),[A, B, C]).
%xmle_axiom('AxiomAnnotation',axiomAnnotation(A, B, C),[A, B, C]).
%xmle_axiom('AnnotationAnnotation',annotationAnnotation(A, B, C),[A, B, C]).

%xmle_axiom('Ontology',ontology(A),[A]).
%xmle_axiom('OntologyAxiom',ontologyAxiom(A, B),[A, B]).
%xmle_axiom('OntologyImport',ontologyImport(A, B),[A, B]).
%xmle_axiom('OntologyVersionInfo',ontologyVersionInfo(A, B),[A, B]).
%mle_axiom('Implies',implies(A, B),[A, B]).

iri_type(Type,Arg,TypedArg) :-
	TypedArg =.. [Type,Arg].

/** <module> Generation and parsing of OWL-XML from owl2_model

---+ Synopsis

Use this module via owl2_io.pl

==
:- use_module(library('thea/owl2_io')).
:- use_module(library('thea/owl2_model')).

test :-
        load_axioms('testfiles/rnao.owlx',owlx),
        forall(axiom(A),writeln(A)).
==

---+ Details

http://www.w3.org/TR/2008/WD-owl2-xml-serialization-20081202/

The XML syntax of OWL 2 corresponds closely to the structural
specification of OWL 2, so it is fully typed. It thus differs somewhat
from the functional-style syntax of OWL 2 [OWL 2 Specification]. For
example, whereas the functional-style syntax uses a nonterminal
SomeValuesFrom for existential restrictions on both the object and
data properties, the XML syntax provides two elements
owl:ObjectSomeValuesFrom and owl:DataSomeValuesFrom.

---++ Status

Alpha - use owl2_from_rdf.pl and OWL-RDF serializations for reliable parsing

---+ Additional Information

@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/
