/* -*- Mode: Prolog -*- */

:- module(owl2_xml,
          [
           owl_parse_xml/1,
           owl_parse_xml/2
          ]).

:- use_module(owl2_model).
:- use_module(owl2_metamodel).
:- use_module(owl2_io).
:- use_module(library(sgml)). % DEPENDENCY: SWI-Prolog
:- use_module(library('semweb/rdf_db')).


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

%% xml_ontology(+XML, ?Ont, ?Axioms:list) is det
% Translate XML term conforming to OWLX to a collecton of owl2_model axioms
xml_ontology(element(_:'Ontology',Atts,Elts),ontology(O),Axioms) :-
        atts_iri(Atts,O),
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
        throw(axiom(E,O)).

        
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
        maplist(xml_desc(Name),Sub,Args).

xml_axiom(Ont,Elt,_) :-
        throw(xml_axiom(Ont,Elt)).


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
        
xml_desc(Parent,Elt,_) :-
        throw(xml_desc(Elt,in(Parent))).

% TODO: this is P4 style
xml_annotation(element(_:'Annotation',Atts,Elts),annotation(P,V)) :-
        !,
        member(annotationURI=P,Atts),
        Elts=[element(_:'Constant',_,[V])].

atts_iri(Atts,IRI) :-
        iri_att(A),
        member(A=IRI,Atts).

iri_att('URI').  % TODO clarify. P4 uses this

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
        open(File,write,IO,[dialect(xml)]),
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
axioms_elts(O,[A|Axioms],[E|Elts]):-
        axiom_xml(O,A,E),
        !,
        axioms_elts(O,Axioms,Elts).
axioms_elts(O,[A|_],_) :-
        throw(axiom(A,O)).

% translate entity axiom to XML
axiom_xml(_Ont,Axiom,element('http://www.w3.org/2006/12/owl2-xml#':Name,['URI'=IRI],[])) :-
                                % TODO: annotations
        xmle_entity(Name,Axiom,[IRI]),
        !.
% translate full axiom to XML, translating description arguments also
axiom_xml(_Ont,Axiom,element('http://www.w3.org/2006/12/owl2-xml#':Name,[],Subs)) :-
        xmle_axiom(Name,Axiom,Args),
        !,
        debug(owl_exporter,'Axiom: ~w',[Axiom]),
        maplist(desc_xml(Name),Args,Subs).
axiom_xml(_Ont,Axiom,_) :-
        throw(axiom(Axiom)).

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
xmle_entity('Class',class(A),[A]).
%xmle_entity('OWLClass',class(A),[A]).  % TODO: clarify. Protege4 exports this
xmle_entity('Datatype',datatype(A),[A]).
xmle_entity('ObjectProperty',objectProperty(A),[A]).
xmle_entity('DataProperty',dataProperty(A),[A]).
xmle_entity('AnnotationProperty',annotationProperty(A),[A]).
xmle_entity('Individual',namedIndividual(A),[A]). % TODO - check
%xmle_entity('Individual',individual(A),[A]).

xmle_axiom('SubClassOf',subClassOf(A, B),[A, B]).
xmle_axiom('EquivalentClasses',equivalentClasses(A),A).
xmle_axiom('DisjointClasses',disjointClasses(A),A).
xmle_axiom('DisjointUnion',disjointUnion(A, B),[A, B]).

xmle_axiom('SubObjectPropertyOf',subPropertyOf(A, B),[A, B]).
xmle_axiom('SubDataPropertyOf',subPropertyOf(A, B),[A, B]).

xmle_axiom('EquivalentObjectProperties',equivalentProperties(A),A).
xmle_axiom('EquivalentDataProperties',equivalentProperties(A),A).

xmle_axiom('DisjointObjectProperties',disjointProperties(A),A).
xmle_axiom('DisjointDataProperties',disjointProperties(A),A).

xmle_axiom('InverseObjectProperties',inverseProperties(A, B),[A, B]).

xmle_axiom('ObjectPropertyDomain',propertyDomain(A, B),[A, B]).
xmle_axiom('DataPropertyDomain',propertyDomain(A, B),[A, B]).
xmle_axiom('ObjectPropertyRange',propertyRange(A, B),[A, B]).
xmle_axiom('DataPropertyRange',propertyRange(A, B),[A, B]).

xmle_axiom('FunctionalObjectProperty',functionalProperty(A),[A]).
xmle_axiom('FunctionalDataProperty',functionalProperty(A),[A]).

xmle_axiom('InverseFunctionalObjectProperty',inverseFunctionalProperty(A),[A]).

xmle_axiom('ReflexiveObjectProperty',reflexiveProperty(A),[A]).
xmle_axiom('IrreflexiveObjectProperty',irreflexiveProperty(A),[A]).
xmle_axiom('SymmetricObjectProperty',symmetricProperty(A),[A]).
xmle_axiom('AsymmetricObjectProperty',asymmetricProperty(A),[A]).
xmle_axiom('TransitiveObjectProperty',transitiveProperty(A),[A]).

% individual axioms
xmle_axiom('SameIndividual',sameIndividual(A),A).
xmle_axiom('DifferentIndividuals',differentIndividuals(A),A).
xmle_axiom('ClassAssertion',classAssertion(A, B),[A, B]).
xmle_axiom('ObjectPropertyAssertion',propertyAssertion(A, B, C),[A, B, C]).
xmle_axiom('DataPropertyAssertion',propertyAssertion(A, B, C),[A, B, C]).
xmle_axiom('NegativeObjectPropertyAssertion',negativePropertyAssertion(A, B, C),[A, B, C]).
xmle_axiom('NegativeDaraPropertyAssertion',negativePropertyAssertion(A, B, C),[A, B, C]).
xmle_axiom('AnnotationAssertion',annotationAssertion(A, B, C),[A, B, C]).
%xmle_axiom('OntologyAnnotation',ontologyAnnotation(A, B, C),[A, B, C]).
%xmle_axiom('AxiomAnnotation',axiomAnnotation(A, B, C),[A, B, C]).
%xmle_axiom('AnnotationAnnotation',annotationAnnotation(A, B, C),[A, B, C]).

%xmle_axiom('Ontology',ontology(A),[A]).
%xmle_axiom('OntologyAxiom',ontologyAxiom(A, B),[A, B]).
%xmle_axiom('OntologyImport',ontologyImport(A, B),[A, B]).
%xmle_axiom('OntologyVersionInfo',ontologyVersionInfo(A, B),[A, B]).
%mle_axiom('Implies',implies(A, B),[A, B]).



/** <module> Generation and parsing of OWL-XML from owl2_model

---+ Synopsis


---+ Details

http://www.w3.org/TR/2008/WD-owl2-xml-serialization-20081202/

The XML syntax of OWL 2 corresponds closely to the structural
specification of OWL 2, so it is fully typed. It thus differs somewhat
from the functional-style syntax of OWL 2 [OWL 2 Specification]. For
example, whereas the functional-style syntax uses a nonterminal
SomeValuesFrom for existential restrictions on both the object and
data properties, the XML syntax provides two elements
owl:ObjectSomeValuesFrom and owl:DataSomeValuesFrom.


---+ Additional Information

@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/
