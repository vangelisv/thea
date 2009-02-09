/* -*- Mode: Prolog -*- */

:- module(owl2_xml,
          [
           owl_parse_xml/1
          ]).

:- use_module(owl2_model).
:- use_module(library(sgml)).

owl_parse_xml(File) :-
        load_structure(File,[XML],[dialect(xmlns),space(remove)]),
        xml_ontology(XML,Ont,Axioms),
        assert_axiom(Ont),
        forall(member(Axiom,Axioms),
               assert_axiom(Axiom)),
        !.
owl_parse_xml(File) :-
        throw(no_parse(File)).

xml_ontology(element(_:'Ontology',Atts,Elts),ontology(O),Axioms) :-
        atts_iri(Atts,O),
        % TODO: imports
        elts_axioms(O,Elts,Axioms).

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

% TODO: this is P4 style
xml_annotation(element(_:'Annotation',Atts,Elts),annotation(P,V)) :-
        !,
        member(annotationURI=P,Atts),
        Elts=[element(_:'Constant',_,[V])].

% TODO: this is P4 style
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

xml_axiom(_Ont,element(_:Name,Atts,_),Axiom) :-
        xmle_entity(Name,Axiom,[IRI]),
        !,
        atts_iri(Atts,IRI).

xml_axiom(_Ont,element(_:Name,_Atts,Sub),Axiom) :-
        xmle_axiom(Name,Axiom,Args),
        !,
        maplist(xml_desc(Name),Sub,Args).

xml_axiom(Ont,Elt,_) :-
        throw(xml_axiom(Ont,Elt)).

        
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

atts_iri(Atts,IRI) :-
        iri_att(A),
        member(A=IRI,Atts).

iri_att('IRI').
iri_att('URI').  % TODO clarify. P4 uses this


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
xmle_entity('OWLClass',class(A),[A]).  % TODO: clarify. Protege4 exports this
xmle_entity('Datatype',datatype(A),[A]).
xmle_entity('ObjectProperty',objectProperty(A),[A]).
xmle_entity('DataProperty',dataProperty(A),[A]).
xmle_entity('AnnotationProperty',annotationProperty(A),[A]).
xmle_entity('NamedIndividual',namedIndividual(A),[A]).

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



/** <module> Generation and parsing of OWL-XML

  ---+ Synopsis

==
:- use_module(bio(owl2_xml)).

% 
demo:-
  nl.
  

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


---+ Additional Information

@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/
