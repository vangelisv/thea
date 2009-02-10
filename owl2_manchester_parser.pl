/* -*- Mode: Prolog -*- */

:- module(owl2_manchester_parser,[]).

tokens_ontology(Toks,Ont) :-
        ontologyDocument( Ont, Toks ).

manchester_atom_ontology(A,Ont) :-
        tokenize(A, Toks),
        ontologyDocument( Ont, Toks ).

% ----------------------------------------
% Tokenization
% ----------------------------------------

% Documents in the Manchester OWL syntax consist of sequences of Unicode characters [UNICODE] and are encoded in UTF-8 [RFC3829].

% The grammar for the Manchester syntax does not explicitly show white
% space. White space is allowed between any two terminals or
% non-terminals except inside nonNegativeInteger, prefix, reference,
% full-IRI, lexicalValue, integerLiteral, decimalLiteral,
% floatingPointLiteral, and languageTag. White space is required
% between two terminals or non-terminals if its removal could cause
% ambiguity. Generally this means requiring white space except before
% and after punctuation (e.g., commas, parentheses, braces, and
% brackets).

% White space is a sequence of blanks (U+20), tabs (U+9), line feeds
% (U+A), carriage returns (U+D), and comments. Comments are maximal
% sequences of Unicode characters starting with a '#' and not
% containing a line feed or a carriage return. Note that comments are
% only recognized where white space is allowed, and thus not inside
% the above non-terminals.

% The syntax uses the keywords 'and', 'or', and 'not', which are used
% in descriptions, that can be confused with their use as IRIs. When
% there is an ambiguity the keyword use is to be used.

% TODO!



% ----------------------------------------
% Meta-productions
% ----------------------------------------

% <NT>List ::= <NT> { , <NT> }
%% zeroOrMore( +F, ?List, +InToks, ?Rest )
zeroOrMore(F,[X|L],In,Rest):-
        Head =.. [F,X,In,Rest1],
        Head,
        !,
        zeroOrMore(F,L,Rest1,Rest).
zeroOrMore(_,[],In,In):- !.

% e.g. zeroOrMore(or,description,L)
zeroOrMore(Delim,F,[X|L],[Delim|In],Rest):-
        Head =.. [F,X,In,Rest1],
        Head,
        !,
        zeroOrMore(Delim,F,L,Rest1,Rest).
zeroOrMore(_,_,[],In,In):- !.

% <NT>2List ::= <NT> , <NT>List
oneOrMore(F,[X|L],In,Rest):-
        Head =.. [F,X,In,Rest1],
        Head,
        !,
        zeroOrMore(F,L,Rest1,Rest).

oneOrMore(Delim,F,L,In,Rest):-
        zeroOrMore(Delim,F,L,In,Rest),
        L\=[].

        
% <NT>AnnotatedList ::= [annotations] <NT> { , [annotations]<NT> }

% ----------------------------------------
% 2.1 IRIs, Integers, Literals, and Entities
% ----------------------------------------

% full-IRI := 'IRI as defined in [RFC3987], enclosed in a pair of < (U+3C) and > (U+3E) characters' 
full_IRI(X) --> [X],{X\='('}. % TODO
% NCName := 'as defined in [XML Namespaces]'
% TODO

% irelative-ref := 'as defined in [RFC3987]'
% TODO

% namespace := full-IRI
namespace(X) --> full_IRI(X).

% prefix := NCName reference := irelative-ref
% curie := [ [ prefix ] ':' ] reference
curie(X) --> [X],{X\='('}.
% TODO

% IRI := full-IRI | curie 
iri(X) --> full_IRI(X).
iri(X) --> curie(X).

% nonNegativeInteger ::= zero | positiveInteger positiveInteger ::= nonZero { digit } digits ::= digit { digit } digit ::= zero | nonZero nonZero := '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' zero ::= '0' 
% TODO

% classIRI ::= IRI
classIRI(X) --> iri(X).

% Datatype ::= datatypeIRI | 'integer' | 'decimal' | 'float' | 'string'
datatype(X) --> datatypeIRI(X).
datatype(integer) --> [integer].
datatype(decimal) --> [decimal].
datatype(float) --> [float].
datatype(string) --> [string].

% datatypeIRI ::= IRI
datatypeIRI(X) --> iri(X).

% objectPropertyIRI ::= IRI
objectPropertyIRI(X) --> iri(X).

% dataPropertyIRI ::= IRI
dataPropertyIRI(X) --> iri(X).

% annotationPropertyIRI ::= IRI
annotationProperty(X) --> iri(X).

% individual ::= individualIRI | nodeID
individual(X) --> individualIRI(X) ; nodeID(X).

% individualIRI ::= IRI 
individualIRI(X) --> iri(X).

% nodeID := 'a node ID of the form _:name as specified in the N-Triples specification [RDF Test Cases]' 
nodeID(X) --> [X]. % TODO

% all the following are TODO

% literal ::= typedLiteral | abbreviatedXSDStringLiteral | abbreviatedRDFTextLiteral | integerLiteral | decimalLiteral | floatingPointLiteral

% typedLiteral ::= lexicalValue '^^' Datatype

% abbreviatedXSDStringLiteral ::= quotedString

% abbreviatedRDFTextLiteral ::= quotedString '@' languageTag languageTag := 'a nonempty (not quoted) string defined as specified in BCP 47 [BCP 47]'

% lexicalValue ::= quotedString quotedString := 'a finite sequence of characters in which " (U+22) and \ (U+5C) occur only in pairs of the form \" (U+22, U+5C) and \\ (U+22, U+22), enclosed in a pair of " (U+22) characters'

% floatingPointLiteral ::= [ '+' | '-'] ( digits ['.'digits] [exponent] | '.' digits[exponent]) ( 'f' | 'F' )

% exponent ::= ('e' | 'E') ['+' | '-'] digits

% decimalLiteral ::= ['+' | '-'] digits '.' digits

% integerLiteral ::= ['+' | '-'] digits

% entity ::= 'Datatype' '(' datatypeIRI ')' | 'Class' '(' classIRI ')' | 'ObjectProperty' '(' objectPropertyIRI ')' | 'DataProperty' '('dataPropertyIRI ')' | 'AnnotationProperty' '(' annotationPropertyIRI ')' | 'NamedIndividual' '(' individualIRI ')' 

% ----------------------------------------
%  2.2 Ontologies and Annotations
% ----------------------------------------

% annotations ::= 'Annotations:' annotationAnnotatedList
annotations(L) --> ['Annotations:'], annotationAnnotatedList(L).

% annotation ::= annotationPropertyIRI annotationTarget
annotation(A-T) -->  annotationPropertyIRI( P),annotationTarget(T).


% annotationTarget ::= nodeID | IRI | literal
annotationTarget(T) --> nodeID(T) ; iri(T) ; literal(T).

% ontologyDocument ::= { namespace } ontology
ontologyDocument(D) --> zeroOrMore(namespace,NSL),ontology(O).

% namespace ::= 'Namespace:' [ prefix ] full-IRI
namespace(NS) --> ['Namespace:'], prefix( P), full_IRI(IRI).
namespace(NS) --> ['Namespace:'], full_IRI(IRI).

% ontology ::= 'Ontology:' [ ontologyIRI [ versionIRI ] ] { import } { annotations } { frame }
ontology(ontology(X,Imports,AL,Frames)) --> ['Ontology:'], ontologyIRI( X), zeroOrMore(import,Imports),zeroOrMore(annotations,AL),zeroOrMore(frame,Frames).
ontology(ontology(Imports,AL,Frames)) --> ['Ontology:'], zeroOrMore(import,Imports),zeroOrMore(annotations,AL),zeroOrMore(frame,Frames).
ontologyIRI(X-V) --> iri(X),versionIRI(V). % TODO
ontologyIRI(X) --> iri(X).

% ontologyIRI ::= IRI
ontologyIRI(X) --> iri(X).

% versionIRI ::= IRI
versionIRI( X) --> iri(X).

% import ::= 'Import:' IRI
import(X) --> ['Import:'], iri(X).


% frame ::= classFrame | objectPropertyFrame | dataPropertyFrame |
% annotationPropertyFrame | individualFrame | misc

frame(X) --> classFrame(X) ; objectPropertyFrame(X) ; dataPropertyFrame(X) ;
 annotationPropertyFrame(X) ; individualFrame(X) ; misc(X).

% ----------------------------------------
%      2.3 Property and Datatype Expressions
% ----------------------------------------

% objectPropertyExpression ::= objectPropertyIRI | inverseObjectProperty
objectPropertyExpression(OPE) --> objectPropertyIRI(OPE) ;inverseObjectProperty(OPE).

% inverseObjectProperty ::= 'inverse' objectPropertyIRI
inverseObjectProperty(inverseOf(P)) --> ['inverse'], objectPropertyIRI(P).

% dataPropertyExpression ::= dataPropertyIRI
dataPropertyExpression(P) --> dataPropertyIRI(P).

% dataRange ::= dataConjunction 'or' dataConjunction { 'or' dataConjunction } | dataConjunction
dataRange(unionOf([C1,C2|CL])) --> dataConjunction(C1),[or],dataConjunction(C2),zeroOrMore(or,dataConjunction(CL)).
dataRange(DR) --> dataConjunction(DR).

%dataConjunction ::= dataPrimary 'and' dataPrimary { 'and' dataPrimary } | dataPrimary
dataConjunction(intersectionOf([D1,D2|DL])) --> primary(D1),[and],primary(D2),zeroOrMore(primary,DL).
dataConjunction(D) --> primary(D).

% dataPrimary ::= [ 'not' ] dataAtomic
dataPrimary(complementOf(A)) --> dataAtomic(A).
dataPrimary(A) --> dataAtomic(A).

% dataAtomic ::= Datatype | '{' literal { ',' literal } '}' |
% datatypeRestriction | '(' dataRange ')'
dataAtomic(A) --> datatype(A).
dataAtomic([X1|XL]) --> ['{'],literal(X1),zeroOrMore(',',literal,XL),['}'].
dataAtomic(A) --> datatypeRestriction(A).
dataAtomic(A) --> ['('],dataRange(A),[')'].

% datatypeRestriction ::= Datatype '[' facet restrictionValue { ',' facet restrictionValue } ']'
datatypeRestriction(foo) --> datatype(X),['['],facet( F),restrictionValue(V), zeroOrMore(',',facetRestrictionValue(FVs)),[']'].
facetRestrictionValue(F-V) --> facet(F),restrictionValue(V).
                                                                                        
% facet ::= 'length' | 'minLength' | 'maxLength' | 'pattern' | 'langPattern' | '<=' | '<' | '>=' | '>'
% facet ::= 'length' | 'minLength' | 'maxLength' | 'pattern' | 'langPattern' | '<=' | '<' | '>=' | '>'

% restrictionValue ::= literal
restrictionValue(V) --> literal(V).


% ----------------------------------------
%      2.4 Descriptions
% ----------------------------------------

% description ::= conjunction 'or' conjunction { 'or' conjunction } | conjunction
description(unionOf([C|CL])) --> conjunction(C), oneOrMore(or,conjunction,CL).
description(D) --> conjunction(D).

% conjunction ::= classIRI 'that' [ 'not' ] restriction { 'and' [ 'not' ] restriction } | primary 'and' primary { 'and' primary }| primary
conjunction(intersectionOf([Genus,D|DL])) --> classIRI(Genus), [that], optNegRestriction(D), zeroOrMore(and, optNegRestriction,DL).
conjunction(intersectionOf([D1,D2|DL])) --> primary(D1),[and],primary(D2),zeroOrMore(primary,DL).
conjunction(D) --> primary(D).

% primary ::= [ 'not' ] ( restriction | atomic )
primary(R) --> optNegRestriction(R).
primary(complementOf(A)) --> [not],atomic(A).
primary(A) --> atomic(A).

optNegRestriction(complementOf(R)) --> [not],restriction(R).
optNegRestriction(R) --> restriction(R).

% restriction ::= objectPropertyExpression 'some' primary |
% objectPropertyExpression 'only' primary | objectPropertyExpression
% 'value' individual | objectPropertyExpression 'Self' |
% objectPropertyExpression 'min' nonNegativeInteger [ primary ] |
% objectPropertyExpression 'max' nonNegativeInteger [ primary ] |
% objectPropertyExpression 'exactly' nonNegativeInteger [ primary ] |
% dataPropertyExpression 'some' dataPrimary | dataPropertyExpression
% 'only' dataPrimary | dataPropertyExpression 'value' literal |
% dataPropertyExpression 'min' nonNegativeInteger [ dataPrimary ] |
% dataPropertyExpression 'max' nonNegativeInteger [ dataPrimary ] |
% dataPropertyExpression 'exactly' nonNegativeInteger [ dataPrimary ]

restriction(someValuesFrom(OPE,C)) --> objectPropertyExpression(OPE),[some],primary(C).
restriction(onlyValuesFrom(OPE,C)) --> objectPropertyExpression(OPE),[only],primary(C).
restriction(hasValue(OPE,C)) --> objectPropertyExpression(OPE),[value],individual(I).
restriction(hasSelf(OPE,C)) --> objectPropertyExpression(OPE),['Self'].
restriction(minCardinality(OPE,Card,CE)) --> objectPropertyExpression(OPE),[min],nni(Card),primary(CE).
restriction(minCardinality(OPE,Card)) --> objectPropertyExpression(OPE),[min],nni(Card).
restriction(maxCardinality(OPE,Card,CE)) --> objectPropertyExpression(OPE),[max],nni(Card),primary(CE).
restriction(maxCardinality(OPE,Card)) --> objectPropertyExpression(OPE),[max],nni(Card).
restriction(exactCardinality(OPE,Card,CE)) --> objectPropertyExpression(OPE),[exact],nni(Card),primary(CE).
restriction(exactCardinality(OPE,Card)) --> objectPropertyExpression(OPE),[exact],nni(Card).

restriction(someValuesFrom(OPE,C)) --> dataPropertyExpression(OPE),[some],dataPrimary(C).
restriction(onlyValuesFrom(OPE,C)) --> dataPropertyExpression(OPE),[only],dataPrimary(C).
restriction(hasValue(OPE,C)) --> dataPropertyExpression(OPE),[value],individual(I).
restriction(hasSelf(OPE,C)) --> dataPropertyExpression(OPE),['Self'].
restriction(minCardinality(OPE,Card,CE)) --> dataPropertyExpression(OPE),[min],nni(Card),dataPrimary(CE).
restriction(minCardinality(OPE,Card)) --> dataPropertyExpression(OPE),[min],nni(Card).
restriction(maxCardinality(OPE,Card,CE)) --> dataPropertyExpression(OPE),[max],nni(Card),dataPrimary(CE).
restriction(maxCardinality(OPE,Card)) --> dataPropertyExpression(OPE),[max],nni(Card).
restriction(exactCardinality(OPE,Card,CE)) --> dataPropertyExpression(OPE),[exact],nni(Card),dataPrimary(CE).
restriction(exactCardinality(OPE,Card)) --> dataPropertyExpression(OPE),[exact],nni(Card).


% atomic ::= classIRI | '{' individual { ',' individual } '}' | '(' description ')'
atomic(X) --> classIRI(X).
atomic([X|L]) --> ['{'],individual(X),zeroOrMore(',',individual,L),['}'].
atomic(X) --> ['('], description(X), [')'].

% <NT>AnnotatedList ::= [annotations] <NT> { , [annotations] <NT> }
annotationAnnotatedList([A|L]) --> annotatedAnnotation(A),zeroOrMoreA(',',annotatedAnnotation,L). % TODO
descriptionAnnotatedList([D|L]) --> annotatedDescription(D),zeroOrMore(',',description,L). % TODO
objectPropertyCharacteristicAnnotatedList([PC|L]) --> annotatedbjectPropertyCharacteristic(PC),zeroOrMore(',',annotatedObjectPropertyCharacteristic,L). % TODO

% <NT>2List ::= <NT> , <NT>List
description2List([D|L]) --> description(D),[','],oneOrMore(',',description,L).

annotatedDescription(A-D) --> annotations(A),description(D).
annotatedDescription(D) --> description(D).

% ----------------------------------------
%  2.5 Frames and Miscellaneous
% ----------------------------------------

% classFrame ::= 'Class:' classIRI { 'Annotations:'
% annotationAnnotatedList | 'SubClassOf:' descriptionAnnotatedList |
% 'EquivalentTo:' descriptionAnnotatedList | 'DisjointWith:'
% descriptionAnnotatedList | 'DisjointUnionOf:' annotations
% description2List }

classFrame(C-EL) --> ['Class:'],classIRI(C),zeroOrMore(classFrameElement,EL).
classFrameElement(annotation=AL) --> ['Annotations:'],annotationAnnotatedList(AL).
classFrameElement(subClassOf=AL) --> ['SubClassOf:'],descriptionAnnotatedList(AL).
classFrameElement(equivalentTo=AL) --> ['EquivalentTo:'],descriptionAnnotatedList(AL).
classFrameElement(disjointWith=AL) --> ['DisjointWith:'],descriptionAnnotatedList(AL).
classFrameElement(disjointUnionOf=AL) --> ['DisjointUnionOf:'],descriptionAnnotatedList(AL).


% objectPropertyFrame ::= 'ObjectProperty:' objectPropertyIRI {
%'Annotations:' annotationAnnotatedList | 'Domain:'
%descriptionAnnotatedList | 'Range:' descriptionAnnotatedList |
%'Characteristics:' objectPropertyCharacteristicAnnotatedList |
%'SubPropertyOf:' objectPropertyExpressionAnnotatedList |
%'EquivalentTo:' objectPropertyExpressionAnnotatedList |
%'DisjointWith:' objectPropertyExpressionAnnotatedList | 'InverseOf:'
%objectPropertyExpressionAnnotatedList | 'SubPropertyChain:'
%annotations objectPropertyExpression 'o' objectPropertyExpression {
%'o' objectPropertyExpression } }

objectPropertyFrame(C-EL) --> ['ObjectProperty:'],objectPropertyIRI(C),zeroOrMore(objectPropertyFrameElement,EL).
objectPropertyFrameElement(domain=AL) --> ['Domain:'],descriptionAnnotatedList(AL).
objectPropertyFrameElement(range=AL) --> ['Range:'],descriptionAnnotatedList(AL).
objectPropertyFrameElement(characteristics=AL) --> ['Characteristics:'],objectPropertyCharacteristicAnnotatedList(AL).
objectPropertyFrameElement(subPropertyOf=AL) --> ['SubPropertyOf:'],objectPropertyExpressionAnnotatedList(AL).
objectPropertyFrameElement(equivalentTo=AL) --> ['EquivalentTo:'],objectPropertyExpressionAnnotatedList(AL).
objectPropertyFrameElement(disjointWith=AL) --> ['DisjointWith:'],objectPropertyExpressionAnnotatedList(AL).
objectPropertyFrameElement(inverseOf=AL) --> ['InverseOf:'],objectPropertyExpressionAnnotatedList(AL).
objectPropertyFrameElement(subPropertyChain=[As,OPE1,OPE2|OPEs]) --> ['SubPropertyChain:'],annotations(As),
        objectPropertyExpression(OPE1),[o],objectPropertyExpression(OPE2),[o],zeroOrMore(o,objectPropertyExpression,OPEs).

% objectPropertyCharacteristic ::= 'Functional' | 'InverseFunctional'
%| 'Reflexive' | 'Irreflexive' | 'Symmetric' | 'Asymmetric' |
%'Transitive'
objectPropertyCharacteristic(P) --> [P],{objectPropertyCharacteristic(P)}.

objectPropertyCharacteristic('Functional').
objectPropertyCharacteristic('InverseFunctional').
objectPropertyCharacteristic('Reflexive').
objectPropertyCharacteristic('Irreflexive').
objectPropertyCharacteristic('Symmetric').
objectPropertyCharacteristic('Asymmetric').
objectPropertyCharacteristic('Transitive').

% dataPropertyFrame ::= 'DataProperty:' dataPropertyIRI {
% 'Annotations:' annotationAnnotatedList | 'Domain:'
% descriptionAnnotatedList | 'Range:' dataRangeAnnotatedList |
% 'Characteristics:' annotations 'Functional' | 'SubPropertyOf:'
% dataPropertyExpressionAnnotatedList | 'EquivalentTo:'
% dataPropertyExpressionAnnotatedList | 'DisjointWith:'
% dataPropertyExpressionAnnotatedList }

dataPropertyFrame(C-EL) --> ['DataProperty:'],dataPropertyIRI(C),zeroOrMore(dataPropertyFrameElement,EL).
dataPropertyFrameElement(domain=AL) --> ['Domain:'],descriptionAnnotatedList(AL).
dataPropertyFrameElement(range=AL) --> ['Range:'],descriptionAnnotatedList(AL).
dataPropertyFrameElement(characteristics=AL) --> ['Characteristics:'],dataPropertyCharacteristicAnnotatedList(AL).
dataPropertyFrameElement(subPropertyOf=AL) --> ['SubPropertyOf:'],dataPropertyExpressionAnnotatedList(AL).
dataPropertyFrameElement(equivalentTo=AL) --> ['EquivalentTo:'],dataPropertyExpressionAnnotatedList(AL).
dataPropertyFrameElement(disjointWith=AL) --> ['DisjointWith:'],dataPropertyExpressionAnnotatedList(AL).

dataPropertyCharacteristic(P) --> [P],{dataPropertyCharacteristic(P)}.
dataPropertyCharacteristic('Functional').

% annotationPropertyFrame ::= 'AnnotationProperty:'
%annotationPropertyIRI { 'Annotations:' annotationAnnotatedList } |
%'Domain:' IRIAnnotatedList | 'Range:' IRIAnnotatedList |
%'SubPropertyOf:' annotationPropertyIRIAnnotatedList

annotationPropertyFrame(C-EL) --> ['AnnotationProperty:'],annotationPropertyIRI(C),zeroOrMore(annotationPropertyFrameElement,EL).
annotationPropertyFrameElement(domain=AL) --> ['Domain:'],descriptionAnnotatedList(AL).
annotationPropertyFrameElement(range=AL) --> ['Range:'],descriptionAnnotatedList(AL).
annotationPropertyFrameElement(subPropertyOf=AL) --> ['SubPropertyOf:'],annotationPropertyExpressionAnnotatedList(AL).


% individualFrame ::= 'Individual:' individual { 'Annotations:'
%annotationAnnotatedList | 'Types:' descriptionAnnotatedList |
%'Facts:' factAnnotatedList | 'SameAs:' individualAnnotatedList |
%'DifferentFrom:' individualAnnotatedList }
individualFrame(I-EL) --> ['Individual:'], individual(I),zeroOrMore(individualFrameElement,EL).
classFrameElement(annotation=AL) --> ['Annotations:'],annotationAnnotatedList(AL).
classFrameElement(types=AL) --> ['Types:'],descriptionAnnotatedList(AL).
classFrameElement(facts=AL) --> ['Facts:'],factAnnotatedList(AL).
classFrameElement(sameAs=AL) --> ['SameAs:'],individualAnnotatedList(AL).
classFrameElement(differentFrom=AL) --> ['DifferentFrom:'],individualAnnotatedList(AL).

% fact ::= [ 'not' ] (objectPropertyFact | dataPropertyFact)
fact(not(F)) --> [not],(objectPropertyFact(F) ;  dataPropertyFact(F)).
fact(F) --> (objectPropertyFact(F) ;  dataPropertyFact(F)).


% objectPropertyFact ::= objectPropertyIRI individual
objectPropertyFact(P-I) --> objectPropertyIRI(P),individual(I).

% dataPropertyFact ::= dataPropertyIRI literal
dataPropertyFact( P-L) --> dataPropertyIRI( P), literal(L).

% TODO
% misc ::= 'EquivalentClasses:' annotations description2List | 'DisjointClasses:'
%annotations description2List | 'EquivalentProperties:' annotations
%objectProperty2List | 'DisjointProperties:' annotations
%objectProperty2List | 'EquivalentProperties:' annotations
%dataProperty2List | 'DisjointProperties:' annotations dataProperty2List
%| 'SameIndividual:' annotations individual2List |
%'DifferentIndividuals:' annotations individual2List | 'HasKey:'
%description annotations ( objectPropertyExpression |
%dataPropertyExpression ) { objectPropertyExpression |
%dataPropertyExpression }

misc(A-DL) --> ['EquivalentClasses:'],annotations(A),description2List(DL).



/** <module> 

  ---+ Synopsis

==
==

---+ Details

http://www.w3.org/TR/owl2-manchester-syntax/

---+ Additional Information

@author  Chris Mungall
@version $Revision$
@see     README
@license License


*/
