/* -*- Mode: Prolog -*- */

:- module(owl2_manchester_parser,
          [
           owl_parse_manchester_syntax_file/1,
           owl_parse_manchester_syntax_file/2,
	   owl_parse_manchester_expression/2,
	   owl_parse_manchester_frame/2
           ]).

:- use_module(owl2_model,[assert_axiom/1]).

:- multifile owl2_io:load_axioms_hook/3.
owl2_io:load_axioms_hook(File,mansyn,Opts) :-
        owl_parse_manchester_syntax_file(File,Opts).
owl2_io:load_axioms_hook(File,owlms,Opts) :-
        owl_parse_manchester_syntax_file(File,Opts).

owl_parse_manchester_syntax_file(File) :-
        owl_parse_manchester_syntax_file(File,[]).

owl_parse_manchester_syntax_file(File,_Opts) :-
        read_file_to_codes(File,Codes,[]),
	codes_tokens_filtered(Codes,Tokens),
	ontologyDocument( Ont, Tokens, [] ),
	process_ontdoc(Ont).

%% owl_parse_manchester_expression(+DescAtom,?Desc) is semidet
owl_parse_manchester_expression(A,X) :-
	atom_codes(A,L),
	codes_tokens_filtered(L,Toks),
	description(X,Toks,[]).

%% owl_parse_manchester_frame(+FrameAtom,?Axiom) is semidet
owl_parse_manchester_frame(A,Axioms) :-
	atom_codes(A,L),
	codes_tokens_filtered(L,Toks),
	frame(X,Toks,[]),
	process_frame(X,'',Axioms).

process_ontdoc( NSL-ontology(O,_L1,_L2,Frames) ) :-
	process_frames(Frames,O-NSL,Axioms),
	maplist(assert_axiom,Axioms).

process_frames([],_,[]).
process_frames([F|Fs],O,Axioms) :-
	!,
	process_frame(F,O,Axioms1),
	process_frames(Fs,O,Axioms2),
	append(Axioms1,Axioms2,Axioms).

process_frame(frame(Type,Name,Props),O,[Unary|Axioms]) :-
	!,
	expand_curie(Name,O,IRI),
	Unary =.. [Type,IRI],
	assert_axiom(Unary),
	process_properties(Props,IRI,Type,O,Axioms).
process_frame(A,_,[A]).

process_properties([],_,_,_,[]).
process_properties([Prop|Props],IRI,Type,O,[Axiom|Axioms]) :-
	process_property(Prop,IRI,Type,O,Axioms1),
	process_properties(Props,IRI,Type,O,Axioms2),
	append(Axioms1,Axioms2,Axioms).

process_property(characteristics=CL,IRI,Type,O,Axioms) :-
	!,
	findall(Axiom,(member(C,CL),
		       process_characteristic(C,IRI,Type,O,Axiom)),
		Axioms).

process_property(P=VL,IRI,Type,O,Axioms) :-
	!,
	findall(Axiom,(member(V,VL),
		       process_slot_value(P,V,IRI,Type,O,Axiom)),
		Axioms).

process_characteristic(C,IRI,_Type,_,Unary) :-
	slot_predicate(C,P),
	Unary =.. [P,IRI].
	%writeln(u=Unary),
	%assert_axiom(Unary).

process_slot_value(S,V,IRI,T,O,Ax) :-
	process_slot_value(S,V,IRI,T,O,Ax).
	%writeln(ax=Ax),
	%assert_axiom(Ax).
	
process_slot_value(disjointWith,V,IRI,objectProperty,O,Ax) :-
	!,
	expand_curie(V,O,VX),
	Ax =.. [disjointProperties,[IRI,VX]].
process_slot_value(inverseOf,V,IRI,_,O,Ax) :-
	!,
	expand_curie(V,O,VX),
	Ax =.. [inverseProperties,IRI,VX].
process_slot_value(S,V,IRI,T,O,Ax) :-
	writeln(eh(S,T)),
	expand_curie(V,O,VX),
	Ax =.. [S,IRI,VX].

expand_curie(Name,O-_NSL,IRI) :-
	concat_atom([O,'#',Name],IRI). % TODO

slot_predicate(S,P) :-
	sub_atom(S,0,1,_,C1),
	C1 @>= 'A',
	C1 @=< 'Z',
	!,
	downcase_atom(C1,C2),
	sub_atom(S,1,_,0,S2),
	atom_concat(C2,S2,P).
slot_predicate(S,S).

% ----------------------------------------
% Tokenization
% ----------------------------------------

codes_tokens_filtered(Codes,Tokens2) :-
	codes_tokens(Codes,[],Tokens),
	findall(T,(member(T,Tokens),T\=''),Tokens2).

codes_tokens([],Buf,[A]) :-
	!,
	atom_rcodes(A,Buf).
codes_tokens([C|Cs],Buf,[A|Toks]) :-
	ws(C),
	!,
	atom_rcodes(A,Buf),
	codes_tokens(Cs,[],Toks).
codes_tokens([C|Cs],Buf,[A1,A2|Toks]) :-
	sep(C),
	!,
	atom_rcodes(A1,Buf),
	atom_codes(A2,[C]),
	codes_tokens(Cs,[],Toks).
codes_tokens([C|Cs],Buf,Toks) :-
	codes_tokens(Cs,[C|Buf],Toks).

atom_rcodes(A,L) :-
	reverse(L,RL),
	atom_codes(A,RL).

ws(0' ).
ws(0'\n).
ws(0'\r).
sep(0',).
sep(0'().
sep(0')).

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
full_IRI(IRI) --> [X],{atom_concat('<',Y,X),atom_concat(IRI,'>',Y)}.

% NCName := 'as defined in [XML Namespaces]'
% TODO

% irelative-ref := 'as defined in [RFC3987]'
% TODO

% namespace := full-IRI
% ?namespace(X) --> full_IRI(X).

% prefix := NCName

prefix(P) --> [P].

% reference := irelative-ref

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
annotation(P-T) -->  annotationPropertyIRI( P),annotationTarget(T).


% annotationTarget ::= nodeID | IRI | literal
annotationTarget(T) --> nodeID(T) ; iri(T) ; literal(T).

% ontologyDocument ::= { namespace } ontology
ontologyDocument( NSL-O ) --> zeroOrMore(namespace,NSL),ontology(O).

% namespace ::= 'Namespace:' [ prefix ] full-IRI
namespace(P-NS) --> ['Namespace:'], prefix( P), full_IRI(NS).
namespace(NS) --> ['Namespace:'], full_IRI(NS).

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
datatypeRestriction(X-F-V-FVs) --> datatype(X),['['],facet( F),restrictionValue(V), zeroOrMore(',',facetRestrictionValue(FVs)),[']'].
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
conjunction(intersectionOf([D|DL])) --> primary(D),oneOrMore('and',conjunction,DL).
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
restriction(allValuesFrom(OPE,C)) --> objectPropertyExpression(OPE),[only],primary(C).
restriction(hasValue(OPE,I)) --> objectPropertyExpression(OPE),[value],individual(I).
restriction(hasSelf(OPE)) --> objectPropertyExpression(OPE),['Self'].
restriction(minCardinality(Card,OPE,CE)) --> objectPropertyExpression(OPE),[min],nni(Card),primary(CE).
restriction(minCardinality(Card,OPE)) --> objectPropertyExpression(OPE),[min],nni(Card).
restriction(maxCardinality(Card,OPE,CE)) --> objectPropertyExpression(OPE),[max],nni(Card),primary(CE).
restriction(maxCardinality(Card,OPE)) --> objectPropertyExpression(OPE),[max],nni(Card).
restriction(exactCardinality(Card,OPE,CE)) --> objectPropertyExpression(OPE),[exactly],nni(Card),primary(CE).
restriction(exactCardinality(Card,OPE)) --> objectPropertyExpression(OPE),[exactly],nni(Card).

restriction(someValuesFrom(OPE,C)) --> dataPropertyExpression(OPE),[some],dataPrimary(C).
restriction(allValuesFrom(OPE,C)) --> dataPropertyExpression(OPE),[only],dataPrimary(C).
restriction(hasValue(OPE,I)) --> dataPropertyExpression(OPE),[value],individual(I).
restriction(hasSelf(OPE)) --> dataPropertyExpression(OPE),['Self'].
restriction(minCardinality(Card,OPE,CE)) --> dataPropertyExpression(OPE),[min],nni(Card),dataPrimary(CE).
restriction(minCardinality(Card,OPE)) --> dataPropertyExpression(OPE),[min],nni(Card).
restriction(maxCardinality(Card,OPE,CE)) --> dataPropertyExpression(OPE),[max],nni(Card),dataPrimary(CE).
restriction(maxCardinality(Card,OPE)) --> dataPropertyExpression(OPE),[max],nni(Card).
restriction(exactCardinality(Card,OPE,CE)) --> dataPropertyExpression(OPE),[exactly],nni(Card),dataPrimary(CE).
restriction(exactCardinality(Card,OPE)) --> dataPropertyExpression(OPE),[exactly],nni(Card).

nni(N) --> [A],{atom_number(A,N)}.


% atomic ::= classIRI | '{' individual { ',' individual } '}' | '(' description ')'
atomic(X) --> classIRI(X).
atomic([X|L]) --> ['{'],individual(X),zeroOrMore(',',individual,L),['}'].
atomic(X) --> ['('], description(X), [')'].

% <NT>AnnotatedList ::= [annotations] <NT> { , [annotations] <NT> }
annotationAnnotatedList([A|L]) --> annotatedAnnotation(A),zeroOrMoreA(',',annotatedAnnotation,L). % TODO
descriptionAnnotatedList([D|L]) --> annotatedDescription(D),zeroOrMore(',',description,L). % TODO
objectPropertyCharacteristicAnnotatedList([PC|L]) --> annotatedObjectPropertyCharacteristic(PC),zeroOrMore(',',annotatedObjectPropertyCharacteristic,L).
objectPropertyExpressionAnnotatedList([PC|L]) --> annotatedObjectPropertyExpression(PC),zeroOrMore(',',annotatedObjectPropertyExpression,L).

annotatedObjectPropertyCharacteristic(X) --> objectPropertyCharacteristic(X). % TODO
annotatedObjectPropertyExpression(X) --> objectPropertyExpression(X). % TODO


% <NT>2List ::= <NT> , <NT>List
description2List([D|L]) --> description(D),zeroOrMore(',',description,L).

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

classFrame( frame(class,C,EL) ) --> ['Class:'],classIRI(C),zeroOrMore(classFrameElement,EL).
classFrameElement(annotation=AL) --> ['Annotations:'],annotationAnnotatedList(AL).
classFrameElement(subClassOf=AL) --> ['SubClassOf:'],descriptionAnnotatedList(AL).
classFrameElement(equivalentTo=AL) --> ['EquivalentTo:'],descriptionAnnotatedList(AL).
classFrameElement(disjointWith=AL) --> ['DisjointWith:'],descriptionAnnotatedList(AL).
classFrameElement(disjointUnionOf=AL) --> ['DisjointUnionOf:'],descriptionAnnotatedList(AL).

% ?
classFrameElement(annotation=AL) --> ['Annotations:'],annotationAnnotatedList(AL).
classFrameElement(types=AL) --> ['Types:'],descriptionAnnotatedList(AL).
classFrameElement(facts=AL) --> ['Facts:'],factAnnotatedList(AL).
classFrameElement(sameAs=AL) --> ['SameAs:'],individualAnnotatedList(AL).
classFrameElement(differentFrom=AL) --> ['DifferentFrom:'],individualAnnotatedList(AL).


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

objectPropertyFrame( frame(objectProperty,C,EL) ) --> ['ObjectProperty:'],objectPropertyIRI(C),zeroOrMore(objectPropertyFrameElement,EL).
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

dataPropertyFrame( frame(dataProperty,C,EL) ) --> ['DataProperty:'],dataPropertyIRI(C),zeroOrMore(dataPropertyFrameElement,EL).
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

annotationPropertyFrame( frame(annotationProperty,C,EL) ) --> ['AnnotationProperty:'],annotationPropertyIRI(C),zeroOrMore(annotationPropertyFrameElement,EL).
annotationPropertyFrameElement(domain=AL) --> ['Domain:'],descriptionAnnotatedList(AL).
annotationPropertyFrameElement(range=AL) --> ['Range:'],descriptionAnnotatedList(AL).
annotationPropertyFrameElement(subPropertyOf=AL) --> ['SubPropertyOf:'],annotationPropertyExpressionAnnotatedList(AL).


% individualFrame ::= 'Individual:' individual { 'Annotations:'
%annotationAnnotatedList | 'Types:' descriptionAnnotatedList |
%'Facts:' factAnnotatedList | 'SameAs:' individualAnnotatedList |
%'DifferentFrom:' individualAnnotatedList }
individualFrame(I-EL) --> ['Individual:'], individual(I),zeroOrMore(individualFrameElement,EL).


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

misc(A-equivalentClasses(DL)) --> ['EquivalentClasses:'],annotations(A),description2List(DL).
misc(equivalentClasses(DL)) --> ['EquivalentClasses:'],description2List(DL).
misc(A-disjointClasses(DL)) --> ['DisjointClasses:'],annotations(A),description2List(DL).
misc(disjointClasses(DL)) --> ['DisjointClasses:'],description2List(DL).
misc(A-equivalentProperties(DL)) --> ['EquivalentProperties:'],annotations(A),description2List(DL).
misc(equivalentProperties(DL)) --> ['EquivalentProperties:'],description2List(DL).



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
