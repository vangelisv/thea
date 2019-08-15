/* -*- Mode: Prolog -*- */

:- module(owl2_manchester_parser,
          [
           owl_parse_manchester_syntax_file/1,
           owl_parse_manchester_syntax_file/2,
	   owl_parse_manchester_expression/2,
	   owl_parse_manchester_frame/2,
           owl_parse_manchester_file_axioms/3   % File, Axioms, Options
           ]).
:- use_module(library(dcg/basics)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(debug)).

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

owl_parse_manchester_file_axioms(File, Axioms, _Opts) :-
        read_file_to_codes(File,Codes,[]),
	codes_tokens_filtered(Codes,Tokens),
	ontologyDocument( Ont, Tokens, []),
        ontdoc_axioms(Ont, Axioms).

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

process_ontdoc(Ontology) :-
        ontdoc_axioms(Ontology, Axioms),
	maplist(assert_axiom,Axioms).

%!      ontdoc_axioms(+OntDOc, -Axioms) is det.
%
%       Translate the parser AST into a list of axioms.

ontdoc_axioms(NSL-ontology(O,_L1,_L2,Frames), Axioms) :-
        maplist(process_frame(O-NSL), Frames, NestedAxioms),
        append(NestedAxioms, Axioms).

%% process_frame(+Ont, +FrameParseTree, -Axioms)
%
% translate the parse tree for a frame into axioms and declarations
%
% e.g. frame(class, foo, [subClassOf=[bar]]),
process_frame(O, frame(Type,Name,Props),[Unary|Axioms]) :-
	!,
	expand_curie(Name,O,IRI),
	Unary =.. [Type,IRI],
        maplist(process_property_(IRI,Type,O), Props, NestedAxioms),
        append(NestedAxioms, Axioms).
process_frame(O, AV,[V]) :-
        value_annotations(AV, V0, _Annotations),
        V0 =.. [Misc,Values0],
        !,
        maplist(expand_curie_o(O), Values0, Values),
        V =.. [Misc,Values].
process_frame(_, AV,[]) :-
        debug(man(axioms), 'Skipped (not a frame) ~p', [AV]).

process_property_(IRI,Type,O,Prop,Axioms) :-
        process_property(Prop,IRI,Type,O,Axioms).

process_property(annotation=AL,IRI,Type,O,Axioms) :-
        !,
        maplist(process_annotation(IRI,Type,O), AL, Axioms).
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

process_annotation(IRI,_Type,O,
                   Name-Value,
                   annotationAssertion(PropertyIRI,IRI,OwlValue)) :-
        expand_curie(Name, O, PropertyIRI),
        expand_curie(Value, O, OwlValue).

process_characteristic(C,IRI,_Type,_,Unary) :-
	slot_predicate(C,P),
	Unary =.. [P,IRI].

process_slot_value(S,V0,IRI,T,O,Ax) :-
        value_annotations(V0, V1, _Annotations),
	expand_curie(V1,O,V),
        (   slot_axiom(S, T, O, IRI, V, Ax)
        ->  true
        ;   slot_axiom(S, T)
        ->  Ax =.. [S,IRI,V]
        ;   debug(man(axioms), 'guessing for: ~p', [S-T]),
            Ax =.. [S,IRI,V]
        ).

%!      slot_axiom(+Attribute, +IRIType, +O, +IRI, +Value, -Axiom)

slot_axiom(disjointWith, objectProperty, _,
           IRI, V, disjointProperties([IRI,V])).
slot_axiom(inverseOf, _, _,
           IRI, V, inverseProperties(IRI,V)).
slot_axiom(domain, _, _,
           IRI, V, propertyDomain(IRI,V)).
slot_axiom(range, _, _,
           IRI, V, propertyRange(IRI,V)).
slot_axiom(types, _, _,
           IRI, V, classAssertion(V,IRI)).
slot_axiom(facts, _, O,
           IRI, P-V0, propertyAssertion(P,IRI,V)) :-
        expand_curie(V0,O,V).
slot_axiom(facts, _, O,
           IRI, not(P-V0), negativePropertyAssertion(P,IRI,V)) :-
        expand_curie(V0,O,V).
slot_axiom(equivalentTo, objectProperty, _,
           IRI, V, equivalentProperties([IRI,V])).
slot_axiom(equivalentTo, dataProperty, _,
           IRI, V, equivalentProperties([IRI,V])).
slot_axiom(equivalentTo, class, _,
           IRI, V, equivalentClasses([IRI,V])).
slot_axiom(disjointWith, objectProperty, _,
           IRI, V, disjointProperties([IRI,V])).
slot_axiom(disjointWith, dataProperty, _,
           IRI, V, disjointProperties([IRI,V])).
slot_axiom(disjointWith, class, _,
           IRI, V, disjointClasses([IRI,V])).
slot_axiom(sameAs, namedIndividual, _,
           IRI, V, sameIndividual([IRI,V])).
slot_axiom(differentFrom, namedIndividual, _,
           IRI, V, differentIndividuals([IRI,V])).

%!      slot_axiom(+Attribute, +IRIType) is semidet.
%
%       Indicates that the generic translation is correct.

slot_axiom(subClassOf,    class).
slot_axiom(subPropertyOf, objectProperty).
slot_axiom(subPropertyOf, dataProperty).
slot_axiom(subPropertyOf, annotationProperty).

%!      value_annotations(+Value0, -Value, -Annotations) is det.
%
%       Split the annotations from the value.
%       @see annotated//2.

value_annotations(annotated(Value, Annotations), Value, Annotations) :- !.
value_annotations(Value, Value, []).

%!      expand_curie(+ASTValue, +Ontology, -RDFValue)

expand_curie(IRI, _O-NSL, FullIRI) :-
        atom(IRI),
        atomic_list_concat([NS,Name], :, IRI),
        xml_name(Name, utf8),
        memberchk(NS-Prefix, NSL),
        !,
        atom_concat(Prefix,Name,FullIRI).
expand_curie(IRI, _, IRI) :-
        atom(IRI),
        uri_is_global(IRI), !.
expand_curie(Name,_O-NSL,IRI) :-
        atom(Name),
        memberchk(''-Prefix, NSL),
        !,
        atom_concat(Prefix,Name,IRI).
expand_curie(inverseOf(X0), O, inverseOf(X)) :-
        !,
        expand_curie(X0, O, X).
expand_curie(X,_,X).

expand_curie_o(O,AST,RDF) :-
        expand_curie(AST,O,RDF).

%!      slot_predicate(+Characteristic, -AxiomName)

slot_predicate('Functional',        functionalProperty).
slot_predicate('InverseFunctional', inverseFunctionalProperty).
slot_predicate('Reflexive',         reflexiveProperty).
slot_predicate('Irreflexive',       irreflexiveProperty).
slot_predicate('Symmetric',         symmetricProperty).
slot_predicate('Asymmetric',        asymmetricProperty).
slot_predicate('Transitive',        transitiveProperty).

% ----------------------------------------
% Tokenization
% ----------------------------------------

codes_tokens_filtered(Codes,Tokens) :-
        phrase(tokens(Tokens), Codes).

tokens(Tokens) -->
        blanks,
        tokens2(Tokens).

tokens2([]) -->
        eos, !.
tokens2([H|T]) -->
        sep(H),
        !,
        tokens(T).
tokens2([H|T]) -->
        token(H),
        tokens(T).

token(String)  --> quotedString(String), !.
token(IRI)     --> iri_token(IRI), !.
token(NodeID)  --> node_id_token(NodeID), !.
token(Float)   --> floatingPointLiteral(Float), !.
token(Float)   --> decimalLiteral(Float), !.
token(Float)   --> integerLiteral(Float), !.
token(^^)      --> "^^", !.
token(@)       --> "@", !.
token(<=)      --> "<=", !.
token(<)       --> "<", !.
token(>=)      --> ">=", !.
token(>)       --> ">", !.
token(Keyword) --> keyword(Keyword), !.
token(H)       --> token_chars(L), { atom_codes(H,L) }.

token_chars([H|T]) -->
        [H],
        \+ { split(H) },
        !,
        token_chars(T).
token_chars([]) -->
        [].

split(0'\s).
split(0'\n).
split(0'\r).
split(0',).
split(0'().
split(0')).
split(0'[).
split(0']).
split(0'{).
split(0'}).
split(0'<).
split(0'>).
split(0'@).
split(0'^).

sep(',') --> ",".
sep('(') --> "(".
sep(')') --> ")".
sep('[') --> "[".
sep(']') --> "]".
sep('{') --> "{".
sep('}') --> "}".

keyword(Keyword) -->
        [C], { between(0'A, 0'Z, C) },
        letters(L),
        ":", !,
        { string_codes(KwdU, [C|L]),
          downcase_atom(KwdU, Kwd),
          Keyword = keyword(Kwd)
        }.

letters([H|T]) --> letter(H), !, letters(T).
letters([]) --> [].

letter(C) --> [C], { between(0'a, 0'z, C) -> true ; between(0'A, 0'Z, C) }.

% floatingPointLiteral ::= [ '+' | '-'] ( digits ['.'digits] [exponent] | '.' digits[exponent]) ( 'f' | 'F' )

floatingPointLiteral(float(Value)) -->
        here(Start),
        opt_sign,
        (   digits
        ->  (   "."
            ->  digits
            ;   ""
            ),
            { Convert = Chars }
        ;   ".",
            digits,
            { Convert = [0'0|Chars] }
        ),
        opt_exponent,
        capture(Start, Chars),
        f, !,
        { number_codes(Value, Convert) }.

here(Start, Start, Start).
capture(Start, [], Left, Left) :-
        same_term(Start, Left), !.
capture([H|T0], [H|T], Left, Left) :-
        capture(T0, T, Left, Left).

f --> "f".
f --> "F".

% exponent ::= ('e' | 'E') ['+' | '-'] digits

opt_exponent --> exponent, !.
opt_exponent --> "".

exponent --> e, opt_sign, digits.
e --> "e", !.
e --> "E".

opt_sign --> "+", !.
opt_sign --> "-", !.
opt_sign --> "".

digits --> digit, digits0.

digits0 --> digit, !, digits0.
digits0 --> "".

digit --> "0".
digit --> "1".
digit --> "2".
digit --> "3".
digit --> "4".
digit --> "5".
digit --> "6".
digit --> "7".
digit --> "8".
digit --> "9".


% decimalLiteral ::= ['+' | '-'] digits '.' digits

decimalLiteral(decimal(Float)) -->
        here(Start), opt_sign, digits, ".", digits, !, capture(Start, Codes),
        { number_codes(Float, Codes) }.

% integerLiteral ::= ['+' | '-'] digits

integerLiteral(integer(Int)) -->
        here(Start), opt_sign, digits, !, capture(Start, Codes),
        { number_codes(Int, Codes) }.

%  quotedString := 'a finite sequence of characters in which " (U+22) and
%  \ (U+5C) occur only in pairs of the form \" (U+22, U+5C) and \\
%  (U+22, U+22), enclosed in a pair of " (U+22) characters'

quotedString(string(String)) -->
        "\"", quotedStringChars(Codes), "\"", !,
        { atom_codes(String, Codes) }.


quotedStringChars([]) --> "".
quotedStringChars([H|T]) -->
        quotedStringChar(H),
        quotedStringChars(T).

quotedStringChar(0'\") --> "\\\"", !.
quotedStringChar(0'\\) --> "\\\\", !.
quotedStringChar(C) --> [C], {   C \== 0'\\, C \== 0'\"
			       ->  true
                               ;   syntax_error(owl_manchester_string)
                               }.

iri_token(iri(IRI)) -->
        "<", iri_chars(Codes), ">", !,
        { atom_codes(IRI, Codes) }.

iri_chars([]) --> [].
iri_chars([H|T]) --> iri_char(H), iri_chars(T).

iri_char(H) --> [H], { \+ ws(H) }.

ws(0'\s).
ws(0'\n).
ws(0'\r).

node_id_token(nodeID(NodeID)) --> "_:", token_chars(L), { atom_codes(NodeID,L) }.


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

:- meta_predicate
        zeroOrMore(3,?,+,?),
        zeroOrMore(+,3,?,+,?),
        oneOrMore(+,3,?,+,?),
        annotated(3,-,+,?).

% <NT>List ::= <NT> { , <NT> }
%% zeroOrMore( +F, ?List, +InToks, ?Rest )
zeroOrMore(F,[X|L],In,Rest):-
        call(F, X, In, Rest1),
        !,
        zeroOrMore(F,L,Rest1,Rest).
zeroOrMore(_,[],In,In):- !.

% e.g. zeroOrMore(or,description,L)
zeroOrMore(Delim,F,[X|L],In,Rest):-
        call(F,X,In,Rest1),
        !,
        (   Rest1 = [Delim|Rest2]
        ->  zeroOrMore(Delim,F,L,Rest2,Rest)
        ;   Rest = Rest1,
            L = []
        ).
zeroOrMore(_,_,[],In,In):- !.

% <NT>2List ::= <NT> , <NT>List
oneOrMore(Delim,F,[X|L],In,Rest):-
        call(F,X,In,Rest1),
        !,
        (   Rest1 = [Delim|Rest2]
        ->  oneOrMore(Delim,F,L,Rest2,Rest)
        ;   Rest = Rest1,
            L = []
        ).

%!      annotated(:F, -Annotated)//

annotated(F, Result) -->
        annotations(A),
        !,
        call(F, R0),
        { Result = annotated(R0, A) }.
annotated(F, Result) -->
        call(F, Result).

% <NT>AnnotatedList ::= [annotations] <NT> { , [annotations]<NT> }

% ----------------------------------------
% 2.1 IRIs, Integers, Literals, and Entities
% ----------------------------------------

% full-IRI := 'IRI as defined in [RFC3987], enclosed in a pair of < (U+3C) and > (U+3E) characters'
full_IRI(IRI) --> [iri(IRI)].

% NCName := 'as defined in [XML Namespaces]'
% TODO

% irelative-ref := 'as defined in [RFC3987]'
% TODO

% namespace := full-IRI
% ?namespace(X) --> full_IRI(X).

% prefixName := NCName

prefixName(NS) --> [P], {atom(P), atom_concat(NS, :, P)}.

% reference := irelative-ref

% curie := [ [ prefix ] ':' ] reference
curie(X) --> [X], { atom(X), \+ reserved(X), \+ phrase(sep(X),_,_) }.
% TODO
reserved(and).                                  % logical connectors
reserved(or).
reserved(not).
reserved(some).                                 % restrictions
reserved(only).
reserved(value).
reserved(min).
reserved(max).
reserved(exactly).
reserved('Self').
reserved(integer).                              % datatypes
reserved(decimal).
reserved(float).
reserved(string).

% IRI := full-IRI | curie
iri(X) --> full_IRI(X).
iri(X) --> curie(X).

% nonNegativeInteger ::= zero | positiveInteger positiveInteger ::= nonZero { digit } digits ::= digit { digit } digit ::= zero | nonZero nonZero := '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' zero ::= '0'
% TODO: this allows for +42
nonNegativeInteger(N) --> [integer(N)], { N >= 0 }.

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
annotationPropertyIRI(X) --> iri(X).

% individual ::= individualIRI | nodeID
individual(X) --> individual(X, _).
individual(X, namedIndividual) --> individualIRI(X).
individual(X, anonymousIndividual) --> nodeID(X).

% individualIRI ::= IRI
individualIRI(X) --> iri(X).

% nodeID := 'a node ID of the form _:name as specified in the N-Triples specification [RDF Test Cases]'
nodeID(X) --> [nodeID(X)].

% all the following are TODO

% literal ::= typedLiteral | abbreviatedXSDStringLiteral | abbreviatedRDFTextLiteral | integerLiteral | decimalLiteral | floatingPointLiteral

literal(X) --> typedLiteral(X), !.
literal(X) --> abbreviatedRDFTextLiteral(X), !.
literal(X) --> abbreviatedXSDStringLiteral(X), !.
literal(X) --> [integer(X)], !.
literal(X) --> [decimal(X)], !.
literal(X) --> [float(X)], !.

% typedLiteral ::= lexicalValue '^^' Datatype

typedLiteral(literal(type(Type,Lexical))) -->
        [string(Lexical),'^^'],datatype(Type).

% abbreviatedXSDStringLiteral ::= quotedString

abbreviatedXSDStringLiteral(literal(type(Type,Lexical))) -->
        [string(Lexical)],
        { Type = 'http://www.w3.org/2001/XMLSchema#string' }.

% abbreviatedRDFTextLiteral ::= quotedString '@' languageTag
% languageTag := 'a nonempty (not quoted) string defined as specified in BCP 47 [BCP 47]'

abbreviatedRDFTextLiteral(literal(lang(Lang,Lexical))) -->
        [string(Lexical),'@',Lang], {atom(Lang)}.

% lexicalValue ::= quotedString

% entity ::= 'Datatype' '(' datatypeIRI ')' | 'Class' '(' classIRI ')' | 'ObjectProperty' '(' objectPropertyIRI ')' | 'DataProperty' '('dataPropertyIRI ')' | 'AnnotationProperty' '(' annotationPropertyIRI ')' | 'NamedIndividual' '(' individualIRI ')'

% ----------------------------------------
%  2.2 Ontologies and Annotations
% ----------------------------------------

% annotations ::= 'Annotations:' annotationAnnotatedList
annotations(L) --> [keyword(annotations)], annotationAnnotatedList(L).

% annotation ::= annotationPropertyIRI annotationTarget
annotation(P-T) -->  annotationPropertyIRI(P),annotationTarget(T).


% annotationTarget ::= nodeID | IRI | literal
annotationTarget(T) --> nodeID(T) ; iri(T) ; literal(T).

% ontologyDocument ::= { namespace } ontology
ontologyDocument( NSL-O ) --> zeroOrMore(prefixDeclaration,NSL),ontology(O).

% prefixDeclaration ::= 'Prefix:' [ prefixName ] full-IRI
prefixDeclaration(P-NS) --> [keyword(prefix)], prefixName(P), full_IRI(NS).

% ontology ::= 'Ontology:' [ ontologyIRI [ versionIRI ] ] { import } { annotations } { frame }
ontology(ontology(X,Imports,AL,Frames)) -->
        [keyword(ontology)], ontologyIRI(X), !,
        zeroOrMore(import,Imports),
        zeroOrMore(annotations,AL),
        zeroOrMore(frame,Frames).
ontology(ontology(Imports,AL,Frames)) -->
        [keyword(ontology)], !,
        zeroOrMore(import,Imports),
        zeroOrMore(annotations,AL),
        zeroOrMore(frame,Frames).
ontologyIRI(X-V) --> iri(X),versionIRI(V). % TODO
ontologyIRI(X) --> iri(X).

% ontologyIRI ::= IRI
ontologyIRI(X) --> iri(X).

% versionIRI ::= IRI
versionIRI( X) --> iri(X).

% import ::= 'Import:' IRI
import(X) --> [keyword(import)], iri(X).


% frame ::= datatypeFrame | classFrame | objectPropertyFrame | dataPropertyFrame |
% annotationPropertyFrame | individualFrame | misc

frame(X) --> (   datatypeFrame(X)
             ;   classFrame(X)
             ;   objectPropertyFrame(X)
             ;   dataPropertyFrame(X)
             ;   annotationPropertyFrame(X)
             ;   individualFrame(X)
             ;   misc(X)
             ).

% ----------------------------------------
%      2.3 Property and Datatype Expressions
% ----------------------------------------

% objectPropertyExpression ::= objectPropertyIRI | 'inverse' objectPropertyIRI
objectPropertyExpression(OPE) -->
        (   [inverse]
        ->  objectPropertyIRI(IRI),
            { OPE = inverseOf(IRI) }
        ;   objectPropertyIRI(OPE)
        ).

% dataPropertyExpression ::= dataPropertyIRI
dataPropertyExpression(P) --> dataPropertyIRI(P).

% dataRange ::= dataConjunction 'or' dataConjunction { 'or' dataConjunction } | dataConjunction
dataRange(DR) -->
        dataConjunction(C1),
        (   [or]
        ->  dataConjunction(C2),
            zeroOrMore(or,dataConjunction,CL),
            { DR = unionOf([C1,C2|CL]) }
        ;   { DR = C1 }
        ).

%dataConjunction ::= dataPrimary 'and' dataPrimary { 'and' dataPrimary } | dataPrimary
dataConjunction(D) -->
        dataPrimary(D1),
        (   [and]
        ->  dataPrimary(D2),
            zeroOrMore(dataPrimary,DL),
            { D = intersectionOf([D1,D2|DL]) }
        ;   { D = D1 }
        ).

% dataPrimary ::= [ 'not' ] dataAtomic
dataPrimary(complementOf(A)) --> [not], !, dataAtomic(A).
dataPrimary(A) --> dataAtomic(A).

% dataAtomic ::= Datatype | '{' literal { ',' literal } '}' |
% datatypeRestriction | '(' dataRange ')'
dataAtomic([X1|XL]) --> ['{'],!, literal(X1),zeroOrMore(',',literal,XL),['}'].
dataAtomic(A) --> ['('],dataRange(A),[')'].
dataAtomic(A) --> datatypeRestriction(A).
dataAtomic(A) --> datatype(A).

% datatypeRestriction ::= Datatype '[' facet restrictionValue { ',' facet restrictionValue } ']'
datatypeRestriction(X-[F-V|FVs]) -->
        datatype(X),['['],!, facet(F),restrictionValue(V),
        zeroOrMore(',',facetRestrictionValue, FVs),[']'].
facetRestrictionValue(F-V) --> facet(F),restrictionValue(V).

% facet ::= 'length' | 'minLength' | 'maxLength' | 'pattern' | 'langRange' | '<=' | '<' | '>=' | '>'

facet(length)    --> [length].
facet(minLength) --> [minLength].
facet(maxLength) --> [maxLength].
facet(pattern)   --> [pattern].
facet(langRange) --> [langRange].
facet(<=)        --> [<=].
facet(<)         --> [<].
facet(>=)        --> [>=].
facet(>)         --> [>].

% restrictionValue ::= literal
restrictionValue(V) --> literal(V).


% ----------------------------------------
%      2.4 Descriptions
% ----------------------------------------

% description ::= conjunction 'or' conjunction { 'or' conjunction } | conjunction
description(D) -->
        conjunction(C1),
        (   [or]
        ->  conjunction(C2),
            zeroOrMore(or, conjunction,CL),
            { D = unionOf([C1,C2|CL]) }
        ;   { D = C1 }
        ).

% conjunction ::= classIRI 'that' [ 'not' ] restriction { 'and' [ 'not' ] restriction } | primary 'and' primary { 'and' primary }| primary
conjunction(intersectionOf([Genus|DL])) --> classIRI(Genus), [that], !, oneOrMore(and, optNegRestriction,DL).
conjunction(D) -->
        primary(P),
        (   [and]
        ->  primary(D1),
            zeroOrMore(and, primary, DL),
            { D = intersectionOf([P,D1|DL]) }
        ;   { D = P }
        ).

% primary ::= [ 'not' ] ( restriction | atomic )
primary(R) --> optNegRestriction(R).
primary(complementOf(A)) --> [not],!,atomic(A).
primary(A) --> atomic(A).

optNegRestriction(complementOf(R)) --> [not],!,restriction(R).
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

restriction(Restriction) --> objectPropertyExpression(OPE), [Type], objectRestriction(Type, OPE, Restriction), !.
restriction(Restriction) --> dataPropertyExpression(OPE),   [Type], dataRestriction(Type, OPE, Restriction), !.

objectRestriction(some,    OPE, someValuesFrom(OPE,C))         --> primary(C).
objectRestriction(only,    OPE, allValuesFrom(OPE,C))          --> primary(C).
objectRestriction(value,   OPE, hasValue(OPE,I))               --> individual(I).
objectRestriction('Self',  OPE, hasSelf(OPE))                  --> [].
objectRestriction(min,     OPE, minCardinality(Card,OPE,CE))   --> nni(Card),primary(CE), !.
objectRestriction(min,     OPE, minCardinality(Card,OPE))      --> nni(Card).
objectRestriction(max,     OPE, maxCardinality(Card,OPE,CE))   --> nni(Card),primary(CE), !.
objectRestriction(max,     OPE, maxCardinality(Card,OPE))      --> nni(Card).
objectRestriction(exactly, OPE, exactCardinality(Card,OPE,CE)) --> nni(Card),primary(CE), !.
objectRestriction(exactly, OPE, exactCardinality(Card,OPE))    --> nni(Card).

dataRestriction(some,    OPE, someValuesFrom(OPE,C))         --> dataPrimary(C).
dataRestriction(only,    OPE, allValuesFrom(OPE,C))          --> dataPrimary(C).
dataRestriction(value,   OPE, hasValue(OPE,L))               --> literal(L).
dataRestriction('Self',  OPE, hasSelf(OPE))                  --> [].
dataRestriction(min,     OPE, minCardinality(Card,OPE,CE))   --> nni(Card),dataPrimary(CE), !.
dataRestriction(min,     OPE, minCardinality(Card,OPE))      --> nni(Card).
dataRestriction(max,     OPE, maxCardinality(Card,OPE,CE))   --> nni(Card),dataPrimary(CE), !.
dataRestriction(max,     OPE, maxCardinality(Card,OPE))      --> nni(Card).
dataRestriction(exactly, OPE, exactCardinality(Card,OPE,CE)) --> nni(Card),dataPrimary(CE), !.
dataRestriction(exactly, OPE, exactCardinality(Card,OPE))    --> nni(Card).

nni(N) --> nonNegativeInteger(N).


% atomic ::= classIRI | '{' individual { ',' individual } '}' | '(' description ')'
atomic(X) --> classIRI(X).
atomic(L) --> ['{'], !, oneOrMore(',',individual,L),['}'].
atomic(X) --> ['('], !, description(X), [')'].

% <NT>AnnotatedList ::= [annotations] <NT> { , [annotations] <NT> }
annotationAnnotatedList(L) --> oneOrMore(',', annotation,L).
descriptionAnnotatedList(L) --> oneOrMore(',',annotatedDescription,L). % TODO
objectPropertyCharacteristicAnnotatedList(L) --> oneOrMore(',',annotatedObjectPropertyCharacteristic,L).
objectPropertyExpressionAnnotatedList(L) --> oneOrMore(',',annotatedObjectPropertyExpression,L).

annotatedObjectPropertyCharacteristic(X) --> objectPropertyCharacteristic(X). % TODO
annotatedObjectPropertyExpression(X) --> objectPropertyExpression(X). % TODO


% <NT>2List ::= <NT> , <NT>List
description2List(L) --> oneOrMore(',',description,L).

annotatedDescription(D) --> annotated(description, D).

objectProperty2List(L) --> oneOrMore(',', objectPropertyExpression, L).

dataProperty2List(L) --> oneOrMore(',', dataPropertyExpression, L).

individual2List(L) --> oneOrMore(',', individual, L).

% ----------------------------------------
%  2.5 Frames and Miscellaneous
% ----------------------------------------

% classFrame ::= 'Class:' classIRI { 'Annotations:'
% annotationAnnotatedList | 'SubClassOf:' descriptionAnnotatedList |
% 'EquivalentTo:' descriptionAnnotatedList | 'DisjointWith:'
% descriptionAnnotatedList | 'DisjointUnionOf:' annotations
% description2List }

datatypeFrame(frame(datatype,C,EL)) --> [keyword(datatype)],!,classIRI(C),zeroOrMore(datatypeFrameElement,EL).
datatypeFrameElement(annotation=AL) --> [keyword(annotations)],!,annotationAnnotatedList(AL).
datatypeFrameElement(equivalentTo=(DR)) --> [keyword(equivalentto)], !, annotated(dataRange, DR).

classFrame( frame(class,C,EL) ) --> [keyword(class)],!,classIRI(C),zeroOrMore(classFrameElement,EL).
classFrameElement(annotation=AL) --> [keyword(annotations)],!,annotationAnnotatedList(AL).
classFrameElement(subClassOf=AL) --> [keyword(subclassof)],!,descriptionAnnotatedList(AL).
classFrameElement(equivalentTo=AL) --> [keyword(equivalentto)],!,descriptionAnnotatedList(AL).
classFrameElement(disjointWith=AL) --> [keyword(disjointwith)],!,descriptionAnnotatedList(AL).
classFrameElement(disjointUnionOf=AL) --> [keyword(disjointunionof)],!,descriptionAnnotatedList(AL).
classFrameElement(hasKey=AL) --> [keyword(haskey)],!, annotated(hasKey, AL).

hasKey(K) --> objectPropertyExpression(K), !.
hasKey(K) --> dataPropertyExpression(K), !.


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

objectPropertyFrame( frame(objectProperty,C,EL) ) --> [keyword(objectproperty)],!,objectPropertyIRI(C),zeroOrMore(objectPropertyFrameElement,EL).
objectPropertyFrameElement(annotation=AL) --> [keyword(annotations)],!,annotationAnnotatedList(AL).
objectPropertyFrameElement(domain=AL) --> [keyword(domain)],!,descriptionAnnotatedList(AL).
objectPropertyFrameElement(range=AL) --> [keyword(range)],!,descriptionAnnotatedList(AL).
objectPropertyFrameElement(characteristics=AL) --> [keyword(characteristics)],!,objectPropertyCharacteristicAnnotatedList(AL).
objectPropertyFrameElement(subPropertyOf=AL) --> [keyword(subpropertyof)],!,objectPropertyExpressionAnnotatedList(AL).
objectPropertyFrameElement(equivalentTo=AL) --> [keyword(equivalentto)],!,objectPropertyExpressionAnnotatedList(AL).
objectPropertyFrameElement(disjointWith=AL) --> [keyword(disjointwith)],!,objectPropertyExpressionAnnotatedList(AL).
objectPropertyFrameElement(inverseOf=AL) --> [keyword(inverseof)],!,objectPropertyExpressionAnnotatedList(AL).
objectPropertyFrameElement(subPropertyChain=Chain) --> [keyword(subpropertychain)],!,annotated(subPropertyChain, Chain).

subPropertyChain([OPE1,OPE2|OPEs]) -->
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

dataPropertyFrame( frame(dataProperty,C,EL) ) --> [keyword(dataproperty)],!,dataPropertyIRI(C),zeroOrMore(dataPropertyFrameElement,EL).
dataPropertyFrameElement(annotation=AL) --> [keyword(annotations)],!,annotationAnnotatedList(AL).
dataPropertyFrameElement(domain=AL) --> [keyword(domain)],!,descriptionAnnotatedList(AL).
dataPropertyFrameElement(range=AL) --> [keyword(range)],!,dataRangeAnnotatedList(AL).
dataPropertyFrameElement(characteristics=AL) --> [keyword(characteristics)],!,dataPropertyCharacteristicAnnotatedList(AL).
dataPropertyFrameElement(subPropertyOf=AL) --> [keyword(subpropertyof)],!,dataPropertyExpressionAnnotatedList(AL).
dataPropertyFrameElement(equivalentTo=AL) --> [keyword(equivalentto)],!,dataPropertyExpressionAnnotatedList(AL).
dataPropertyFrameElement(disjointWith=AL) --> [keyword(disjointwith)],!,dataPropertyExpressionAnnotatedList(AL).

dataRangeAnnotatedList(AL) --> oneOrMore(',', dataRangeAnnotated, AL).
dataRangeAnnotated(R) --> annotated(dataRange, R).

dataPropertyCharacteristicAnnotatedList(P) --> [P],{dataPropertyCharacteristic(P)}.
dataPropertyCharacteristic('Functional').

dataPropertyExpressionAnnotatedList(AL) --> oneOrMore(',', annotatedDataPropertyExpression, AL).
annotatedDataPropertyExpression(E) --> annotated(dataPropertyExpression, E).


% annotationPropertyFrame ::= 'AnnotationProperty:'
%annotationPropertyIRI { 'Annotations:' annotationAnnotatedList } |
%'Domain:' IRIAnnotatedList | 'Range:' IRIAnnotatedList |
%'SubPropertyOf:' annotationPropertyIRIAnnotatedList

annotationPropertyFrame( frame(annotationProperty,C,EL) ) --> [keyword(annotationproperty)],!,annotationPropertyIRI(C),zeroOrMore(annotationPropertyFrameElement,EL).
annotationPropertyFrameElement(annotation=AL) --> [keyword(annotations)],!,annotationAnnotatedList(AL).
annotationPropertyFrameElement(domain=AL) --> [keyword(domain)],!,iriAnnotatedList(AL).
annotationPropertyFrameElement(range=AL) --> [keyword(range)],!,iriAnnotatedList(AL).
annotationPropertyFrameElement(subPropertyOf=AL) --> [keyword(subpropertyof)],!,annotationPropertyIRIAnnotatedList(AL).

%!  iriAnnotatedList(-List)//
%
%   This is not defined in the syntax.  It suggests a list if IRIs, but the example
%   shows `integer`.  We therefore use datatype//1.

iriAnnotatedList(AL) --> oneOrMore(',', annotatedIRI, AL).
annotatedIRI(IRI) --> annotated(datatype, IRI).

annotationPropertyIRIAnnotatedList(AL) --> oneOrMore(',', annotatedAnnotationPropertyIRI, AL).
annotatedAnnotationPropertyIRI(E) --> annotated(annotationPropertyIRI, E).



% individualFrame ::= 'Individual:' individual { 'Annotations:'
%annotationAnnotatedList | 'Types:' descriptionAnnotatedList |
%'Facts:' factAnnotatedList | 'SameAs:' individualAnnotatedList |
%'DifferentFrom:' individualAnnotatedList }
individualFrame(frame(Type,I,EL)) --> [keyword(individual)],!, individual(I,Type),zeroOrMore(individualFrameElement,EL).
individualFrameElement(annotation=AL) --> [keyword(annotations)],!,annotationAnnotatedList(AL).
individualFrameElement(types=AL) --> [keyword(types)],!,descriptionAnnotatedList(AL).
individualFrameElement(facts=AL) --> [keyword(facts)],!,factAnnotatedList(AL).
individualFrameElement(sameAs=AL) --> [keyword(sameas)],!,individualAnnotatedList(AL).
individualFrameElement(differentFrom=AL) --> [keyword(differentfrom)],!,individualAnnotatedList(AL).

factAnnotatedList(AL) --> oneOrMore(',', factAnnotated, AL).
factAnnotated(F) --> annotated(fact, F).

individualAnnotatedList(AL) --> oneOrMore(',', individualAnnotated, AL).
individualAnnotated(I) --> annotated(individual, I).

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

misc(Misc) --> [keyword(Kwd)], annotated(misc_content(Kwd), Misc), !.

misc_content(equivalentclasses,    equivalentClasses(DL))    --> description2List(DL).
misc_content(disjointclasses,      disjointClasses(DL))      --> description2List(DL).
misc_content(equivalentproperties, equivalentProperties(DL)) --> objectProperty2List(DL).
misc_content(disjointproperties,   disjointProperties(DL))   --> objectProperty2List(DL).
misc_content(equivalentproperties, equivalentProperties(DL)) --> dataProperty2List(DL).
misc_content(disjointproperties,   disjointProperties(DL))   --> dataProperty2List(DL).
misc_content(sameindividual,       sameIndividual(DL))       --> individual2List(DL).
misc_content(differentindividuals, differentIndividuals(DL)) --> individual2List(DL).



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
