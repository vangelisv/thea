/* -*- Mode: Prolog -*- */

:- use_module(owl2_model,[]).
:- use_module('owl2_from_rdf').

%% class(?Class,?Deprecated,?Complete,?Annotations, ?Descriptions)
owl_parser:class(C,false,true,Annotations,Descs) :-
        owl2_model:class(C),
        (   owl2_model:equivalentClasses(ECL) -> select(C,ECL,Descs) ; Descs = []),
	findall(annotation(AP,AV),owl2_model:annotationAssertion(AP,C,AV),Annotations).

owl_parser:individual(I,Annotations,ITList,IVList) :-
        owl2_model:classAssertion(_,I),
	findall(C1,owl2_model:classAssertion(C1,I),ITList),
	findall(value(P,V),owl2_model:propertyAssertion(P,I,V),IVList),
	findall(annotation(AP,AV),owl2_model:annotationAssertion(AP,I,AV),Annotations).


%% TODO: this is just an example so far...

owl2_from_rdf:owl_repository('http://www.semanticweb.gr/elevator.owl','testfiles/elevator5-tbox.owl').
owl2_from_rdf:owl_repository('http://www.kleemann.gr/elevator/data','testfiles/elevator5-abox.owl').
owl2_from_rdf:owl_repository('http://www.theoldtile.gr/data','testfiles/elevator5-tiles.owl').


