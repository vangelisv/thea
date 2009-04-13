/* -*- Mode: Prolog -*- */

:- use_module(owl2_model,[]).
:- use_module('owl2_from_rdf').

%% class(?Class,?Deprecated,?Complete,?Annotations, ?Descriptions)
owl_parser:class(C,false,true,Annotations,Descs) :-
        owl2_model:class(C),
        (   owl2_model:equivalentClasses(ECL) -> select(C,ECL,Descs) ; Descs = []),
	findall(annotation(AP,AV),owl2_model:annotationAssertion(AP,C,AV),Annotations).

owl_parser:subclassOf(X,Y) :-
	owl2_model:subClassOf(X,Y).


owl_parser:property(PID,Deprecated,AnnotationList,PID_SuperList,
		    PTList,
		    PID_DomainList,PID_RangeList):-
	(   owl2_model:dataProperty(PID) ;  owl2_model:objectProperty(PID)),
	owl_parser:property_type(PID,PTList),
	Deprecated = false,
	findall(annotation(AP,AV),owl2_model:annotationAssertion(AP,PID,AV),AnnotationList),
	findall(SPID,owl2_model:subPropertyOf(PID,SPID),PID_SuperList),
	findall(Domain,owl2_model:propertyDomain(PID,Domain),PID_DomainList),
	findall(Range,owl2_model:propertyRange(PID,Range),PID_RangeList).
	

owl_parser:property_type(PID,[OT,F,IF,T,S,iof(Inv)]) :-
	(   owl2_model:dataProperty(PID) -> OT =datatypeProperty ; true),
	(   owl2_model:objectProperty(PID) -> OT = objectProperty ; true),
	(   owl2_model:functionalProperty(PID) -> F = functional ; true),
	(   owl2_model:inverseFunctionalProperty(PID) -> IF=inversefunctional; true),
	(   owl2_model:transitiveProperty(PID) -> T = transitive ; true),
	(   owl2_model:symmetricProperty(PID)-> S = symmetric ; true),
	(   owl2_model:inverseProperties(PID,Inv) ; true),!.
	

owl_parser:individual(I,Annotations,ITList,IVList) :-
        owl2_model:classAssertion(_,I),
	findall(C1,owl2_model:classAssertion(C1,I),ITList),
	findall(value(P,V),owl2_model:propertyAssertion(P,I,V),IVList),
	findall(annotation(AP,AV),owl2_model:annotationAssertion(AP,I,AV),Annotations).




%% TODO: this is just an example so far...

owl2_from_rdf:owl_repository('http://www.semanticweb.gr/elevator.owl','testfiles/elevator5-tbox.owl').
owl2_from_rdf:owl_repository('http://www.kleemann.gr/elevator/data','testfiles/elevator5-abox.owl').
owl2_from_rdf:owl_repository('http://www.theoldtile.gr/data','testfiles/elevator5-tiles.owl').


