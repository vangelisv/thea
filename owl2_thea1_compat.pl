/* -*- Mode: Prolog -*- */
:- module(owl2_thea1_compat,[
			     class/5,
			     subclassOf/2,
			     disjointSet/1,
			     equivalentSet/1,
			     property/7,
			     sameIndividuals/1,
			     individual/4
		    ]
	 ).



:- use_module(owl2_model,[]).
:- use_module('owl2_from_rdf').

%% class(?Class,?Deprecated,?Complete,?Annotations, ?Descriptions)
class(C,false,true,Annotations,Descs) :-
        owl2_model:class(C),
        (   owl2_model:equivalentClasses(ECL),member(C,ECL) -> select(C,ECL,Descs) ; Descs = []),
	findall(annotation(AP,AV),owl2_model:annotationAssertion(AP,C,AV),Annotations).

subclassOf(X,Y) :-
	owl2_model:subClassOf(X,Y).

disjointSet(S) :-
	owl2_model:disjointClasses(S).

equivalentSet(S) :-
	owl2_model:equivalentClasses(S).

property(PID,Deprecated,AnnotationList,PID_SuperList,
		    PTList,
		    PID_DomainList,PID_RangeList):-
	(   owl2_model:dataProperty(PID) ;  owl2_model:objectProperty(PID)),
	property_type(PID,PTList),
	Deprecated = false,
	findall(annotation(AP,AV),owl2_model:annotationAssertion(AP,PID,AV),AnnotationList),
	findall(SPID,owl2_model:subPropertyOf(PID,SPID),PID_SuperList),
	findall(Domain,owl2_model:propertyDomain(PID,Domain),PID_DomainList),
	findall(Range,owl2_model:propertyRange(PID,Range),PID_RangeList).
	

property_type(PID,[OT,F,IF,T,S,iof(Inv)]) :-
	(   owl2_model:dataProperty(PID) -> OT =datatypeProperty ; true),
	(   owl2_model:objectProperty(PID) -> OT = objectProperty ; true),
	(   owl2_model:functionalProperty(PID) -> F = functional ; true),
	(   owl2_model:inverseFunctionalProperty(PID) -> IF=inversefunctional; true),
	(   owl2_model:transitiveProperty(PID) -> T = transitive ; true),
	(   owl2_model:symmetricProperty(PID)-> S = symmetric ; true),
	(   owl2_model:inverseProperties(PID,Inv) ; true),!.
	

				
sameIndividuals(Set) :-
	owl2_model:sameIndividual(Set).



individual(I,Annotations,ITList,IVList) :-
        owl2_model:classAssertion(_,I),
	findall(C1,owl2_model:classAssertion(C1,I),ITList),
	findall(value(P,V),owl2_model:propertyAssertion(P,I,V),IVList),
	findall(annotation(AP,AV),owl2_model:annotationAssertion(AP,I,AV),Annotations).



go :-
	owl_parse('http://www.theoldtile.gr/data',complete,complete,true),
	open('thea2',write,St),
	print(St, '****   Classes *****'),nl(St),
	forall(owl_parser:class(A,B,C,D,E),(print(St,A-B-C-D-E),nl(St))),
	print(St, '****   Sub Classes *****'),nl(St),
	forall(owl_parser:subclassOf(A,B),(print(St,A-B),nl(St))),
	print(St, '****   Equiv Set  *****'),nl(St),	
	forall(owl_parser:equivalentSet(S),(print(St,S),nl(St))),
	print(St, '****   DisjointSet Set  *****'),nl(St),
	forall(owl_parser:disjointSet(S),(print(St,S),nl(St))),
	print(St, '****   Properties   *****'),nl(St),
	forall(owl_parser:property(A,B,C,D,E,F,G),(print(St,A-B-C-D-E-F-G),nl(St))),	
	print(St, '****   Annotation Properties  *****'),nl(St),
	forall(owl_parser:annotationProperty(A),(print(St-A),nl(St))),	
	print(St, '****   Individuals  *****'),nl(St),      	
	forall(owl_parser:individual(A,B,C,D),(print(St,A-B-C-D),nl(St))),	
	print(St, '****   Different Individuals  *****'),nl(St),
	forall(owl_parser:differentIndividuals(S),(print(St,S),nl(St))),	
	print(St, '****   Same Individuals  *****'),nl(St),
	forall(owl_parser:sameIndividuals(S),(print(St,S),nl(St))),	
      

	close(St).

