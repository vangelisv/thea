:- module(owl2_owllink,
	  [ owl_link/4

	  ]).


:-use_module(library('http/http_client')).
:-use_module(library('sgml')).
:-use_module(library('sgml_write')).
:-use_module('owl2_xml').


/*
         	OWLLink Language Wrapper 
	     -------------------------------

This module is an implementation of the OWLLink interface. It allows
communication between a Prolog program and an OWLLink enabled reasoner.
It is using SWI Prolog's HTTP and SGML packages. 

	     -------------------------------
*/


%% owl_link(+ReasonerURL, +Request:list, -Response:list) is det
%  Implementes OWLLink over HTTP. Sends an OWLLink request to the ReasonerURL and
%  gets its Response. 

owl_link(ReasonerURL,Request,Response,Options) :-
	owl_link_request(Request,RequestXML),
	% write Request to predefined file
	(   member(request_file=Filename,Options) -> true ; Filename = '_thea_owllink_request.xml'),
	% capture(Filename,(print('<?xml version=\"1.0\"?>'),print_elements(RequestXML,0))),
	% Filename = 'protege_tell.xml',
	open(Filename,write,St),
	xml_write(St,element('RequestMessage',[xmlns='http://www.owllink.org/owllink-xml#',
					      xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance',
					      xsi:schemaLocation='http://www.owllink.org/owllink-xml# http://www.owllink.org/owllink-xml-20091016.xsd',
					      xmlns:owl2='http://www.w3.org/2006/12/owl2-xml#',
					      xmlns:owl='http://www.w3.org/2002/07/owl#'],
			     RequestXML),[layout(true)]),close(St),  % layout false to avoid newlines
	size_file(Filename,RLength), % size_file is not counting correctly newlines in 
	                                                     % windows environment.
	% print(RLength),nl,						     
        % read(RLength1), print(RLength1),
	RLength1 is RLength,
	(   http_post(ReasonerURL,file(Filename),Result,
			[request_header('Content-Type' = 'text/xml'), 
			 request_header('Content-Length' = RLength1)]) 
	-> 	
   	    % write reasoner response to predefined file
	    (	member(response_file=FilenameResponse,Options) -> true ; FilenameResponse = '_thea_owllink_response.xml'),
	    open(FilenameResponse,write,St),write(St,Result),close(St),
   	    % Read response from file into an xml structure
	    open(FilenameResponse,read,St),	    
	    load_structure(St,Response,[dialect(xml),space(sgml)]),	close(St)
	; debug(owllink,'Reasoner error ~w',[ReasonerURL])
	).
	

% owl_link_request(+TheaReq,-OwlLinkReq)
%	 Convert a request from Thea / OWL2 to OwlLink XML Schema format

owl_link_request([],[]).
owl_link_request([Req1|ReqRest],[Res1|ResRest]) :-
	owl_link_request(Req1,Res1),
	owl_link_request(ReqRest,ResRest).



% 	 <!--  Management -->
owl_link_request(getDescription,element('GetDescription',[],[])).
owl_link_request(getSettings(KB),element('GetSettings',[kb=KB],[])).		 
owl_link_request(createKB(KB_Name_Attrs,Prefixes),element('CreateKB',KB_Name_Attrs,EPrefixes)):-
	options_to_elements(Prefixes,EPrefixes).
owl_link_request(releaseKB(KB),element('ReleaseKB',[kb=KB],[])).
owl_link_request(set(KB,Key,Settings),element('Set',[kb=KB,key=Key],LiteralSettings)) :-
	options_to_literals(Settings,LiteralSettings).

owl_link_request(isKBSatisfiable(KB),element('IsKBSatisfiable',[kb=KB],[])).
owl_link_request(isKBStructurallyConsistent(KB),element('IsKBStructurallyConsistent',[kb=KB],[])).
owl_link_request(isTBoxConsistent(KB),element('IsTBoxConsistent',[kb=KB],[])).
owl_link_request(loadOntology(KB,IRIMappings),element('LoadOntology',[kb=KB],EIRIMappings)) :-
	options_to_elements(IRIMappings,EIRIMappings).


%      <!-- reasoner invocation -->
owl_link_request(classify(KB),element('Classify',[kb=KB],[])).
owl_link_request(realize(KB),element('Realize',[kb=KB],[])).


%      <!-- Ask-RetrieveingKBEntities -->
owl_link_request(getAllAnnotationProperties(KB),element('GetAllAnnotationProperties',[kb=KB],[])).
owl_link_request(getAllObjectProperties(KB),element('GetAllObjectProperties',[kb=KB],[])).
owl_link_request(getAllDatatypes(KB),element('GetAllDatatypes',[kb=KB],[])).
owl_link_request(getAllIndividuals(KB),element('GetAllIndividuals',[kb=KB],[])).
owl_link_request(getAllDataProperties(KB),element('GetAllDataProperties',[kb=KB],[])).
owl_link_request(getAllClasses(KB),element('GetAllClasses',[kb=KB],[])).


%      <!-- Ask-ClassAsks-->
owl_link_request(isClassSatisfiable(KB,Class),element('IsClassSatisfiable',[kb=KB],[ClassXML])) :-
	desc_xml(_,Class,ClassXML).

owl_link_request(isClassSubsumedBy(KB,Class1,Class2),element('IsClassSubsumedBy',[kb=KB],[ClassXML1,ClassXML2])) :-
	desc_xml(_,Class1,ClassXML1),
	desc_xml(_,Class2,ClassXML2).
owl_link_request(areClassesDisjoint(KB,Classes),element('AreClassesDisjoint',[kb=KB],EClasses)) :-
	axioms_elts(_,Classes,EClasses).
owl_link_request(areClassesEquivalent(KB,Classes),element('AreClassesEquivalent',[kb=KB],EClasses)) :-
	axioms_elts(_,Classes,EClasses).

%      <!-- Ask-ClassQueries -->
owl_link_request(getSubClasses(KB,Class),element('GetSubClasses',[kb=KB],[ClassXML])) :-
	desc_xml(_,Class,ClassXML).
owl_link_request(getSuperClasses(KB,Class),element('GetSuperClasses',[kb=KB],[ClassXML])) :-
	desc_xml(_,Class,ClassXML).
	
%      <!-- Ask-ClassHierarchy -->
owl_link_request(getEquivalentClasses(KB,Class),element('GetEquivalentClasses',[kb=KB],ClassElement)) :-
	(   nonvar(Class) -> desc_xml(_,Class,ClassXML), ClassElement=[ClassXML] ; ClassElement= []).
owl_link_request(getSubClassHierarchy(KB,Class),element('GetSubClassHierarchy',[kb=KB],ClassElement)) :-
	(   nonvar(Class) -> desc_xml(_,Class,ClassXML), ClassElement=[ClassXML] ; ClassElement= []).


	
%      <!-- Ask-Individuals -->
owl_link_request(areIndividualsEquivalent(KB,Individuals),element('AreIndividualsEquivalent',[kb=KB],EIndividuals)) :-
	axioms_elts(_,Individuals,EIndividuals).
owl_link_request(areIndividualsDisjoint(KB,Individuals),element('AreIndividualsDisjoint',[kb=KB],EIndividuals)) :-
	axioms_elts(_,Individuals,EIndividuals).
owl_link_request(isInstanceOf(KB,Individual,Direct),element('IsInstanceOf',[kb=KB,direct=Direct],IndividualXML)) :-	
	axiom_xml(_,Individual,IndividualXML).

%      <!-- Ask-IndividuualClassQuerysynsets -->
owl_link_request(getTypes(KB,Individual,Direct),element('GetTypes',[kb=KB,direct=Direct],IndividualXML)) :-	
	axiom_xml(_,Individual,IndividualXML).
owl_link_request(getFlattenTypes(KB,Individual,Direct),element('GetFlattenTypes',[kb=KB,direct=Direct],IndividualXML)) :-	
	axiom_xml(_,Individual,IndividualXML).
owl_link_request(getEquivalentIndividuals(KB,Individual),
		 element('GetEquivalentIndividuals',[kb=KB],IndividualXML)) :-	
	axiom_xml(_,Individual,IndividualXML).
owl_link_request(getDisjointIndividuals(KB,Individual),
		 element('GetDisjointIndividuals',[kb=KB],IndividualXML)) :-	
	axiom_xml(_,Individual,IndividualXML).
owl_link_request(getFlattenDisjointIndividuals(KB,Individual),
		 element('GetFlattenDisjointIndividuals',[kb=KB],IndividualXML)) :-	
	axiom_xml(_,Individual,IndividualXML).

      
%      <!-- Ask-IndividualPropertyQueries -->
owl_link_request(getObjectPropertiesOfSource(KB,Individual,Negative),
		 element('GetObjectPropertiesOfSource',[kb=KB,negative=Negative],IndividualXML)) :-	
	axiom_xml(_,Individual,IndividualXML).
owl_link_request(getObjectPropertiesBetween(KB,I1,I2,Negative),
		 element('GetObjectPropertiesBetween',[kb=KB,negative=Negative],IXML)) :-	
	axiom_xml(_,I1,I1XML),
	axiom_xml(_,I2,I2XML),
	append(I1XML,I2XML,IXML).
owl_link_request(getObjectPropertiesOfFiller(KB,Individual,Negative),
		 element('GetObjectPropertiesOfFiller',[kb=KB,negative=Negative],IndividualXML)) :-	
	axiom_xml(_,Individual,IndividualXML).

%      <!-- Ask-IndividualDataPropertyQueries -->
owl_link_request(getDataPropertiesOfSource(KB,Individual),
		 element('GetDataPropertiesOfSource',[kb=KB],IndividualXML)) :-	
	axiom_xml(_,Individual,IndividualXML).
owl_link_request(getDataPropertiesBetween(KB,I1,Literal,Negative),
		 element('GetDataPropertiesBetween',[kb=KB,negative=Negative],ILXML)) :-	
	axiom_xml(_,I1,I1XML),
	axiom_xml(_,Literal,LiteralXML),
	append(I1XML,LiteralXML,ILXML).
owl_link_request(getDataPropertiesOfLiteral(KB,Literal,Negative),
		 element('GetDataPropertiesOfLiteral',[kb=KB,negative=Negative],LiteralXML)) :-	
	axiom_xml(_,Literal,LiteralXML).

%      <!-- Ask-IndividualIndividualQueries -->
owl_link_request(getInstances(KB,Class),[element('GetInstances',[kb=KB],ClassXML)]) :-
	axiom_xml(_,Class,ClassXML).
owl_link_request(getObjectPropertyFillers(KB,Individual,ObjectProperty,Negative),
		 element('GetObjectPropertyFillers',[kb=KB,negative=Negative],[IndividualXML,ObjectPropertyXML])) :-	
	axiom_xml(_,Individual,IndividualXML),
	axiom_xml(_,ObjectProperty,ObjectPropertyXML).

owl_link_request(getObjectPropertySources(KB,Individual,ObjectProperty,Negative),
		 element('GetObjectPropertySources',[kb=KB,negative=Negative],[IndividualXML,ObjectPropertyXML])) :-	
	axiom_xml(_,Individual,IndividualXML),
	axiom_xml(_,ObjectProperty,ObjectPropertyXML).

/* TODO rest of requests...
      <!-- Ask-IndividualIndividualQueriesFlatten-->
      <xsd:element ref="ol:GetFlattenInstances" />
      <xsd:element ref="ol:GetFlattenObjectPropertyFillers" />
      <xsd:element ref="ol:GetFlattenObjectPropertySources" />
      <!-- Ask-IndividualIndividualDataQueriesSynsets-->
      <xsd:element ref="ol:GetDataPropertyFillers" />
      <xsd:element ref="ol:GetDataPropertySources" />
      <!-- Ask-IndividualIndividualDataQueriesFlatten -->
      <xsd:element ref="ol:GetFlattenDataPropertySources" />
      <!-- Ask-ObjectPropQueries -->
      <xsd:element ref="ol:GetSubObjectProperties" />
      <xsd:element ref="ol:GetSuperObjectProperties" />
      <xsd:element ref="ol:GetEquivalentObjectProperties" />
      <xsd:element ref="ol:GetDisjointObjectProperties" />
      <!--  Ask-ObjectPropHierarchy -->
      <xsd:element ref="ol:GetSubObjectPropertyHierarchy" />
      <!--  Ask-ObjectPropAsks -->
      <xsd:element ref="ol:AreObjectPropertiesEquivalent" />
      <xsd:element ref="ol:IsObjectPropertySubsumedBy" />
      <xsd:element ref="ol:IsObjectPropertySatisfiable" />
      <xsd:element ref="ol:AreObjectPropertiesDisjoint" />
      <xsd:element ref="ol:IsObjectPropertySymmetric" />
      <xsd:element ref="ol:IsObjectPropertyTransitive" />
      <xsd:element ref="ol:IsObjectPropertyFunctional" />
      <xsd:element ref="ol:IsObjectPropertyInverseFunctional" />
      <xsd:element ref="ol:IsObjectPropertyReflexive" />
      <xsd:element ref="ol:IsObjectPropertyIrreflexive" />
      <xsd:element ref="ol:IsObjectPropertyAsymmetric" />
      <!--  Ask-DataPropAsks -->
      <xsd:element ref="ol:AreDataPropertiesEquivalent" />
      <xsd:element ref="ol:IsDataPropertySubsumedBy" />
      <xsd:element ref="ol:IsDataPropertySatisfiable" />
      <xsd:element ref="ol:AreDataPropertiesDisjoint" />
      <xsd:element ref="ol:IsDataPropertyFunctional" />
      <!-- Ask-DataPropQueries -->
      <xsd:element ref="ol:GetSubDataProperties" />
      <xsd:element ref="ol:GetSuperDataProperties" />
      <!-- disjointness, equivalence -->
      <xsd:element ref="ol:GetEquivalentDataProperties" />
      <xsd:element ref="ol:GetDisjointDataProperties" />
      <!--  Ask-DataPropHierarchy -->
      <xsd:element ref="ol:GetSubDataPropertyHierarchy" />
      <xsd:element ref="ol:AreIndividualsRelated" />
      <xsd:element ref="ol:IsIndividualRelatedWithLiteral" />
*/
owl_link_request(tell(KB,Axioms),
		 element('Tell',[kb=KB],AxiomsXML)) :-	
		  axioms_elts(_,Axioms,AxiomsXML).




options_to_elements([],[]).
options_to_elements([O|Rest],[element(T,Args,[])|RestXML]) :-
	O =.. [T|Args],
	options_to_elements(Rest,RestXML).


options_to_literals([],[]).
options_to_literals([O|Rest],[element('Literal',[],[O])|RestXML]) :-
	options_to_literals(Rest,RestXML).


% dig_ask_response(+ASKQuery, +ReasonerResult, ?Result)
%
%	 Lower level predicate. It converts the XML
%	 representation (ReasonerResult) of the responses to an ASK
%	 query to a list representation of the Results based on DIGs
%	 response language.


dig_ask_response(allConceptNames,element(conceptSet,_,Synonyms),Result) :-!,
	maplist(map_synonyms(concept),Synonyms,Result1),flatten(Result1,Result).	
dig_ask_response(allRoleNames,element(roleSet,_,Synonyms),Result) :-!,
	maplist(map_synonyms(role),Synonyms,Result1),flatten(Result1,Result).		
dig_ask_response(allIndividuals,element(individualSet,_,Individuals),Result) :-!,
	maplist(map_concepts(concept),Individuals,Result).		


dig_ask_response(parents(_),element(conceptSet,_,Synonyms),Result) :-!,
	maplist(map_synonyms(concept),Synonyms,Result1),flatten(Result1,Result).	

dig_ask_response(children(_),element(conceptSet,_,Synonyms),Result) :-!,
	maplist(map_synonyms(concept),Synonyms,Result1),flatten(Result1,Result).	

dig_ask_response(ancestors(_),element(conceptSet,_,Synonyms),Result) :-!,
	maplist(map_synonyms(concept),Synonyms,Result1),flatten(Result1,Result).	

dig_ask_response(descendants(_),element(conceptSet,_,Synonyms),Result) :-!,
	maplist(map_synonyms(concept),Synonyms,Result1),flatten(Result1,Result).	

dig_ask_response(equivalents(_),element(conceptSet,_,Synonyms),Result) :-!,
	maplist(map_synonyms(concept),Synonyms,Result1),flatten(Result1,Result).	


dig_ask_response(instances(_),element(individualSet,_,Individuals),Result) :-!,
	maplist(map_concepts(concept),Individuals,Result).		

dig_ask_response(types(_),element(conceptSet,_,Synonyms),Result) :-!,
	maplist(map_synonyms(concept),Synonyms,Result1),flatten(Result1,Result).	

dig_ask_response(instance(_,_),element(Result,_,_),Result) :- !.

dig_ask_response(roleFillers(_,_),element(individualSet,_,Individuals),Result) :-!,
	maplist(map_concepts(concept),Individuals,Result).	

dig_ask_response(relatedIndividuals(_),element(individualPairSet,_,IndividualPairs),Result) :-!,
	maplist(map_individual_pairs(concept),IndividualPairs,Result).	

dig_ask_response(_,Result,Result) :- !.



%
%	 Mapping predicates to convert between XML representation and
%	 list representation of ask responses.
%

map_concepts(_What,element(catom,[name=X],[]),X) :- !.
map_concepts(_What,element(ratom,[name=X],[]),X) :- !.
map_concepts(_What,element(individual,[name=X],[]),X) :- !.
map_concepts(_What,element(top,[],[]),top) :- !.
map_concepts(_What,element(bottom,[],[]),top) :- !.
map_concepts(_What,X,X).

map_synonyms(What,element(synonyms,_,Set1),Set2) :-
	     maplist(map_concepts(What),Set1,Set2).

map_individual_pairs(What,element(individualPair,_,Set1),Set2) :-
	     maplist(map_concepts(What),Set1,Set2).





% owl_as2dig(+OwlAsTerm,?TellElement) 
%
%	 Predicate to convert a Thea prolog OWL abstract term into
%	 a DIG Tell element ready to be submitted to the DIG
%	 reasoner via a tell request.



%
%  Definition of a class (concept).
%

owl_as2dig(class(C,_,complete,_,[intersectionOf([])]),element(defconcept,[name=C],[])) :- !.

owl_as2dig(class(C,_,complete,_,[]),element(defconcept,[name=C],[])) :- !.


%
%  Definition of a class (concept) and 	equivalent class
%

owl_as2dig(class(C,_,complete,_,[D]),[element(defconcept,[name=C],[]),R1,R2]) :- !,
	owl_as2dig(subclassOf(C,D),R1),
	owl_as2dig(subclassOf(D,C),R2).

%
%  A class completely defined as an intersection of descriptions
%

owl_as2dig(class(C,_,complete,_,DL),[element(defconcept,[name=C],[])|R]) :- !,
	owl_as2dig(subclassOf(C,intersectionOf(DL)),R).


%
%  A class partially defined as a subclass of descriptions
%

owl_as2dig(class(C,_,partial,_,DL),[element(defconcept,[name=C],[])|R]) :- !,
	% subclassOf(C,D)
	maplist(map_subclass_dig(C),DL,R).

%
%  Subclass axiom 
%

owl_as2dig(subclassOf(A,B),R) :- !,     
     owl_as2dig(description(A,_),Rb),
     owl_as2dig(description(B,_),Rh),
     R = element(impliesc,[],[Rb,Rh]).

%
%  Intersection description produces an 'and' of tells
%

owl_as2dig(description(intersectionOf(DL),X),element(and,[],R)):- !,
	owl_as2dig(description_list(DL,X,','),R).


%
%  Union description produces an 'or' of tells
%

owl_as2dig(description(unionOf(DL),X),element(or,[],R)):-!,
	owl_as2dig(description_list(DL,X,';'),R).

%
%  Union description -> not tell
%

owl_as2dig(description(complementOf(D),_),R) :- !,
	owl_as2dig(description(D,_),D1),
	R = element(not,[],[D1]).


%
%  'One of' description -> iset tell element.
%

owl_as2dig(description(oneOf(DL),_),R) :- !,
	owl_as2dig(description_list(DL,_,_),L),
	maplist(map_dig_individuals,L,L1),
	R = element(iset,[],L1).



%
%  Value restriction on property --> some iset
%

owl_as2dig(description(restriction(PropertyID,value(Value)),_),R) :- !, 
	R = element(some,[],[element(ratom,[name=PropertyID],[]),
			     element(iset,[],[element(individual,[name=Value],[])])]).


%
%  Universal qualifier on property --> all(R, E)
%

owl_as2dig(description(restriction(PropertyID,allValuesFrom(Descr)),_),R) :-  !,
	owl_as2dig(description(Descr,_),D1),
	R =  element(all,[], [element(ratom,[name=PropertyID],[]),
			      D1]).

%
%  Existential qualifier on property --> some(R, E)
%

owl_as2dig(description(restriction(PropertyID,someValuesFrom(Descr)),_),R) :-  !,
	owl_as2dig(description(Descr,_),D1),
	R =  element(some,[], [element(ratom,[name=PropertyID],[]),
			      D1]).

%
%  Cardinalities max, min, both 
%

owl_as2dig(description(restriction(PropertyID,maxCardinality(literal(type(_Type,C)))),_),R) :-  !,
	R = element(atmost,[num=C],[element(ratom,[name=PropertyID],[]),element(top,[],[])]).
owl_as2dig(description(restriction(PropertyID,maxCardinality(C)),_),R) :-  !,
	R = element(atmost,[num=C],[element(ratom,[name=PropertyID],[]),element(top,[],[])]).


owl_as2dig(description(restriction(PropertyID,minCardinality(literal(type(_Type,C)))),_),R) :-  !,
	R = element(atleast,[num=C],[element(ratom,[name=PropertyID],[]),element(top,[],[])]).
owl_as2dig(description(restriction(PropertyID,minCardinality(C)),_),R) :-  !,
	R = element(atleast,[num=C],[element(ratom,[name=PropertyID],[]),element(top,[],[])]).


owl_as2dig(description(restriction(PropertyID,cardinality(literal(type(_Type,C)))),_),R) :-  !,
	R = element(and,[],[element(atleast,[num=C],[element(ratom,[name=PropertyID],[]),element(top,[],[])]),
	     element(atmost,[num=C],[element(ratom,[name=PropertyID],[]),element(top,[],[])])]).

owl_as2dig(description(restriction(PropertyID,cardinality(C)),_),R) :-  !,
	R = element(and,[],[element(atleast,[num=C],[element(ratom,[name=PropertyID],[]),element(top,[],[])]),
	     element(atmost,[num=C],[element(ratom,[name=PropertyID],[]),element(top,[],[])])]).


%
%  Description List --> returns a list of description conversions.
%

owl_as2dig(description_list([],_,_),[]) :-!.

owl_as2dig(description_list([Descr|Rest],X,_),[H|Tail]) :-
	owl_as2dig(description(Descr,X),H),!,	
	owl_as2dig(description_list(Rest,X,_),Tail).

%
%  All remaining descriptions translate to a catom element
%

owl_as2dig(description(Any,_),element(catom, [name=Any],[])).


%
%  Property translation. Translates to a set of subproperties, domain,
%  range and property attribute tells that are handled through a set of
%  mapping functions.
%

owl_as2dig(property(PID,_Deprecated,_AnnotationList,PID_SuperList,PTList,PID_DomainList,PID_RangeList),Tells) :- !,
	maplist(map_subproperty_dig(PID),PID_SuperList,L1),
	maplist(map_domain_dig(PID),PID_DomainList,L2),
	maplist(map_range_dig(PID),PID_RangeList,L3),
	process_pt_dig(PID,PTList,L4),
	flatten([L1,L2,L3,L4],Tells).

%
%  Individual translation. Translates to a set of instancof (type),
%  property and role tells that are handled through approrpiate
%  mapping functions.
%

owl_as2dig(individual(IID,_,TypeList,PropertyList),L) :- !,
       maplist(map_instanceof_dig(IID),TypeList,L1),
       maplist(map_property_dig(IID), PropertyList,L2),
       flatten([element(defindividual,[name=IID],[]),L1,L2], L).


%
%  Equivalent Sets translation. Translates to a set of equalc,
%  through approrpiate  mapping functions.
%

owl_as2dig(equivalentSet(S),element(equalc,[],L)) :- !,
       maplist(map_description_dig,S,L).


map_description_dig(C,R) :-
  owl_as2dig(description(C,_),R).

%
%  Mapping of a subclass, calls the subclassOf translation
%

map_subclass_dig(C,D,R) :- 
	owl_as2dig(subclassOf(C,D),R).


%
%  Mapping of a subproperty relation.
%

map_subproperty_dig(PID,SuperP,element(impliesr,[],[element(ratom,[name=PID],[]), 
						    element(ratom,[name=SuperP],[])])).

%
%  Mapping of a domain definition
%

map_domain_dig(PID,Domain,Tell) :-
	owl_as2dig(description(Domain,_),E),
	Tell = element(domain,[],[element(ratom,[name=PID],[]),E]).

%
%  Mapping of a range definition. If a datatype property then either a
%  rangeint or a rangestring. If it is an objectproperty then get the
%  range from the description of the doamin.
%

map_range_dig(PID,Domain,Tell) :-
	(   property(PID,_,_,_,[datatypeProperty,_,_,_,_,_],_,Range),!,
	    (	member('http://www.w3.org/2001/XMLSchema#int',Range),Val = rangeint,! ; Val = rangestring),
	    Tell = element(Val,[],[element(attribute,[name=PID],[])]) ;
	owl_as2dig(description(Domain,_),E),
	Tell = element(range,[],[element(ratom,[name=PID],[]),E])).

%
%  Translate property attributes. Define attribute or role. Create
%  functional, inverse, transitive, symmetric and inverse
%  property tells.
%

process_pt_dig(PID, [Type,F,IF,T,S,iof(Inv)],[Typet, Ft,IFt,Tt,St,INVt]) :-
	(   Type = datatypeProperty, !, Typet = element(defattribute,[name=PID],[]) ; Typet = element(defrole,[name=PID],[])),
	(   nonvar(F), !, Ft = element(functional,[],[element(ratom,[name=PID],[])]) ;  Ft = []),
	(   nonvar(IF), !, IFt = element(inverse,[],[element(ratom,[name=PID],[])])  ; IFt = []),
	(   nonvar(T), !, Tt = element(transitive,[],[element(ratom,[name=PID],[])]) ; Tt = []),
	(   nonvar(S), !, St = element(equalr,[],[element(ratom,[name=PID],[]),
						      element(inverse,[],[element(ratom,[name=PID],[])])]) ; St = []),

	(   nonvar(Inv), !, INVt = element(equalr,[],[element(ratom,[name=PID],[]), 
						      element(inverse,[],[element(ratom,[name=Inv],[])])]) ; INVt = []).

%
%  Mapping of a class membership (instanceof tell).
%

map_instanceof_dig(Instance,Type,Tell) :-
	owl_as2dig(description(Type,_),E),
	Tell = element(instanceof,[],[element(individual,[name=Instance],[]),E]).


%
%  Mapping of an instance property values. For a datatypeproperty
%  value(I, P, V) for an objectproperty related(I, R, I). 
%

map_property_dig(IID, value(P,V), Tells):-	
	(   property(P,_,_,_,[datatypeProperty,_,_,_,_,_],_,Range),! ,
	    (	V = literal(type(_T,V1)),!
	    ; V1 = V 
	    ),
	    (	member('http://www.w3.org/2001/XMLSchema#int',Range),Val = ival,! 
	    ; 
	    Val = sval 
	    ), 
	    Tells = element(value,[],[element(individual,[name=IID],[]),
				      element(attribute,[name=P],[]),
				      element(Val,[],[V1])]) 
	;
	Tells = element(related,[],[element(individual,[name=IID],[]),
				      element(ratom,[name=P],[]),
				      element(individual,[name=V],[])])
	).


map_dig_individuals(X, element(individual,[name=X],[])).
 
