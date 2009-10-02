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
	(   member(response_file=FilenameResponse,Options) -> true ; FilenameResponse = '_thea_owllink_response.xml'),

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
	(   member(no_reasoner,Options) ->
	       open(FilenameResponse,read,St),	    
	       load_structure(St,ResponseXML,[dialect(xml),space(sgml)]),close(St),
	       ResponseXML = [element('ResponseMessage',_,ResponsesXML)],
	       owl_link_response(ResponsesXML,Response)
	;   
	   (   http_post(ReasonerURL,file(Filename),Result,
			 [request_header('Content-Type' = 'text/xml'), 
			  request_header('Content-Length' = RLength1)]) -> 	
	       % write reasoner response to predefined file
	          open(FilenameResponse,write,St),write(St,Result),close(St),
	          % Read response from file into an xml structure
	          open(FilenameResponse,read,St),	    
	          load_structure(St,Response,[dialect(xml),space(sgml)]),close(St),
	       	  ResponseXML = [element('ResponseMessage',_,ResponsesXML)],
	          owl_link_response(ResponsesXML,Response)
	   ; 
	          debug(owllink,'Reasoner error ~w',[ReasonerURL])
	   )
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

owl_link_request(Req,_) :-
	throw(cannot_parse_request(Req)).

% ------------------------------------------------------------

options_to_elements([],[]).
options_to_elements([O|Rest],[element(T,Args,[])|RestXML]) :-
	O =.. [T|Args],
	options_to_elements(Rest,RestXML).


options_to_literals([],[]).
options_to_literals([O|Rest],[element('Literal',[],[O])|RestXML]) :-
	options_to_literals(Rest,RestXML).



% owl_link_response(+ResponseElementList -ResponseTheaTerms)
%
%	 It converts the XML representation of the responses to Thea
%	 term representation of the Results based on OWLLink
%	 response language.

owl_link_response([],[]).
owl_link_response([ResXMLH|ResXMLRest],[ResH|ResRest]) :-
	owl_link_response(ResXMLH,ResH),
	owl_link_response(ResXMLRest,ResRest).


owl_link_response(element('Description',Attrs,Elements),
		  description(Name,MessageO,Descriptions)) :- 
	member(name=Name,Attrs),
	(   member(message=Message,Attrs) -> MessageO = [Message] ; MessageO = []),
	maplist(response_elements,Elements,Descriptions).

owl_link_response(element('KB',Attrs,_Elements), kb(KB,WarningO)) :- 
	member(kb=KB,Attrs),
	(   member(warning=Warning,Attrs) -> WarningO = [Warning] ; WarningO = []).

owl_link_response(element('OK',Attrs,_Elements), ok(WarningO)) :- 
	(   member(warning=Warning,Attrs) -> WarningO = [Warning] ; WarningO = []).

owl_link_response(element('BooleanResponse',Attrs,_Elements), booleanResponse(Result,WarningO)) :- 
	member(result=Result,Attrs),
	(   member(warning=Warning,Attrs) -> WarningO = [Warning] ; WarningO = []).

owl_link_response(element('Settings',Attrs,Elements), settings(WarningO,Settings)) :- 
	(   member(warning=Warning,Attrs) -> WarningO = [Warning] ; WarningO = []),
	maplist(response_elements,Elements,Settings).
	

%% pass all other responses unprocessed for now
%  TODO: handle all remaining responses into Prolog terms.
%  mainly inferred axioms....
owl_link_response(Res,Res). 



response_elements(element(protocolVersion,Attrs,_),protocolVersion(Attrs)) :- !.
response_elements(X,X).




