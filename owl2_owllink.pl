/* -*- Mode: Prolog -*- */

:- module(owl2_owllink,
	  [ owl_link/4 % File
	  ]).

:-use_module(library('http/http_client')).
:-use_module(library('sgml')).
:-use_module(library('sgml_write')).
:-use_module('owl2_xml').


/** <module> Implements the OWLLink interface

OWLLink Language Wrapper

This module is an implementation of the OWLLink interface. It allows
communication between a Prolog program and an OWLLink enabled reasoner.
It is using SWI Prologs HTTP and SGML packages. See more in
owl2_owllink.txt

  @author Vangelis Vassiliadis
  @license GPL

*/

%% owl_link(+ReasonerURL, +Request:list, -Response:list, +Options:list) is det
%
%  Implements OWLLink over HTTP. Sends an OWLLink request to the ReasonerURL and
%  gets its Response. Both Request and Response are lists of Prolog
%  terms. The complete grammar for the terms is given in
%  owl2_owllink.txt

owl_link(ReasonerURL,Request,Response,Options) :-
	owl_link_request_l(Request,RequestXML),
	% write Request to predefined file
	(   member(request_file=Filename,Options) -> true ; Filename = '_thea_owllink_request.xml'),
	(   member(response_file=FilenameResponse,Options) -> true ; FilenameResponse = '_thea_owllink_response.xml'),
	open(Filename,write,St),
	xml_write(St,element('RequestMessage',
			     [xmlns='http://www.owllink.org/owllink#',
			      xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance',
			      xsi:schemaLocation='http://www.owllink.org/owllink# http://www.owllink.org/owllink-20091116.xsd'],
			     RequestXML),[layout(true)]),close(St),
			     % size_file(Filename,_RLength),  size_file is not counting correctly newlines in windows environment.
	(   member(no_reasoner,Options) ->
	       open(FilenameResponse,read,St),
	       load_structure(St,ResponseXML,[dialect(xmlns),space(sgml)]),close(St),
	       ResponseXML = [element('ResponseMessage',_,ResponsesXML)],
	       owl_link_response(ResponsesXML,Response)
	;
	   (   http_post(ReasonerURL,file(Filename),Result,[]) ->
		%	 [request_header('Content-Type' = 'text/xml'),
		%	  request_header('Content-Length' = RLength1)]) ->
	       % write reasoner response to predefined file
	          open(FilenameResponse,write,St),write(St,Result),close(St),
	          % Read response from file into an xml structure
	          open(FilenameResponse,read,St),
	          load_structure(St,ResponseXML,[dialect(xmlns),space(sgml)]),close(St),
	       	  ResponseXML = [element(_:'ResponseMessage',_,ResponsesXML)],
	          owl_link_response_l(ResponsesXML,Response)
	   ;
	          debug(owllink,'Reasoner error ~w',[ReasonerURL])
	   )
	).


owl_link_request_l([],[]).
owl_link_request_l([Req1|ReqRest],[Res1|ResRest]) :-
	owl_link_request(Req1,Res1),
	owl_link_request_l(ReqRest,ResRest).



%% owl_link_request(+TheaReq, -OwlLinkReq) is semidet
%
%	 Convert a request from Thea / OWL2 to OwlLink XML Schema format

% 	 <!--  Management -->
owl_link_request(getDescription,element('GetDescription',[],[])) :-!.
owl_link_request(getSettings(KB),element('GetSettings',[kb=KB],[])) :- !.
owl_link_request(getPrefixes(KB),element('GetPrefixes',[kb=KB],[])) :- !.

owl_link_request(createKB(KB_Name_Attrs,Prefixes),element('CreateKB',KB_Name_Attrs,EPrefixes)):-
	options_to_elements(Prefixes,EPrefixes), !.
owl_link_request(releaseKB(KB),element('ReleaseKB',[kb=KB],[])) :-!.
owl_link_request(set(KB,Key,Settings),element('Set',[kb=KB,key=Key],LiteralSettings)) :-
	options_to_literals(Settings,LiteralSettings),!.

owl_link_request(isKBSatisfiable(KB),element('IsKBSatisfiable',[kb=KB],[])) :- !.
owl_link_request(isKBStructurallyConsistent(KB),element('IsKBStructurallyConsistent',[kb=KB],[])) :-!.
owl_link_request(isTBoxConsistent(KB),element('IsTBoxConsistent',[kb=KB],[])) :- !.
owl_link_request(loadOntologies(KB,IRIs,IRIMappings,Imports),element('LoadOntologies',[kb=KB,considerImports=Imports],Elements)) :-
	options_to_elements(IRIMappings,MappingsEL),
	options_to_elements(IRIs,IRIsEL),
	merge(IRIsEL,MappingsEL,Elements),!.

%      <!-- reasoner invocation -->
owl_link_request(classify(KB),element('Classify',[kb=KB],[])) :- !.
owl_link_request(realize(KB),element('Realize',[kb=KB],[])) :- !.

%      <!-- general entailment request -->
owl_link_request(isEntailed(KB,Axioms,Options),element('IsEntailed',[kb=KB|Options],AxiomsXML)) :-
	axioms_elts(_,Axioms,AxiomsXML),!.
owl_link_request(isEntailedDirect(KB,Axioms,Options),element('IsEntailedDirect',[kb=KB|Options],AxiomsXML)) :-
	axioms_elts(_,Axioms,AxiomsXML),!.


%      <!-- Ask-RetrieveingKBEntities -->
owl_link_request(getAllAnnotationProperties(KB),element('GetAllAnnotationProperties',[kb=KB],[])) :-!.
owl_link_request(getAllObjectProperties(KB),element('GetAllObjectProperties',[kb=KB],[])) :- !.
owl_link_request(getAllDatatypes(KB),element('GetAllDatatypes',[kb=KB],[])) :- !.
owl_link_request(getAllIndividuals(KB),element('GetAllIndividuals',[kb=KB],[])) :- !.
owl_link_request(getAllDataProperties(KB),element('GetAllDataProperties',[kb=KB],[])) :- !.
owl_link_request(getAllClasses(KB),element('GetAllClasses',[kb=KB],[])) :- !.


%      <!-- Ask-ClassAsks-->
owl_link_request(isClassSatisfiable(KB,Class),element('IsClassSatisfiable',[kb=KB],[ClassXML])) :-
	desc_xml(_,Class,ClassXML),!.
owl_link_request(isClassSubsumedBy(KB,Class1,Class2),element('IsClassSubsumedBy',[kb=KB],[ClassXML1,ClassXML2])) :-
	desc_xml(_,Class1,ClassXML1),
	desc_xml(_,Class2,ClassXML2),!.
owl_link_request(areClassesDisjoint(KB,Classes),element('AreClassesDisjoint',[kb=KB],EClasses)) :-
	axioms_elts(_,Classes,EClasses),!.
owl_link_request(areClassesEquivalent(KB,Classes),element('AreClassesEquivalent',[kb=KB],EClasses)) :-
	axioms_elts(_,Classes,EClasses),!.

%      <!-- Ask-ClassQueries -->
owl_link_request(getSubClasses(KB,Class),element('GetSubClasses',[kb=KB],[ClassXML])) :-
	desc_xml(_,Class,ClassXML),!.
owl_link_request(getSubClasses(KB,Class,Direct),element('GetSubClasses',[kb=KB,direct=Direct],[ClassXML])) :-
	desc_xml(_,Class,ClassXML),!.

owl_link_request(getSuperClasses(KB,Class),element('GetSuperClasses',[kb=KB],[ClassXML])) :-
	desc_xml(_,Class,ClassXML),!.
owl_link_request(getSuperClasses(KB,Class,Direct),element('GetSuperClasses',[kb=KB,direct=Direct],[ClassXML])) :-
	desc_xml(_,Class,ClassXML),!.
owl_link_request(getDisjointClasses(KB,Class),element('GetDisjointClasses',[kb=KB],ClassElement)) :-
	(   nonvar(Class) -> desc_xml(_,Class,ClassXML), ClassElement=[ClassXML] ; ClassElement= []),!.

%      <!-- Ask-ClassHierarchy -->
owl_link_request(getEquivalentClasses(KB,Class),element('GetEquivalentClasses',[kb=KB],ClassElement)) :-
	(   nonvar(Class) -> desc_xml(_,Class,ClassXML), ClassElement=[ClassXML] ; ClassElement= []),!.
owl_link_request(getSubClassHierarchy(KB,Class),element('GetSubClassHierarchy',[kb=KB],ClassElement)) :-
	(   nonvar(Class) -> desc_xml(_,Class,ClassXML), ClassElement=[ClassXML] ; ClassElement= []),!.

%      <!-- Ask-IndividuualClassQuerysynsets -->
owl_link_request(getTypes(KB,Individual,Direct),
		 element('GetTypes',[kb=KB,direct=Direct],[IndividualXML])) :-
	axiom_xml(_,Individual,IndividualXML),!.
owl_link_request(getFlattenedTypes(KB,Individual,Direct),
		 element('GetFlattenedTypes',[kb=KB,direct=Direct],[IndividualXML])) :-
	axiom_xml(_,Individual,IndividualXML),!.
owl_link_request(getSameIndividuals(KB,Individual,Direct),
		 element('GetSameIndividuals',[kb=KB,direct=Direct],[IndividualXML])) :-
	axiom_xml(_,Individual,IndividualXML),!.
owl_link_request(getDifferentIndividuals(KB,Individual,Direct),
		 element('GetDifferentIndividuals',[kb=KB,direct=Direct],[IndividualXML])) :-
	axiom_xml(_,Individual,IndividualXML),!.
owl_link_request(getFlattenedDifferentIndividuals(KB,Individual,Direct),
		 element('GetFlattenedDifferentIndividuals',[kb=KB,direct=Direct],[IndividualXML])) :-
	axiom_xml(_,Individual,IndividualXML),!.

owl_link_request(getEquivalentIndividuals(KB,Individual),
		 element('GetEquivalentIndividuals',[kb=KB],IndividualXML)) :-
	axiom_xml(_,Individual,IndividualXML),!.
owl_link_request(getDisjointIndividuals(KB,Individual),
		 element('GetDisjointIndividuals',[kb=KB],IndividualXML)) :-
	axiom_xml(_,Individual,IndividualXML),!.
owl_link_request(getFlattenDisjointIndividuals(KB,Individual),
		 element('GetFlattenDisjointIndividuals',[kb=KB],IndividualXML)) :-
	axiom_xml(_,Individual,IndividualXML),!.


%      <!-- Ask-IndividualPropertyQueries -->
owl_link_request(getObjectPropertiesOfSource(KB,Individual,Negative),
		 element('GetObjectPropertiesOfSource',[kb=KB,negative=Negative],[IndividualXML])) :-
	axiom_xml(_,Individual,IndividualXML),!.
owl_link_request(getObjectPropertiesBetween(KB,I1,I2,Negative),
		 element('GetObjectPropertiesBetween',[kb=KB,negative=Negative],[I1XML,I2XML])) :-
	axiom_xml(_,I1,I1XML),
	axiom_xml(_,I2,I2XML),!.
owl_link_request(getObjectPropertiesOfTarget(KB,Individual,Negative),
		 element('GetObjectPropertiesOfTarget',[kb=KB,negative=Negative],[IndividualXML])) :-
	axiom_xml(_,Individual,IndividualXML),!.

%      <!-- Ask-IndividualDataPropertyQueries -->
owl_link_request(getDataPropertiesOfSource(KB,Individual,Negative),
		 element('GetDataPropertiesOfSource',[kb=KB,negative=Negative],[IndividualXML])) :-
	axiom_xml(_,Individual,IndividualXML),!.
owl_link_request(getDataPropertiesBetween(KB,I1,Literal,Negative),
		 element('GetDataPropertiesBetween',[kb=KB,negative=Negative],[IXML,LiteralXML])) :-
	axiom_xml(_,I1,IXML),
	axiom_xml(_,Literal,LiteralXML),!.
owl_link_request(getDataPropertiesOfLiteral(KB,Literal,Negative),
		 element('GetDataPropertiesOfLiteral',[kb=KB,negative=Negative],LiteralXML)) :-
	axiom_xml(_,Literal,LiteralXML),!.

%      <!-- Ask-IndividualIndividualQueries -->
owl_link_request(getInstances(KB,Class,Direct),element('GetInstances',[kb=KB,direct=Direct],[ClassXML])) :-
	axiom_xml(_,Class,ClassXML),!.
owl_link_request(getObjectPropertyTargets(KB,ObjectProperty,Individual,Negative),
		 element('GetObjectPropertyTargets',[kb=KB,negative=Negative],[ObjectPropertyXML,IndividualXML])) :-
	axiom_xml(_,Individual,IndividualXML),
	axiom_xml(_,ObjectProperty,ObjectPropertyXML),!.
owl_link_request(getObjectPropertySources(KB,ObjectProperty,Individual,Negative),
		 element('GetObjectPropertySources',[kb=KB,negative=Negative],[ObjectPropertyXML,IndividualXML])) :-
	axiom_xml(_,Individual,IndividualXML),
	axiom_xml(_,ObjectProperty,ObjectPropertyXML),!.

%      <!-- Ask-IndividualIndividualQueriesFlatten-->
owl_link_request(getFlattenedInstances(KB,Class,Direct),element('GetFlattenedInstances',[kb=KB,direct=Direct],[ClassXML])) :-
	axiom_xml(_,Class,ClassXML),!.
owl_link_request(getFlattenedObjectPropertyTargets(KB,ObjectProperty,Individual,Negative),
		 element('GetFlattenedObjectPropertyTargets',[kb=KB,negative=Negative],[ObjectPropertyXML,IndividualXML])) :-
	axiom_xml(_,Individual,IndividualXML),
	axiom_xml(_,ObjectProperty,ObjectPropertyXML),!.
owl_link_request(getFlattenedObjectPropertySources(KB,ObjectProperty,Individual,Negative),
		 element('GetFlattenedObjectPropertySources',[kb=KB,negative=Negative],[ObjectPropertyXML,IndividualXML])) :-
	axiom_xml(_,Individual,IndividualXML),
	axiom_xml(_,ObjectProperty,ObjectPropertyXML),!.

%      <!-- Ask-IndividualIndividualDataQueriesSynsets-->
owl_link_request(getDataPropertyTargets(KB,DataProperty,Individual,Negative),
		 element('GetDataPropertyTargets',[kb=KB,negative=Negative],[DataPropertyXML,IndividualXML])) :-
	axiom_xml(_,Individual,IndividualXML),
	axiom_xml(_,DataProperty,DataPropertyXML),!.
owl_link_request(getDataPropertySources(KB,DataProperty,Literal,Negative),
		 element('GetDataPropertySources',[kb=KB,negative=Negative],[DataPropertyXML,LiteralXML])) :-
	axiom_xml(_,Literal,LiteralXML),
	axiom_xml(_,DataProperty,DataPropertyXML),!.

%       <!-- Ask-IndividualIndividualDataQueriesFlatten -->
owl_link_request(getFlattenedDataPropertySources(KB,ObjectProperty,Literal,Negative),
		 element('GetFlattenedDataPropertySources',[kb=KB,negative=Negative],[ObjectPropertyXML,LiteralXML])) :-
	axiom_xml(_,Literal,LiteralXML),
	axiom_xml(_,ObjectProperty,ObjectPropertyXML),!.

%      <!-- Ask-ObjectPropQueries -->
owl_link_request(getSubObjectProperties(KB,ObjectProperty,Direct),
		 element('GetSubObjectProperties',[kb=KB,direct=Direct],[ObjectPropertyXML])) :-
	axiom_xml(_,ObjectProperty,ObjectPropertyXML),!.
owl_link_request(getSuperObjectProperties(KB,ObjectProperty,Direct),
		 element('GetSuperObjectProperties',[kb=KB,direct=Direct],[ObjectPropertyXML])) :-
	axiom_xml(_,ObjectProperty,ObjectPropertyXML),!.
owl_link_request(getEquivalentObjectProperties(KB,ObjectProperty,Direct),
		 element('GetEquivalentObjectProperties',[kb=KB,direct=Direct],[ObjectPropertyXML])) :-
	axiom_xml(_,ObjectProperty,ObjectPropertyXML),!.
owl_link_request(getDisjointObjectProperties(KB,ObjectProperty,Direct),
		 element('GetDisjointObjectProperties',[kb=KB,direct=Direct],[ObjectPropertyXML])) :-
	axiom_xml(_,ObjectProperty,ObjectPropertyXML),!.

%      <!--  Ask-ObjectPropHierarchy -->
owl_link_request(getSubObjectPropertyHierarchy(KB,ObjectProperty),
		 element('GetSubObjectPropertyHierarchy',[kb=KB],[ObjectPropertyXML])) :-
	axiom_xml(_,ObjectProperty,ObjectPropertyXML),!.


	% <!--  Ask-ObjectPropAsks -->
owl_link_request(isObjectPropertySatisfiable(KB,ObjectProperty),
		 element('IsObjectPropertySatisfiable',[kb=KB],[ObjectPropertyXML])) :-
	axiom_xml(_,ObjectProperty,ObjectPropertyXML),!.

	% <!--  Ask-DataPropAsks -->
owl_link_request(isDataPropertySatisfiable(KB,DataProperty),
		 element('IsDataPropertySatisfiable',[kb=KB],[DataPropertyXML])) :-
	axiom_xml(_,DataProperty,DataPropertyXML),!.

	% <!-- Ask-DataPropQueries -->
owl_link_request(getSubDataProperties(KB,DataProperty,Direct),
		 element('GetSubDataProperties',[kb=KB,direct=Direct],[DataPropertyXML])) :-
	axiom_xml(_,DataProperty,DataPropertyXML),!.
owl_link_request(getSuperDataProperties(KB,DataProperty,Direct),
		 element('GetSuperDataProperties',[kb=KB,direct=Direct],[DataPropertyXML])) :-
	axiom_xml(_,DataProperty,DataPropertyXML),!.

	% <!-- disjointness, equivalence -->
owl_link_request(getEquivalentDataProperties(KB,DataProperty,Direct),
		 element('GetEquivalentDataProperties',[kb=KB,direct=Direct],[DataPropertyXML])) :-
	axiom_xml(_,DataProperty,DataPropertyXML),!.
owl_link_request(getDisjointDataProperties(KB,DataProperty,Direct),
		 element('GetDisjointDataProperties',[kb=KB,direct=Direct],[DataPropertyXML])) :-
	axiom_xml(_,DataProperty,DataPropertyXML),!.

	% <!--  Ask-DataPropHierarchy -->
owl_link_request(getSubDataPropertyHierarchy(KB,DataProperty),
		 element('GetSubDataPropertyHierarchy',[kb=KB],[DataPropertyXML])) :-
	axiom_xml(_,DataProperty,DataPropertyXML),!.

owl_link_request(tell(KB,Axioms),
		 element('Tell',[kb=KB],AxiomsXML)) :-
		  axioms_elts(_,Axioms,AxiomsXML),!.

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

owl_link_response_l([],[]).
owl_link_response_l([ResXMLH|ResXMLRest],[ResH|ResRest]) :-
	owl_link_response(ResXMLH,ResH),
	owl_link_response_l(ResXMLRest,ResRest).


owl_link_response(element(_:'Description',Attrs,Elements),
		  description(Name,MessageO,Descriptions)) :-
	member(name=Name,Attrs),
	(   member(message=Message,Attrs) -> MessageO = [Message] ; MessageO = []),
	maplist(response_elements,Elements,Descriptions),!.

owl_link_response(element(_:'KB',Attrs,_Elements), kb(KB,WarningO)) :-
	member(kb=KB,Attrs),
	(   member(warning=Warning,Attrs) -> WarningO = [Warning] ; WarningO = []),!.

owl_link_response(element(_:'Settings',Attrs,Elements), settings(WarningO,Settings)) :-
	(   member(warning=Warning,Attrs) -> WarningO = [Warning] ; WarningO = []),
	maplist(response_elements,Elements,Settings),!.

owl_link_response(element(_:'Prefixes',Attrs,SettingsE), settings(WarningO,Settings)) :-
	(   member(warning=Warning,Attrs) -> WarningO = [Warning] ; WarningO = []),
	maplist(response_elements,SettingsE,Settings),!.


owl_link_response(element(_:'OK',Attrs,_Elements), ok(WarningO)) :-
	(   member(warning=Warning,Attrs) -> WarningO = [Warning] ; WarningO = []),!.

owl_link_response(element(_:'SetOfAnnotationProperties',Attrs,Elements),
		  annotationProperties(WarningO,AnnotationProperties)) :-
	(   member(warning=Warning,Attrs) -> WarningO = [Warning] ; WarningO = []),
	maplist(response_elements,Elements,AnnotationProperties),!.

owl_link_response(element(_:'SetOfClasses',Attrs,ClassLE), setOfClasses(WarningO,ClassL)) :-
	(   member(warning=Warning,Attrs) -> WarningO = [Warning] ; WarningO = []),
	maplist(xml_desc(_),ClassLE,ClassL),!.

owl_link_response(element(_:'SetOfIndividuals',Attrs,IndLE), setOfIndividuals(WarningO,IndL)) :-
	(   member(warning=Warning,Attrs) -> WarningO = [Warning] ; WarningO = []),
	maplist(xml_axiom(_),IndLE,IndL),!.

	% untested
owl_link_response(element(_:'SetOfObjectProperties',Attrs,ObjectPropertyLE), setOfLiterals(WarningO,ObjectPropertyL)) :-
	(   member(warning=Warning,Attrs) -> WarningO = [Warning] ; WarningO = []),
	maplist(xml_axiom(_),ObjectPropertyLE,ObjectPropertyL),!.

	% untested
owl_link_response(element(_:'SetOfDataProperties',Attrs,DataPropertyLE), setOfLiterals(WarningO,DataPropertyL)) :-
	(   member(warning=Warning,Attrs) -> WarningO = [Warning] ; WarningO = []),
	maplist(xml_axiom(_),DataPropertyLE,DataPropertyL),!.

	% untested
owl_link_response(element(_:'SetOfLiterals',Attrs,LitLE), setOfLiterals(WarningO,LitL)) :-
	(   member(warning=Warning,Attrs) -> WarningO = [Warning] ; WarningO = []),
	maplist(xml_axiom(_),LitLE,LitL),!.

	% untested
owl_link_response(element(_:'SetOfDatatypes',Attrs,DatatypeLE), setOfLiterals(WarningO,DatatypeL)) :-
	(   member(warning=Warning,Attrs) -> WarningO = [Warning] ; WarningO = []),
	maplist(xml_axiom(_),DatatypeLE,DatatypeL),!.


owl_link_response(element(_:'BooleanResponse',Attrs,_Elements), booleanResponse(Result,WarningO)) :-
	member(result=Result,Attrs),
	(   member(warning=Warning,Attrs) -> WarningO = [Warning] ; WarningO = []),!.

	% untested
owl_link_response(element(_:'StringReponse',Attrs,_), stringReponse(Result,WarningO)) :-
	member(result=Result,Attrs),
	(   member(warning=Warning,Attrs) -> WarningO = [Warning] ; WarningO = []),!.


owl_link_response(element(_:'ClassSynsets',Attrs,SynsetsLE), classSynsets(WarningO,SynsetsL)) :-
	(   member(warning=Warning,Attrs) -> WarningO = [Warning] ; WarningO = []),
	maplist(response_elements,SynsetsLE,SynsetsL),!.

owl_link_response(element(_:'Classes',Attrs,ClassesLE), classes(WarningO,ClassesL)) :-
	(   member(warning=Warning,Attrs) -> WarningO = [Warning] ; WarningO = []),
	maplist(xml_desc(_),ClassesLE,ClassesL),!.

owl_link_response(element(_:'ClassHierarchy',Attrs,[SynsetLE|PairElements]), classHierarchy(WarningO,SynsetL,Pairs)) :-
	(   member(warning=Warning,Attrs) -> WarningO = [Warning] ; WarningO = []),
	response_elements(SynsetLE,SynsetL),
	maplist(response_elements,PairElements,Pairs),!.

owl_link_response(element(_:'SetOfDataPropertySynsets',Attrs,SynsetsLE), dataPropertySynsets(WarningO,SynsetsL)) :-
	(   member(warning=Warning,Attrs) -> WarningO = [Warning] ; WarningO = []),
	maplist(response_elements,SynsetsLE,SynsetsL),!.

owl_link_response(element(_:'DataPropertySynonyms',Attrs,SynonymsLE), dataPropertySynonyms(WarningO,SynonymsL)) :-
	(   member(warning=Warning,Attrs) -> WarningO = [Warning] ; WarningO = []),
	maplist(xml_axiom(_),SynonymsLE,SynonymsL),!.


owl_link_response(element(_:'SetOfIndividualSynsets',Attrs,SynsetsLE), individualSynsets(WarningO,SynsetsL)) :-
	(   member(warning=Warning,Attrs) -> WarningO = [Warning] ; WarningO = []),
	maplist(response_elements,SynsetsLE,SynsetsL),!.

owl_link_response(element(_:'IndividualSynonyms',Attrs,SynonymsLE), individualSynonyms(WarningO,SynonymsL)) :-
	(   member(warning=Warning,Attrs) -> WarningO = [Warning] ; WarningO = []),
	maplist(xml_axiom(_),SynonymsLE,SynonymsL),!.

owl_link_response(element(_:'ObjectPropertyHierarchy',Attrs,[SynsetLE|PairElements]),
		  objectPropertyHierarchy(WarningO,SynsetL,Pairs)) :-
	(   member(warning=Warning,Attrs) -> WarningO = [Warning] ; WarningO = []),
	response_elements(SynsetLE,SynsetL),
	maplist(response_elements,PairElements,Pairs),!.

owl_link_response(element(_:'SetOfObjectPropertySynsets',Attrs,SynsetsLE), objectPropertySynsets(WarningO,SynsetsL)) :-
	(   member(warning=Warning,Attrs) -> WarningO = [Warning] ; WarningO = []),
	maplist(response_elements,SynsetsLE,SynsetsL),!.

owl_link_response(element(_:'ObjectPropertySynsets',Attrs,SynsetsLE), objectPropertySynsets(WarningO,SynsetsL)) :-
	(   member(warning=Warning,Attrs) -> WarningO = [Warning] ; WarningO = []),
	maplist(response_elements,SynsetsLE,SynsetsL),!.

owl_link_response(element(_:'DataPropertySynsets',Attrs,SynsetsLE), dataPropertySynsets(WarningO,SynsetsL)) :-
	(   member(warning=Warning,Attrs) -> WarningO = [Warning] ; WarningO = []),
	maplist(response_elements,SynsetsLE,SynsetsL),!.

owl_link_response(element(_:'DataPropertyHierarchy',Attrs,[SynsetLE|PairElements]),
		  dataPropertyHierarchy(WarningO,SynsetL,Pairs)) :-
	(   member(warning=Warning,Attrs) -> WarningO = [Warning] ; WarningO = []),
	response_elements(SynsetLE,SynsetL),
	maplist(response_elements,PairElements,Pairs),!.


owl_link_response(element(_:'Error',[error=Message],_Elements), error(Message)) :- !.
owl_link_response(element(_:'SyntaxError',[error=Message],_Elements), syntaxError(Message)) :- !.
owl_link_response(element(_:'KBError',[error=Message],_Elements), kbError(Message)) :- !.
owl_link_response(element(_:'SemanticError',[error=Message],_Elements), semanticError(Message)) :- !.


owl_link_response(Res,_) :-
	throw(cannot_parse_response(Res)).


response_elements(element(protocolVersion,Attrs,_),protocolVersion(Attrs)) :- !.
response_elements(element(_:'ClassSubClassesPair',_,[element(_:'ClassSynset',_,ClassDescL),
						     element(_:'SubClassSynsets',_,SynsetsE)]),
		  classSubClassesPair(synset(Classes),Synsets)) :-
	maplist(xml_desc(_),ClassDescL,Classes),
	maplist(response_elements,SynsetsE,Synsets),
	!.

response_elements(element(_:'ObjectPropertySubObjectPropertiesPair',_,[element(_:'ObjectPropertySynset',_,OPL),
						     element(_:'SubObjectPropertySynsets',_,SynsetsE)]),
		  objectPropertySubObjectPropertyPairs(synset(OP),Synsets)) :-
	maplist(xml_desc(_),OPL,OP),
	maplist(response_elements,SynsetsE,Synsets),
	!.

response_elements(element(_:'DataPropertySubDataPropertiesPair',_,[element(_:'DataPropertySynset',_,DPL),
						     element(_:'SubDataPropertySynsets',_,SynsetsE)]),
		  dataPropertySubDataPropertyPairs(synset(DP),Synsets)) :-
	maplist(xml_desc(_),DPL,DP),
	maplist(response_elements,SynsetsE,Synsets),
	!.


response_elements(element(_:'ClassSynsets',_,SynsetsE),Synsets) :-
	 maplist(response_elements,SynsetsE,Synsets),!.

response_elements(element(_:'ObjectPropertySynsets',_,SynsetsE),Synsets) :-
	 maplist(response_elements,SynsetsE,Synsets),!.

response_elements(element(_:'DataPropertySynsets',_,SynsetsE),Synsets) :-
	 maplist(response_elements,SynsetsE,Synsets),!.


response_elements(element(_:'SubClassSynsets',_,SynsetsE),Synsets) :-
	 maplist(response_elements,SynsetsE,Synsets),!.


response_elements(element(_:'ClassSynset',_,ClassDescL),synset(Classes)) :-
	maplist(xml_desc(_),ClassDescL,Classes),!.

response_elements(element(_:'DataPropertySynset',_,DPL),synset(DP)) :-
	maplist(xml_axiom(_),DPL,DP),!.

response_elements(element(_:'IndividualSynset',_,IndL),synset(Ind)) :-
	maplist(xml_axiom(_),IndL,Ind),!.

response_elements(element(_:'ObjectPropertySynset',_,OPL),synset(OP)) :-
	maplist(xml_axiom(_),OPL,OP),!.


response_elements(element('_:Setting',[key=Key], [DataRangeE,LiteralE]),setting(Key,DataRange,Literal)) :-
		  LiteralE = element('Literal',_,[Literal]),
		  (   DataRangeE = element('owl:Datatype',Attrs,_) ->
		  (   member(abbreviatedIRI=DataType,Attrs), DataRange = DataType,! ;
		      member('IRI' = DataType,Attrs),DataRange = DataType,! ;
		      DataRange = unknown_datatype) ;
		  DataRange = DataRangeE),!.

response_elements(element(_:'Prefix',[key=Key,name=Name,fullIRI=IRI],_),prefix(Key,Name,IRI)) :- !.

response_elements(element(_:'AnnotationProperty',Attrs, _),annotationProperty(Attrs)) :- !.


response_elements(element(_:'Property',[key=Key], [DataRangeE,LiteralE]),property(Key,DataRange,Literal)) :-
		  LiteralE = element('Literal',_,[Literal]),
		  (   DataRangeE = element('owl:Datatype',Attrs,_) ->
		  (   member(abbreviatedIRI=DataType,Attrs), DataRange = DataType,! ;
		      member('IRI' = DataType,Attrs),DataRange = DataType,! ;
		      DataRange = unknown_datatype) ;
		      DataRange = DataRangeE),!.



response_elements(X,X).





