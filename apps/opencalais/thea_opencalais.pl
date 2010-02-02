/*

Hello

*/


:- module(thea_opencalais,
	  [ rest/3 % File
	  ]).

:-use_module(library('http/http_client')).
:-use_module(library('sgml')).
:-use_module(library('sgml_write')).
:-use_module('../../owl2_model.pl').
:-use_module('../../owl2_from_rdf.pl').


/** <module> Thea wrapper for open calais service.

Open Calais service wrapper



  @author Vangelis Vassiliadis
  @license GPL


*/


:- dynamic open_calais/1.

open_calais(class('http://s.opencalais.com/1/type/er/Geo/Country')).
open_calais(class('http://s.opencalais.com/1/type/er/Geo/City')).
open_calais(license('nbqjkw3dn3mfhmuejy7h9c62')).

%% rest(+Request:list, +Params:string, -Result) is det
%
% Calls Open Calais Rest service and posts the Request.
% Requires a valid Open_Calais license key in a dynamic
% open_calais(license(License)) to be asserted.
%
% Request can be any of the following:
%   * http(URL)
%   * file(Filename)
%   * text(Atom)
% Params is a string encoded XML see description in http://www.opencalais.com/APICalls
%
% Result is the RDF response of Open_Calais see http://www.opencalais.com/documentation/calais-web-service-api/interpreting-api-response/rdf
%


rest(_Request,_Params,_Result) :-
	not(open_calais(license(_))),
	throw(open_calais_exception(missing_license)),!.

rest(Request,Params,Result) :-
	open_calais(license(LicenseID)),
        (   Request = http(URL) -> http_get(URL,Content,[]);
	Request = file(Filename) -> read_file_to_codes(Filename,Codes,[]),atom_codes(Content,Codes) ;
	Request = text(Content) -> true ; throw(open_calais_exception(invalid_request_type))),

	http_post('api.opencalais.com/enlighten/rest/',
		  form(['licenseID'=LicenseID,content=Content,paramsXML=Params]),
		  Result,[]),
	open('_opencalais',write,St),
	write(St,'<?xml version="1.0"?>'),nl(St),write(St,Result),
	close(St),
	owl_parse_rdf('_opencalais',[imports(false),clear(complete)]).


i(I) :-
	classAssertion(C,I),
	print('---------------------------------------------------'),nl,
	print(I), print('\t::\t'), print(C),nl,
	print('---------------------------------------------------'),nl,
	forall(propertyAssertion(P,I,V),
	       (   print(P),print('\t:\t'),print(V),nl)).


dereference(URI,Options):-
	catch(owl_parse_rdf(URI,Options),io_error(A,B,C),(nl,nl,print(A-B-C),nl,nl)).

dereference(URI,Options,Ext) :-
	atom_concat(URI,Ext,Ext_URI),
	catch(owl_parse_rdf(Ext_URI,Options),io_error(A,B,C),(nl,nl,print(A-B-C),nl,nl)).

open_calais_entity(C,I) :-
	open_calais_class(C),
	classAssertion(C,I).


