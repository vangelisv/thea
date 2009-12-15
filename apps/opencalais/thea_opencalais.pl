/*

Hello

*/


:- module(thea_opencalais,
	  [ rest/4 % File
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


:- dynamic open_calais_class/1.

open_calais_class('http://s.opencalais.com/1/type/er/Geo/Country').

%% owl_link(+ReasonerURL, +Request:list, -Response:list, +Options:list) is det
%  Implements OWLLink over HTTP. Sends an OWLLink request to the ReasonerURL and
%  gets its Response.


rest(LicenseID,Content,Result,Params) :-
	LicenseID='nbqjkw3dn3mfhmuejy7h9c62',
	atomic_list_concat(['licenseID=',LicenseID,'&content=',Content,'&paramsXML=',Params],Atom),
	atom_codes(Atom,Codes),
	http_post('api.opencalais.com/enlighten/rest/',
		  codes('application/x-www-form-urlencoded',Codes),
		  Result,[]),
	open('_opencalais',write,St),
	write(St,'<?xml version="1.0"?>'),nl(St),
	write(St,Result),
	close(St),
	owl_parse_rdf('_opencalais',[imports(false),clear(complete)]).


i(I) :-
	classAssertion(C,I),
	print('---------------------------------------------------'),nl,
	print(I), print('\t::\t'), print(C),nl,
	print('---------------------------------------------------'),nl,
	forall(propertyAssertion(P,I,V),
	       (   print(P),print('\t:\t'),print(V),nl)).


dereference(URI,Options) :-
	atom_concat(URI,'.rdf',RDF_URI),
	catch(owl_parse_rdf(RDF_URI,Options),io_error(A,B,C),(nl,nl,print(A-B-C),nl,nl)).

open_calais_entity(C,I) :-
	open_calais_class(C),
	classAssertion(C,I).


