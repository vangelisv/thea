% * -*- Mode: Prolog -*- */

:- module(owl2_catalog,
	  [
	   register_ontology_localpath/2,
	   save_catalog/0,
	   save_catalog/1,
	   load_catalog/0,
	   load_catalog/1
	  ]).

:- use_module(library(sgml)).

:- dynamic ontology_localpath/2.

register_ontology_localpath(URL,Local) :-
	retractall(ontology_localpath(URL,Local)),
	assert(ontology_localpath(URL,Local)).

owl2_from_rdf:owl_repository_hook(URL,Local) :-
	ontology_localpath(URL,Local).
owl2_from_rdf:owl_repository_hook(URL,Local) :-
	exists_file('catalog.xml'),
	url_local(URL,Local).

url_local(URL,LocalExpanded) :-
        load_structure('catalog.xml',[element(_,Atts,Elts)],[dialect(xmlns),space(remove)]),
	member('xml:base'=Base,Atts),
	member(element(_,[name=URL,local=Local],_),Elts),
	atom_concat(Base,Local,LocalExpanded).

save_catalog :-
	save_catalog('catalog.pl').

save_catalog(F) :-
	tell(F),
	forall(ontology_localpath(A,B),
	       format('~q.~n',[ontology_localpath(A,B)])),
	told.

load_catalog :-
	load_catalog('catalog.pl').

load_catalog(F) :-
	consult(F).

