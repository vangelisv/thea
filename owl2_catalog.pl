% * -*- Mode: Prolog -*- */

:- module(owl2_catalog,
	  []).

:- use_module(library(sgml)).

owl2_from_rdf:owl_repository_hook(URL,Local) :-
	exists_file('catalog.xml'),
	url_local(URL,Local).

url_local(URL,LocalExpanded) :-
        load_structure('catalog.xml',[element(_,Atts,Elts)],[dialect(xmlns),space(remove)]),
	member('xml:base'=Base,Atts),
	member(element(_,[name=URL,local=Local],_),Elts),
	atom_concat(Base,Local,LocalExpanded).

	
