/*

Example use of Thea Open Calais module
Written by Vangelis Vassiliadis, Feb 2010
GNU GPL License

*/


:- use_module('thea_opencalais.pl').
:- use_module('../../owl2_util.pl').
:- use_module('../../owl2_from_rdf.pl').

pv_attr(Property,List,Value) :-
	member(Property=literal(Value),List),!.
pv_attr(Property,List,Value) :-
	member(Property=ID,List),
	oc_entity(ID,_,IDPVList,_,_),
	pv_attr('http://s.opencalais.com/1/pred/name',IDPVList,Value),!.

pv_attr(_,_,'n/a').


person_travel(Person,Date,DateString,Origin,Destination,Status,PVList) :-
	oc_relation(_I,'http://s.opencalais.com/1/type/em/r/PersonTravel',PVList),
	pv_attr('http://s.opencalais.com/1/pred/person',PVList,Person),
	pv_attr('http://s.opencalais.com/1/pred/locationdestination',PVList,Destination),
	pv_attr('http://s.opencalais.com/1/pred/locationorigin',PVList,Origin),
	pv_attr('http://s.opencalais.com/1/pred/date',PVList,Date),
	pv_attr('http://s.opencalais.com/1/pred/datestring',PVList,DateString),
	pv_attr('http://s.opencalais.com/1/pred/status',PVList,Status).


quotation(Person,Quotation) :-
	 oc_relation(_I,'http://s.opencalais.com/1/type/em/r/Quotation',PVList),
	 pv_attr('http://s.opencalais.com/1/pred/person',PVList,Person),
	 pv_attr('http://s.opencalais.com/1/pred/quote',PVList,Quotation).


% ---
% Example Usage
%


o :- owl_parse_rdf('owl.opencalais-4.3.xml.owl',[imports(false),clear(complete)]).
pope :- oc_rest(http('http://news.bbc.co.uk/2/hi/uk_news/8492597.stm'),'',_X).
un :- oc_rest(http('http://www.scoop.co.nz/stories/WO0002/S00002.htm'),'',_X).
% papal_visits :- oc_rest(http('http://en.wikipedia.org/wiki/List_of_journeys_of_Pope_Benedict_XVI'),'',_X).
papal_visits :- oc_rest(file(papal_visits),'',_X).
stats(File) :- owl_statistics(all,XML), open(File,write,S), xml_write(S,XML,[header(true)]),close(S).


dereference(URI,Options):-
	catch(owl_parse_rdf(URI,Options),io_error(A,B,C),(nl,nl,print(A-B-C),nl,nl)).

dereference(URI,Options,Ext) :-
	atom_concat(URI,Ext,Ext_URI),
	catch(owl_parse_rdf(Ext_URI,Options),io_error(A,B,C),(nl,nl,print(A-B-C),nl,nl)).





