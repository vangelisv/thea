/* -*- Mode: Prolog -*- */

:- module(owl2_util,
          [
           owl_parse_pro/1,
           owlx_file_to_prolog/1,
           rdf_file_to_prolog/1,
           rdf_file_to_prolog/2,
	   download_import_closure/1,
	   download_import_closure/2,
           write_owl_as_prolog/0,
           undeclared_property/3,
           expand_namespaces/0,
           remove_namespaces/0,
           contract_namespaces/0,
           remove_ns/2,
           replace_ns_prefix/4,
           use_label_as_IRI/2,
           get_IRI_from_label/2,
           use_labels_for_IRIs/0,
           use_safe_labels_for_IRIs/0,
           replace_labels_with_IRIs/0,
           use_numeric_IRIs_for_classes/2,
           prefix_IRIs/1,
           translate_IRIs/1,
           translate_IRIs/2,
           map_IRIs/3,
           assume_entity_declarations/0,
           collect_orphan_axioms/1,
           use_class_labels_as_synonyms/1,
           class_label_synonym_axiom/2,
	   any_axiom_template/1,
           write_ontology_summary/0,
	   show_class/1,
           treeview/1,
           treeview/2,
	   owl_statistics/2
          ]).

:- use_module(swrl). % required for retracting swrl rules
:- use_module(owl2_model).
:- use_module(owl2_metamodel).
:- use_module(owl2_from_rdf).
:- use_module(owl2_xml).
:- use_module(owl2_catalog).
:- use_module(owl2_reasoner).


:-use_module(library('http/http_open')).
:-use_module(library('http/http_client')).
:-use_module(library('http/thread_httpd.pl')).
:-use_module(library(sgml)).


% @Deprecated
% use owl2_io
owl_parse_pro(F):-
        owl2_model:consult(F).


% @Deprecated
% use owl2_io
owlx_file_to_prolog(F):-
        owl_parse_xml(F),
        write_owl_as_prolog.

% @Deprecated
% use owl2_io
rdf_file_to_prolog(F):-
        owl_parse_rdf(F),
        write_owl_as_prolog.

% @Deprecated
% use owl2_io
rdf_file_to_prolog(F,Opts):-
        owl_parse_rdf(F,Opts),
        write_owl_as_prolog.

% @Deprecated
% use owl2_io
write_owl_as_prolog:-
        forall(axiompred(PS),
               write_axioms(PS)).

write_axioms(P/A):-
        !,
        functor(H,P,A),
        write_axioms(H).

write_axioms(H):-
        forall(H,format('~q.~n',[H])).

%% download_import_closure(+F)
% see download_import_closure/2
download_import_closure(F) :-
	download_import_closure(F,[]).

%% download_import_closure(+F,+Opts)
% given a file or URL, calling this predicate will
% download all imported files to the current directory.
% the full closure is followed - downloaded files will
% also have their imports chains downloaded.
% TODO - create a catalog.xml file
download_import_closure(F,Opts) :-
	retractall(temp_catalog(_,_)),
	download_import_closure(F,[],Opts),
	save_catalog.
download_import_closure([],_,_) :- !.
download_import_closure([F|Fs],IL,Opts) :-
	member(F,IL),
	!,
	download_import_closure(Fs,IL,Opts).
download_import_closure([F|Fs],IL,Opts) :-
	!,
	get_import_closure(F,Xs,Opts),
	append(Fs,Xs,Fs2),
	download_import_closure(Fs2,[F|IL],Opts).
download_import_closure(F,IL,Opts) :-
	\+ is_list(F), % allow non-lists
	!,
	download_import_closure([F],IL,Opts).

get_import_closure(F,Xs,_Opts) :-
	import_url_local(F,Local),
	exists_file(Local),
	!,
	register_ontology_localpath(F,Local),
        ensure_loaded(library(semweb/rdf_db)),
	rdf_load(Local,[]),
	findall(X,rdf_has(_,'http://www.w3.org/2002/07/owl#imports',X),Xs),
	debug(download,'adding ~w',[Xs]),
	rdf_retractall(_,_,_).
get_import_closure(F,[],_Opts) :-
	format(user_error,'Could not download: ~w~n',[F]).

% download a URL and store it locally in a directory with the same
% name as the domain, with the same dir structure
%
%  http://x.org/foo/bar/waz.owl ==> x.org/foo/bar/waz.owl
import_url_local(F,Local) :-
	%sub_atom(F,0,_,_,'http:'),
	concat_atom(['',Local],'http://',F),
	!,
	%truncate_url_to_local(F,Local),
	%debug(download,'downloading ~w from ~w',[Local,F]),
        % 2010-11-13 : added -N option: don't re-retrieve files unless newer than local
	sformat(Cmd,'wget -N -x ~w',[F]),
	debug(download,'cmd: ~w',[Cmd]),
	shell(Cmd).
import_url_local(F,F).


expr_refp(someValuesFrom(P,_),P).
expr_refp(allValuesFrom(P,_),P).
undeclared_property(P,A,X) :-
        axiom_contains_expression(A,X),
        expr_refp(X,P),
        atom(P),
        \+ property(P).



%% remove_namespaces
% calls translate_IRIs/1 with remove_ns/2 as argument,
% which has the effect of stripping URI prefixes from all
% names. Note that we cannot then write the results
% straight out as RDF since our identifiers are no longer IRIs.
% however, namespace-stripped simple atom identifiers are
% useful when working with prolog.
%
% @see prefix_IRIs/1 for the converse operation
remove_namespaces:-
        translate_IRIs(remove_ns).

expand_namespaces:-
        translate_IRIs(expand_ns).

%% remove_namespaces
% calls translate_IRIs/1 with contract_ns/2 as argument
contract_namespaces:-
        translate_IRIs(contract_ns).

%% prefix_IRIs(+NS)
% attaches a prefix to all names.
prefix_IRIs(X):-
        translate_IRIs(prefix_IRI(X)).



%% use_numeric_IRIs_for_classes(NS,Base)
%
% e.g. =|use_numeric_IRIs_for_classes('http://example.org/cars#',car_)|=
use_numeric_IRIs_for_classes(NS,Base) :-
        findall(From-To,
                (   class(From),
                    atom_concat(NS,_Local,From),
                    gensym(Base,NewLocal),
                    atom_concat(NS,NewLocal,To)),
                Map),
        length(Map,NumMappings),
        format(user_error,'Mappings: ~w~n',[NumMappings]),
        translate_IRIs(replace_IRI_using_map(Map)).



%% use_labels_for_IRIs/0
% uses rdfs:label if available, if not uses local part of URI.
% e.g.
% if we have axioms like subClassOf('http://x.org#1','http://x.org#2'),
% and label annotationAssertion/2 axioms, then this we be replaced by
% subClassOf(human,animal) etc
use_labels_for_IRIs:-
        translate_IRIs(use_label_as_IRI).

use_safe_labels_for_IRIs:-
        translate_IRIs(use_safe_label_as_IRI).

replace_labels_with_IRIs:-
        translate_IRIs(get_IRI_from_label).

remove_ns(IRI,X) :-
        concat_atom([_,X],'#',IRI),
        !.
remove_ns(X,X).

%% replace_ns_prefix(+From,+To,+OldIRI,?NewIRI) is det
replace_ns_prefix(From,To,OldIRI,NewIRI) :-
        atom_concat(From,Local,OldIRI),
        !,
        atom_concat(To,Local,NewIRI).
replace_ns_prefix(_,_,X,X).

contract_ns(URI,ID) :-
        atom(URI),
        rdf_db:rdf_global_id(NS:Local,URI),
        NS \= rdf,
        NS \= rdfs,
        NS \= owl,
        !,
        concat_atom([NS,Local],':',ID).
contract_ns(X,X).



use_safe_label_as_IRI(IRI,X) :-
        use_label_as_IRI(IRI,X1),
        atom_chars(X1,Chars),
        replace_nonalpha(Chars,Chars2),
        atom_chars(X,Chars2),
        !.

use_label_as_IRI(IRI,X) :-
        labelAnnotation_value(IRI,X),
        !.
use_label_as_IRI(IRI,X) :-
        remove_ns(IRI,X),
        !.
use_label_as_IRI(X,X).

get_IRI_from_label(X,IRI) :- labelAnnotation_value(IRI,X),!.
get_IRI_from_label(X,X).



use_property_as_IRI(Prop,IRI,NewIRI) :-
        anyPropertyAssertion(Prop,IRI,Literal),
	Literal=literal(type(_,Val)),
	concat_atom([NS,Local],':',Val),
	rdf_global_id(NS:Local,NewIRI),
        !.
use_property_as_IRI(X,X).


prefix_IRI(Pre,X,Y) :-
        (   entity(X) ; ontology(X)),
        \+ sub_atom(X,0,_,_,Pre),
        \+ sub_atom(X,0,_,_,http),
        !,
        atom_concat(Pre,X,Y).
prefix_IRI(_,X,X) :- !.

replace_IRI_using_map(Map,X,Y) :- member(X-Y,Map),!.
replace_IRI_using_map(_,X,X).


%% translate_IRIs(+Goal)
% Goal must be a 2 argument predicate Goal(+IRI,?TranslatedAtom)
% all IRIs are translated. The result need not be a valid IRI, it
% can be any prolog atom (possibly compound term as well)
:- module_transparent translate_IRIs/1.
translate_IRIs(Goal):-
        findall(A,axiom(A),Axioms),
        findall(A-A2,(member(A,Axioms),
                      map_IRIs(Goal,A,A2)),
                MapPairs),
        forall(member(A-A2,MapPairs),
               (   findall(O2,(ontologyAxiom(O,A),
                              map_IRIs(Goal,O,O2)),
                           Os),
                   retract_axiom(A),
                   assert_axiom(A2),
                   forall(member(O,Os),
                          assert_axiom(A2,O)))).

:- module_transparent translate_IRIs/2.
translate_IRIs(Goal,Ontology):-
        findall(A,ontologyAxiom(Ontology,A),Axioms),
        maplist(map_IRIs(Goal),Axioms,Axioms2),
        maplist(retract_axiom,Axioms),
        forall(member(A,Axioms2),
               assert_axiom(A,Ontology)).

%% map_IRIs(+MapGoal,+AxiomIn,?AxiomOut)
:- module_transparent map_IRIs/3.
map_IRIs(_,[],[]) :- !.
map_IRIs(G,[X|Xs],[X2|X2s]) :-
        !,
        map_IRIs(G,X,X2),
        map_IRIs(G,Xs,X2s).
map_IRIs(G,X,X2) :-
        atom(X),
        call(G,X,X2),
        !.
map_IRIs(G,X,X2) :-
        X=..[F|Args],
        Args\=[],
        !,
        maplist(map_IRIs(G),Args,Args2),
        %call(G,F,F2),           % swrl axioms use IRIs as functors
        F2=F,
        X2=..[F2|Args2].
map_IRIs(G,X,X2) :-
        call(G,X,X2),
        !.

%% use_class_labels_as_synonyms(+NS)
% sometimes ontologies have opaque numeric IRIs.
% sometimes it is convenient to work with a knowledge base in
% which the labels are used. We can get the reasoner to combine
% these by stating equivalentClasses/2 axioms.
use_class_labels_as_synonyms(NS) :-
        forall(labelAnnotation_value(IRI,X),
               (   atom_concat(NS,X,C),
                   assert_axiom(class(C)),
                   assert_axiom(equivalentClasses([C,IRI])))).

class_label_synonym_axiom(NS,Ax) :-
        entity(IRI),
        labelAnnotation_value(IRI,X),
        atom_concat(NS,X,C),
        (   Ax=class(C)
        ;   Ax=equivalentClasses([C,IRI])).

any_axiom_template(T) :-
	findall(T,
		(   axiom(A),
		    extract_axiom_template(A,T)),
		Ts),
	uniqify(Ts,Ts2),
	member(T,Ts2).

% remove dupes from list (why not use sort? vars? ...)
uniqify([],[]).
uniqify([H|L],L2) :-
	\+ \+ member(H,L),
	uniqify(L,L2).
uniqify([H|L],[H|L2]) :-
	uniqify(L,L2).


% INCOMPLETE!
inferred_declaration(class(C)) :- classAssertion(C,_),atom(C).
inferred_declaration(class(C)) :- subClassOf(C,_),atom(C).
inferred_declaration(class(C)) :- equivalentClasses(L),member(C,L),atom(C).
inferred_declaration(namedIndividual(I)) :- classAssertion(_,I).
inferred_declaration(namedIndividual(I)) :- propertyAssertion(_,I,_).
inferred_declaration(namedIndividual(I)) :- propertyAssertion(_,_,I).
inferred_declaration(objectProperty(P)) :- subPropertyOf(P,_).
inferred_declaration(objectProperty(P)) :- subPropertyOf(_,P).
inferred_declaration(objectProperty(P)) :- inverseProperties(P,_).
inferred_declaration(objectProperty(P)) :- inverseProperties(_,P).



assume_entity_declarations :-
        forall(inferred_declaration(A),
               assert_axiom(A)).

collect_orphan_axioms(Ont) :-
        (   \+ ontology(Ont)
        ->  assert_axiom(ontology(Ont))
        ;   true),
        forall((axiom(A),\+ontologyAxiom(_,A)),
               assert_axiom(A,Ont)).

               
               
        

%% extract_axiom_template(+Ax,?Template)
% replaces atoms with variables
extract_axiom_template(A,_) :-
	atom(A),
	!.
extract_axiom_template(literal(_),_) :-
	!.
extract_axiom_template([],[]).
extract_axiom_template(L,L2) :-
	L=[_|_],
	maplist(extract_axiom_template,L,L2).
	%uniqify(L2,L3).
extract_axiom_template(A,T) :-
	A=..[P|L],
	maplist(extract_axiom_template,L,L2),
	T=..[P|L2].


write_dllearner_conf(Query,Var) :-
        setof(C,Var^(Query,classAssertion(C,Var)),Classes),
        forall(member(Class,Classes),
               write_dllearner_conf(Query,Var,Class)).

write_dllearner_conf(Query,Var,Class) :-
        labelAnnotation_value(Class,N),
        format('// Target: ~w "~w"~n',[Class,N]),
        nl,
        forall((Query,classAssertion(Class,Var)),
               format('+"~w"~n',[Var])),
        forall((Query,\+classAssertion(Class,Var)),
               format('-"~w"~n',[Var])),
        nl.


write_ontology_summary:-
        forall(axiompred(PS),
               write_axiom_summary(PS)).

write_axiom_summary(P/A) :-
        functor(H,P,A),
        aggregate(count,H,H,Count),
        format('Axiom: ~w count = ~w',[P,Count]).

write_new_preds:-
        typedg(_,_),
        fail.

typedg(TG,G) :-
        export_list(owl2_model,EL),
        member(TP/Arity,EL),
        functor(TG,TP,Arity),
        clause(TG,(G,_)),
        owl2_model:axiompred(P/Arity),
        functor(G,P,Arity),
        (   owlpredicate_typed(P2,TP),
            P2\=P
        ->  format('**** ~w ~w ~w~n',[P,P2,TP])
        ;   true),
        \+ owlpredicate_typed(_,TP),
        format('~q.~n',[owlpredicate_typed(P,TP)]).

owlpredargs :-
        export_list(owl2_model,EL),
        member(TP/Arity,EL),
        functor(TG,TP,Arity),
        clause(TG,(_,subsumed_by(_,L))),
        \+ owlpredicate_arguments(TP,_),
        format('~q.~n',[owlpredicate_arguments(TP,L)]),
        fail.

get_class(Q,C) :- annotationLabel_value(C,Q),!.
get_class(C,C).

show_superclasses(R,Q) :-
	get_class(Q,C),
	forall(reasoner_ask(R,subClassOf(C,P)),
	       show_class(P)).

class_pp(C) --> {labelAnnotation_value(C,L)},!,[L].
class_pp(C) --> {atom(C)},!,[C].
class_pp([]) --> !.
class_pp([C]) --> !,class_pp(C).
class_pp([C1,C2|L]) --> !,class_pp(C1),[' '],class_pp([C2|L]).
class_pp(C) --> {C=..[P|Args]},!,[P,'( '],class_pp(Args),[' ) '].



show_class(C) :- class_pp(C,Toks,[]),maplist(write,Toks),nl.

% this could all be moved to a separate module..
treeview(Class) :-
        forall(treeview(Class,X-Y-subClassOf(X,Y),_,[]),
               true).

treeview(Class,Opts) :-
        member(traverse(P),Opts),
        !,
        forall(treeview(Class,
                        X-Y-(   subClassOf(X,Y)
                            ;   X=someValuesFrom(P,Y)),
                        _,
                        []),
               true).
treeview(Class,Opts) :-
        forall(treeview(Class,X-Y-subClassOf(X,Y),_,Opts),
               true).


% treeview(+Class, +Template, ?Tab, +Opts)
% @param Class IRI of class to work back from
% @param Template X-Y-Goal
% @param Tab Description here
% @param Opts Description here

treeview(Class,Template,Tab2,Opts) :-
        copy_term(Template,Class-Parent-Goal),
        (   Goal
        *-> treeview(Parent,Template,Tab,Opts),
            Tab2 = [' '|Tab],
            writetab(Tab2)
        ;   Tab2=[]),
        write_owl_class(Class,Opts),
        nl.

writetab([]).
writetab([_|L]):-
        write(' '),
        writetab(L).

write_owl_class(Class,_) :-
        labelAnnotation_value(Class,Label),
        !,
        write(Label).
write_owl_class(Class,_) :-
        write(Class).

replace_nonalpha(['_'|T],L) :-
        replace_nonalpha1([x,x,x|T],L).
replace_nonalpha([H|T],L) :-
        downcase_atom(H,H2),
        replace_nonalpha1([H2|T],L).
replace_nonalpha1([],[]) :- !.
replace_nonalpha1([H|T],[H|T2]) :-
        isalpha(H),
        !,
        replace_nonalpha1(T,T2).
replace_nonalpha1([_|T],['_'|T2]) :-
        replace_nonalpha1(T,T2).

isalpha('_').
isalpha(X) :- X @>= 'a',X @=< 'z'.
isalpha(X) :- X @>= 'A',X @=< 'Z'.
isalpha(X) :- X @>= '0',X @=< '9'.


               

stats(File) :- owl_statistics(all,XML), open(File,write,S), xml_write(S,XML,[header(true)]),close(S).

%% owl_statistics(+Item, -XMLResult) is det
%
% @param Item Given either all OR a specific ontology(O), it returns
% @param XMLResult an XML structure (see sgml SWI package) with
% statistic info on the Ontology. Currently number or axioms per
% axiompredicate plus axioms as CDATA.
%
% @tbd Complete with other statistic info.


owl_statistics(all,[element(all_axioms,[axiomCount=ACount],[AxiomsElement|OList1])]) :-

	findall(element(owl4,[source=Source,count=Count],[]),
		(   aggregate_all(set(S),(owl2_from_rdf:owl(_,_,_,Source),
					       (atom(Source),S=Source ; Source = used(_),S=used)),SetSource),
		    member(Source,SetSource),
		    aggregate_all(count,((Source = used, Source1 = used(_) ; Source1=Source),owl2_from_rdf:owl(_,_,_,Source1)),Count)),
		OWL4),

	owl_statistics(axioms,AxiomsElement),
	findall(OElem,(ontology(O),owl_statistics(ontology(O),OElem)), OList),
	append(OWL4,OList,OList1),
	aggregate_all(count,axiom(_),ACount),!.


owl_statistics(ontology(O),element(ontology,[name=O,axiomCount=ACount,axiomPredCount=AxiomPredCount],OntAxioms)) :-
	findall(element(P-axioms,[axiomCount=APCount],[]),  % or enter P
		(   axiompred(P/A),functor(T,P,A),
		    % findall(AT,
			%    (	ontologyAxiom(O,T),owl_stats_axiom_element(T,AT)),
			%    _PredAxioms),
		    aggregate_all(count,ontologyAxiom(O,T),APCount)
		),
		OntAxioms),
	aggregate_all(count,ontologyAxiom(O,_Axiom),ACount),
	aggregate_all(sum(X), member(element(_Pred,[axiomCount=X],_),OntAxioms),AxiomPredCount).


owl_statistics(axioms,element(allAxioms,[axiomCount=ACount,axiomPredCount=AxiomPredCount],OntAxioms)) :-
	findall(element(P-axioms,[axiomCount=APCount],[]),  % or enter P
		(   axiompred(P/A),functor(T,P,A),
		    % findall(AT,
			%    (	ontologyAxiom(O,T),owl_stats_axiom_element(T,AT)),
			%    _PredAxioms),
		    aggregate_all(count,axiom(T),APCount)
		),
		OntAxioms),
	aggregate_all(count,axiom(_Axiom),ACount),
	aggregate_all(sum(X), member(element(_Pred,[axiomCount=X],_),OntAxioms),AxiomPredCount).


owl_stats_axiom_element(class(C),element(class,[name=C],[])) :- !.
owl_stats_axiom_element(T,AT) :-
	term_to_atom(T,AT).




% ---------------------------------------------------------------
%
%

init_service(Port) :-
	(   nonvar(Port), !, http_server(graph_http_reply,[port(Port),workers(1)]) ; true).


graph_http_reply(Request) :-
	format('Content-type: text/xml\r\n\r\n'),
	member(input(StIn),Request),
	member(peer(_Peer),Request),
	member(path(Path),Request),

	(   Path = '/crossdomain.xml',!,
	    xml_write(element('cross-domain-policy',[],[element(allow-access-from,[domain='*'],[])]),[])
	;
	     set_stream(StIn,timeout(0)),
	     load_structure(StIn, RequestXML,[dialect(xml),space(sgml)])

	),
	open('graph_http.log',append,Log),write(Log,RequestXML),nl(Log),
	owl_generate_graph(_,false,Result),
	xml_write([Result],[header(true)]),
	xml_write(Log,[Result],[header(true)]),nl(Log),
	close(Log).


graph(File,Reify) :-
	(   ontology(Ontology) -> owl_generate_graph(Ontology,Reify,Result) ;
	    Result = element(error,[],[no_ontology_found])
	),
	open(File,write,S), xml_write(S,[Result],[header(true)]),close(S).

owl_generate_graph(Ontology,ReifyAxioms,Result) :-
	(   ontology(Ontology) ->
	    owl_generate_graph(Ontology,ReifyAxioms,Result,[])
	 ;
	    Result = element(error,[],[no_ontology_found])).

owl_generate_graph(Ontology,ReifyAxioms,element(ontology_graph,[name=Ontology],Nodes),AxiomPreds) :-
	findall(Node,
		(   axiompred(P/A), (AxiomPreds = [_|_] , member(P/A,AxiomPreds) ; AxiomPreds = []),
		    functor(T,P,A),
		    ontologyAxiom(Ontology,T), owl_generate_axiom_graph(T,ReifyAxioms,Node)
		),
		Nodes).


owl_generate_axiom_graph(subClassOf(Sub,Super),true,element(node, [type=subClassOf],Children)) :-
	Children = [element(arc,[type=axiom_argument],[SubNode]),
		    element(arc,[type=axiom_argument],[SuperNode])],
	owl2_generate_ce_graph(Sub,element(E,L,C)),
	SubNode = element(E,L,[element(arc,[type=subClassOf],[SuperNode])|C]),
	owl2_generate_ce_graph(Super,SuperNode).

owl_generate_axiom_graph(equivalentClasses([Sub,Super]),true,element(node, [type=equivalentClass],Children)) :-
	Children = [element(arc,[type=axiom_argument],[SubNode]),
		    element(arc,[type=axiom_argument],[SuperNode])],
	owl2_generate_ce_graph(Sub,element(E,L,C)),
	SubNode = element(E,L,[element(arc,[type=equivalentClass],[SuperNode])|C]),
	owl2_generate_ce_graph(Super,SuperNode).


owl_generate_axiom_graph(subClassOf(Sub,Super),false,SubNode) :-
	owl2_generate_ce_graph(Sub,element(E,L,C)),
	SubNode = element(E,L,[element(arc,[type=subClassOf],[SuperNode])|C]),
	owl2_generate_ce_graph(Super,SuperNode).

owl_generate_axiom_graph(equivalentClasses([Sub,Super]),false,SubNode) :-
	owl2_generate_ce_graph(Sub,element(E,L,C)),
	SubNode = element(E,L,[element(arc,[type=equivalentClass],[SuperNode])|C]),
	owl2_generate_ce_graph(Super,SuperNode).



% owl_generate_axiom_graph(_,_,element(node,[type=axiom_error],[])).

%
% Class Expressions (Descriptions)
%

owl2_generate_ce_graph(intersectionOf([E|Rest]),element(node,[type=intersectionOf],Children)) :-
	findall(element(arc,[type=member],[Node]),
		(member(X,[E|Rest]), owl2_generate_ce_graph(X,Node)),
		Children),!.

owl2_generate_ce_graph(unionOf([E|Rest]),element(node,[type=unionOf],Children)) :-
	findall(element(arc,[type=member],[Node]),
		(member(X,[E|Rest]), owl2_generate_ce_graph(X,Node)),
		Children),!.

owl2_generate_ce_graph(IRI,element(node,[type=class,id=NSLocalA],[])) :- atom(IRI),rdf_db:rdf_global_id(NSLocal,IRI),
	term_to_atom(NSLocal,NSLocalA),!. % better iri(IRI).

%  owl2_generate_ce_graph(_,element(node,[type=unhandled_ce_node],[])) :-
%  !.

owl2_export_axiom(oneOf([E|Rest]),main_triple(BNode,'rdf:type',Type)) :-
	as2rdf_bnode(oneOf([E|Rest]),BNode),
	owl2_export_list([E|Rest],LNode),
	(   classExpression(E) -> Type = 'owl:Class'; Type = 'owl:Datatype'),
	owl_rdf_assert(BNode,'owl:oneOf', LNode),!.

owl2_export_axiom(datatypeRestriction(DT,FVs),main_triple(BNode,'rdf:type','rdfs:Datatype')) :-
	as2rdf_bnode(datatypeRestriction(DT,FVs),BNode),
	owl_rdf_assert(BNode,'rdf:type','rdfs:Datatype'),
	owl2_export_axiom(DT,main_triple(Tpe,_,_)),owl_rdf_assert(BNode,'owl:onDatatype',Tpe),
	owl2_export_list(FVs,LNode),
	owl_rdf_assert(BNode,'owl:withRestrictions',LNode).

owl2_export_axiom(facetRestriction(F,V),main_triple(BNode,F2,_V2)) :-
	(   sub_atom(F,_,_,_,'#')
	->  F2=F2
	;   atom_concat('xsd:',F,F2)),
	as2rdf_bnode(facetRestriction(F,V),BNode),
	owl_rdf_assert(BNode,F,V).

owl2_export_axiom(complementOf(E),main_triple(BNode,'rdf:type',Type)) :-
	as2rdf_bnode(complementOf(E),BNode),
	owl2_export_axiom(E,main_triple(Te,_,_)),
	(   classExpression(E) -> Type = 'owl:complementOf'; Type = 'owl:datatypeComplementOf'),
	owl_rdf_assert(BNode,'owl:complementOf', Te),!.



owl2_export_axiom(someValuesFrom(PE,CEorDR),main_triple(BNode,'rdf:type','owl:Restriction')) :-
	as2rdf_bnode(someValuesFrom(PE,CEorDR),BNode),
	owl_rdf_assert(BNode,'rdf:type','owl:Restriction'),
	owl2_export_axiom(PE,main_triple(Tpe,_,_)),owl_rdf_assert(BNode,'owl:onProperty',Tpe),
	owl2_export_axiom(CEorDR,main_triple(Tce,_,_)),owl_rdf_assert(BNode,'owl:someValuesFrom',Tce),!.


owl2_export_axiom(allValuesFrom(PE,CEorDR),main_triple(BNode,'rdf:type','owl:Restriction')) :-
	as2rdf_bnode(allValuesFrom(PE,CEorDR),BNode),
	owl_rdf_assert(BNode,'rdf:type','owl:Restriction'),
	owl2_export_axiom(PE,main_triple(Tpe,_,_)),owl_rdf_assert(BNode,'owl:onProperty',Tpe),
	owl2_export_axiom(CEorDR,main_triple(Tce,_,_)),owl_rdf_assert(BNode,'owl:allValuesFrom',Tce),!.

owl2_export_axiom(hasValue(PE,Value),main_triple(BNode,'rdf:type','owl:Restriction')) :-
	as2rdf_bnode(hasValue(PE,Value),BNode),
	owl_rdf_assert(BNode,'rdf:type','owl:Restriction'),
	owl2_export_axiom(PE,main_triple(Tpe,_,_)),owl_rdf_assert(BNode,'owl:onProperty',Tpe),
	owl2_export_axiom(Value,main_triple(TValue,_,_)),owl_rdf_assert(BNode,'owl:hasValue',TValue),!.

owl2_export_axiom(hasSelf(OPE),main_triple(BNode,'rdf:type','owl:Restriction')) :-
	as2rdf_bnode(hasValue(OPE),BNode),
	owl_rdf_assert(BNode,'rdf:type','owl:Restriction'),
	owl2_export_axiom(OPE,main_triple(Tope,_,_)),owl_rdf_assert(BNode,'owl:onProperty',Tope),
	owl_rdf_assert(BNode,'owl:hasSelf',	literal(type('http://www.w3.org/2001/XMLSchema#boolean','true'))),!.


owl2_export_axiom(minCardinality(N,OPE),main_triple(BNode,'rdf:type','owl:Restriction')) :-
	as2rdf_bnode(minCardinality(N,OPE),BNode),
	owl_rdf_assert(BNode,'rdf:type','owl:Restriction'),
	owl_rdf_assert(BNode,'owl:minCardinality',literal(type('http://www.w3.org/2001/XMLSchema#nonNegativeInteger',N))),
	owl2_export_axiom(OPE,main_triple(Tope,_,_)),owl_rdf_assert(BNode,'owl:onProperty',Tope),!.

owl2_export_axiom(minCardinality(N,OPE,CEorDR),main_triple(BNode,'rdf:type','owl:Restriction')) :-
	as2rdf_bnode(minCardinality(N,OPE,CEorDR),BNode),
	owl_rdf_assert(BNode,'rdf:type','owl:Restriction'),
	owl_rdf_assert(BNode,'owl:minQualifiedCardinality',literal(type('http://www.w3.org/2001/XMLSchema#nonNegativeInteger',N))),
	owl2_export_axiom(OPE,main_triple(Tope,_,_)),owl_rdf_assert(BNode,'owl:onProperty',Tope),
	owl2_export_axiom(CEorDR,main_triple(Tce,_,_)),
	(   classExpression(CEorDR) -> owl_rdf_assert(BNode,'owl:onClass',Tce); owl_rdf_assert(BNode,'owl:onDataRange',Tce)),!.


owl2_export_axiom(maxCardinality(N,OPE),main_triple(BNode,'rdf:type','owl:Restriction')) :-
	as2rdf_bnode(maxCardinality(N,OPE),BNode),
	owl_rdf_assert(BNode,'rdf:type','owl:Restriction'),
	owl_rdf_assert(BNode,'owl:maxCardinality',literal(type('http://www.w3.org/2001/XMLSchema#nonNegativeInteger',N))),
	owl2_export_axiom(OPE,main_triple(Tope,_,_)),owl_rdf_assert(BNode,'owl:onProperty',Tope),!.

owl2_export_axiom(maxCardinality(N,OPE,CEorDR),main_triple(BNode,'rdf:type','owl:Restriction')) :-
	as2rdf_bnode(maxCardinality(N,OPE,CEorDR),BNode),
	owl_rdf_assert(BNode,'rdf:type','owl:Restriction'),
	owl_rdf_assert(BNode,'owl:maxQualifiedCardinality',literal(type('http://www.w3.org/2001/XMLSchema#nonNegativeInteger',N))),
	owl2_export_axiom(OPE,main_triple(Tope,_,_)),owl_rdf_assert(BNode,'owl:onProperty',Tope),
	owl2_export_axiom(CEorDR,main_triple(Tce,_,_)),
	(   classExpression(CEorDR) -> owl_rdf_assert(BNode,'owl:onClass',Tce); owl_rdf_assert(BNode,'owl:onDataRange',Tce)),!.


owl2_export_axiom(exactCardinality(N,OPE),main_triple(BNode,'rdf:type','owl:Restriction')) :-
	as2rdf_bnode(exactCardinality(N,OPE),BNode),
	owl_rdf_assert(BNode,'rdf:type','owl:Restriction'),
	owl_rdf_assert(BNode,'owl:cardinality',literal(type('http://www.w3.org/2001/XMLSchema#nonNegativeInteger',N))),
	owl2_export_axiom(OPE,main_triple(Tope,_,_)),owl_rdf_assert(BNode,'owl:onProperty',Tope),!.

owl2_export_axiom(exactCardinality(N,OPE,CEorDR),main_triple(BNode,'rdf:type','owl:Restriction')) :-
	as2rdf_bnode(exactCardinality(N,OPE,CEorDR),BNode),
	owl_rdf_assert(BNode,'rdf:type','owl:Restriction'),
	owl_rdf_assert(BNode,'owl:qualifiedCardinality',literal(type('http://www.w3.org/2001/XMLSchema#nonNegativeInteger',N))),
	owl2_export_axiom(OPE,main_triple(Tope,_,_)),owl_rdf_assert(BNode,'owl:onProperty',Tope),
	owl2_export_axiom(CEorDR,main_triple(Tce,_,_)),
	(   classExpression(CEorDR) -> owl_rdf_assert(BNode,'owl:onClass',Tce); owl_rdf_assert(BNode,'owl:onDataRange',Tce)),!.




/** <module> Various utility predicates for OWL ontologies

  ---+ Synopsis

==
:- use_module(bio(owl2_util)).

%
demo:-
  nl.


==

---+ Details

This is a collection of very ad-hoc predicates for doing things with OWL.
This should not be regarded as stable.


---+ Additional Information



*/
