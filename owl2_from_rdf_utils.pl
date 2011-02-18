% -----------------------------------------------------------------------
%                                UTILITY Predicates
% -----------------------------------------------------------------------


%%       owl_clear_as
%
%       Clears the prolog terms that store the Abstract Syntax
%       implementation of the OWL ontology.

owl_clear_as :-
        debug(owl_parser,'Clearing abstract syntax',[]),
        forall((axiompred(PredSpec),predspec_head(PredSpec,Head)),
               retractall(Head)).

predspec_head(Pred/A,Head) :- functor(Head,Pred,A).

u_assert(Term) :-
	call(Term), !; assert(Term).


convert(T,V,typed_value(T,V)).


%%	rdf_2_owl(+Base, +Ont) is det
%
%       Converts RDF triples to OWL/4 triples so that
%	their use can tracked by the OWL parser.


rdf_2_owl(Base,Ont) :-
%	debug(owl_parser, 'Removing existing owl triples',[]),
%	retractall(owl(_,_,_,Ont)),
	debug(owl_parser,'Copying RDF triples to OWL triples for Ontology ~w',[Ont]),
	rdf(X,Y,Z,Base:_),
%	owl_fix_no(X,X1), owl_fix_no(Y,Y1), owl_fix_no(Z,Z1),
	assert(owl(X,Y,Z,Ont)), fail.

rdf_2_owl(_,Ont) :-
	owl_count(Ont,Z),
	debug(owl_parser,'Number of owl triples copied: ~w',[Z]).


%%	owl_count(?U).
%       Returns/Checks the number of unused OWL triples.

owl_count(O,U) :-
	findall(1,owl(_,_,_,O),X), length(X,U).

%% expand_and_assert(S,P,O) is det
%
% adds a owl(S,P,O,not_used) after expanding namespaces.
% this is required for the triple replacement rules,
% which use shortened rdfs/owl namespaces.
% (or we could just use the expanded forms here which
%  may be faster..)
expand_and_assert(X1,Y1,Z1) :-
	expand_ns(X1,X),
	expand_ns(Y1,Y),
	expand_ns(Z1,Z),!,
	retractall(owl(X,Y,Z, used1)),
	assert(owl(X,Y,Z, not_used)).


%%       test_use_owl(+Triples:list) is nondet
%
%       As use_owl/1, but does not consume the triple.  If owl(S,P,O)
%       in Triples has a non-ground variable then this will succeed
%       non-deterministically.  If all variables are ground, then this
%       will succeed semi-deterministically.
test_use_owl([]).
test_use_owl([owl(S,P,O)|Rest]) :-
	test_use_owl(S,P,O),
	test_use_owl(Rest).


%%       test_use_owl(?S,?P,?O)
%	As use_owl/3, but does not consume the triple. Expands the S,P,O.
%
%       If any of S, P or O is non-ground then this will succeed
%       non-deterministically.  If all variables are ground, then this
%       will succeed semi-deterministically.
test_use_owl(X1,Y1,Z1) :-
	expand_ns(X1,X),
	expand_ns(Y1,Y),
	expand_ns(Z1,Z),!,
	owl(X,Y,Z, not_used).

test_use_owl(X1,Y1,Z1,named) :-
	expand_ns(X1,X),
	expand_ns(Y1,Y),
	expand_ns(Z1,Z),
	owl(X,Y,Z, not_used),
	not(sub_string(X,0,2,_,'__')).


%%       use_owl(+Triples:list)
%	Marks a list of OWL triples as used, but only if all match. Expands the S,P,O.

use_owl(Triples) :-
        test_use_owl(Triples),
        use_owl_2(Triples).

% consume all triples; we have already tested the list and know that all match
use_owl_2([]).
use_owl_2([owl(S,P,O)|Triples]) :-
        use_owl(S,P,O),
        use_owl_2(Triples).

%%       use_owl(?S,?P,?O)
%	Marks an OWL triple as used. Expands the S,P,O.

use_owl(X1,Y1,Z1) :-
	expand_ns(X1,X),
	expand_ns(Y1,Y),
	expand_ns(Z1,Z),
	owl(X,Y,Z, not_used),
	debug(owl_parser_detail,'using ~w ~w ~w',[X,Y,Z]),
	retract(owl(X,Y,Z, not_used)),
	assert(owl(X,Y,Z,used1)).


%%	use_owl(?S,?P,?O,+Named)
%
%       Named = named: Same as use_owl/3, but marks only if S 	is Named URI (i.e. non blank node).

use_owl(X1,Y1,Z1,named) :-
	expand_ns(X1,X),
	expand_ns(Y1,Y),
	expand_ns(Z1,Z),
	owl(X,Y,Z, not_used),
	not(sub_string(X,0,2,_,'__')),
	retract(owl(X,Y,Z, not_used)),
	assert(owl(X,Y,Z,used2)).

%%       use_owl(?S,?P,?O,Term)
%
%	Marks an OWL triple as used. Expands the S,P,O.

use_owl(X1,Y1,Z1,Term) :-
	expand_ns(X1,X),
	expand_ns(Y1,Y),
	expand_ns(Z1,Z),
	owl(X,Y,Z, not_used),
	debug(owl_parser_detail,'using ~w ~w ~w',[X,Y,Z]),
	retract(owl(X,Y,Z, not_used)),
	assert(owl(X,Y,Z,used(Term))).


%%	use_owl(?S,?P,?O,+Named,Term)
%
%       Named = named: Same as use_owl/3, but marks only if S 	is Named URI (i.e. non blank node).

use_owl(X1,Y1,Z1,named,Term) :-
	expand_ns(X1,X),
	expand_ns(Y1,Y),
	expand_ns(Z1,Z),
	owl(X,Y,Z, not_used),
	not(sub_string(X,0,2,_,'__')),
	retract(owl(X,Y,Z, not_used)),
	assert(owl(X,Y,Z,used(Term))).


%%       expand_ns(+NS_URL, ?Full_URL)
%
%       Expands a 'namespaced' URI of the form ns:fragment to a full URI
%       substituting the full expansion for ns from the ns/2 facts
expand_ns(NS_URL, Full_URL) :-
	nonvar(NS_URL),
	not(NS_URL = literal(_)),
	uri_split(NS_URL,Short_NS,Term, ':'),
	rdf_db:ns(Short_NS,Long_NS),!,
	concat_atom([Long_NS,Term],Full_URL).

expand_ns(URL, URL).


%%       collapse_ns(+FullURL, ?NSURL, +Char, +Options)
%
%	Collapses a full URI of the form Path#fragment to a Namespaced
%	URI NS:fragment substituting the full expansion for ns from
%	the ns/2 facts
%	Char is either ':' for normal ns notation or '_' for builing
%	prolog terms.
%	Options supported: no_base(ShortNs): Use only term!


collapse_ns(FullURL, NSURL,Char,Options) :-
	nonvar(FullURL),
	not(FullURL = literal(_)),
	uri_split(FullURL,LongNS, Term, '#'),
	concat(LongNS,'#',LongNS1),
	rdf_db:ns(ShortNS,LongNS1),
	(   member(no_base(ShortNS),Options), ! , NSURL = Term
	;
	concat_atom([ShortNS,Char,Term],NSURL)
	),!.
% CJM
collapse_ns(FullURL, NSURL,_Char,Options) :-
	nonvar(FullURL),
	not(FullURL = literal(_)),
	uri_split(FullURL,LongNS, Term, '#'),
	member(no_base(LongNS),Options),
        !,
        NSURL = Term.


collapse_ns(URL, URL,_,_).



%%       uri_split(+URI,-Namespace,-Term,+Split_Char) is det
%
%       Splits a URI into the Namespace and the Term parts
%       separated by the Split_Char character.
%       It supposes URI = concat(Namespace,Split_Char,Term)

uri_split(URI,Namespace,Term,Split_Char) :-
	sub_atom(URI,Start,_,After,Split_Char),
	sub_atom(URI,0,Start,_,Namespace),
	Start1 is Start + 1,
	sub_atom(URI,Start1,After,_,Term).


%%       owl_collect_linked_nodes(+Node,+Predicate, +InList,-OutList)

%	Appends Node to the InList, and recursively, all other
%	Nodes that are linked with the Predicate to the Node. The
%	result is returned to OutList.

owl_collect_linked_nodes(Node,Predicate,InList,OutList) :-
	use_owl(Node,Predicate,A),!,
	owl_collect_linked_nodes(Node,Predicate,InList,List1),
	owl_collect_linked_nodes(A,Predicate,List1,OutList).

owl_collect_linked_nodes(Node,Predicate,InList,OutList) :-
	use_owl(A,Predicate,Node),!,
	owl_collect_linked_nodes(Node,Predicate,InList,List1),
	owl_collect_linked_nodes(A,Predicate,List1,OutList).

owl_collect_linked_nodes(Node,_,List, [Node|List]) :-
	not(memberchk(Node, List)),!.

owl_collect_linked_nodes(_,_,List, List) :- !.


% ----------------------------------------------------------------
%                OWL Parser implementation predicates
% ----------------------------------------------------------------


%%       owl_get_bnode(+Node,+Description)
%
%	if Node is a blank (not named) node, then it is asserted in
%	the database as a blanknode(Node,Description,used) term.
%	The purpose is to record when a blank node has been used, so
%	subsequent uses of it will result in structure sharing.

owl_get_bnode(Node,Description) :-
	sub_string(Node,0,2,_,'__'),!,
	not( blanknode(Node,_,_)),
	assert(blanknode(Node,Description, used)).

owl_get_bnode(_,_).


