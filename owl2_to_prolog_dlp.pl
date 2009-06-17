/* -*- Mode: Prolog -*- */

:- module(owl2_to_prolog_dlp,
          [
           owl_write_all_dlpterms/0,
           owl_write_all_dlpterms/1,
           owl_dlpterm/2,
           owl_dlpterm/3,
           owl_write_dlpterm/2,
           owl_write_prolog_code/2
           ]).

:- use_module(owl2_model).
:- use_module(owl2_from_rdf,[collapse_ns/4]).

:- multifile owl2_io:save_axioms_hook/3.
owl2_io:save_axioms_hook(_File,dlp,Opts) :-
        owl_write_all_dlpterms(Opts).


%% owl_write_all_dlpterms
% as owl_write_all_dlpterms/1
owl_write_all_dlpterms :-
        owl_write_all_dlpterms([]).

%% owl_write_all_dlpterms(+Opts:list)
% writes all axiom/1 as dlpterms on output stream using owl_write_dlpterm/2
owl_write_all_dlpterms(Opts) :-
        forall(member(write_directives(Dir),Opts),
               write_directives(Dir,Opts)),
        setof(Ax,axiom(Ax),Axs),
        forall(member(Ax,Axs),
               owl_write_dlpterm(Ax,Opts)).

write_directives(table,Opts) :-
        forall(class(C),
               write_table_class(C,Opts)),
        forall(property(P),
               write_table_property(P,Opts)).

write_table_class(X,Opts) :-
        collapse_ns(X,X1,'_',Opts),
        format(':- table ~q/1.~n',[X1]).
write_table_property(X,Opts) :-
        collapse_ns(X,X1,'_',Opts),
        format(':- table ~q/2.~n',[X1]).


%% owl_dlpterm(+OwlAsTerm,?DlpTerm)
owl_dlpterm(OwlAsTerm,R) :- 
	owl_as2prolog(OwlAsTerm,R,_).
        
%% owl_dlpterm(+OwlAsTerm,?DlpTerm,+Options:list)
% (Options currently ignored)
owl_dlpterm(OwlAsTerm,R,_) :- 
	owl_as2prolog(OwlAsTerm,R,_).


%% owl_write_dlpterm(+OwlAsTerm,+Options) 
%
%	 Converts the prolog OWL abstract syntax term (as parsed by
%	 OWl parser) into prolog logic code, based on the mapping
%	 proposed by [Grosof] in the context of DLP. The prolog code
%	 is written into the current output stream, so
%	 redirecting the output stream into a file is suggested in order
%	 to capture the generated code. Options are generic options to
%	 modify the behaviour of the code generation. Currently only the
%	 no_base(Namespace) is supported. This option tells the code
%	 generator not to prefix the prolog predicates with the
%	 namespace prefix.



owl_write_dlpterm(OwlAsTerm,Options) :- 
	owl_as2prolog(OwlAsTerm,R,_),
        format('% ~q ~n',[OwlAsTerm]),
	owl_write_prolog_code(R,Options),
        !.
owl_write_dlpterm(OwlAsTerm,_) :- 
        throw(thea(cannot_write(OwlAsTerm))).


%% owl_write_prolog_code(+Term,+Options) 
%
%	 Term is an intermediate format generated from the
%	 owl_as2prolog/3 predicate. This predicate handles the
%	 prolog code generation from this intermediate format
%	 into prolog code. 
%        For Options see the owl_as2prolog/2 predicate.


%
% Generate code for each item in the list.
% 

owl_write_prolog_code([],_) :- !.


owl_write_prolog_code([H|T],Options) :-
	owl_write_prolog_code(H,Options),!, 
	owl_write_prolog_code(T,Options).


%
% Generate code for the or (;) prolog construct.
% 

owl_write_prolog_code(;(A,B),Options) :- !,
	write('('), owl_write_prolog_code(A,Options), 	write(';'), 
	owl_write_prolog_code(B,Options), write(')').

%
% Generate code for the and (;) prolog construct.
% 

owl_write_prolog_code( (A,B), Options ) :- !,
	owl_write_prolog_code(A,Options), 
	write(','), 
	owl_write_prolog_code(B,Options).


%
% Generate code for a prolog rule Head :- Body 
% 

owl_write_prolog_code( ('owl:Nothing'(_):- _), Options) :-
        members(suppress_owl_nothing(true),Options),
        !.

owl_write_prolog_code( ( ( H1; H2) :- B), Options) :- !,
        (   member(disjunctive_datalog(true),Options)
        ->  owl_write_prolog_head_disjunction((H1;H2),Options),
            write(':-'),            
	    nl, write('     '), 
	    owl_write_prolog_code(B,Options), 
	    write('.'), nl
        ;   true).
        
owl_write_prolog_code( (H :- B), Options) :-!,
	(   H = false , ! 
	;   
	B = false , ! 
	; 
	H = [], ! 
	; 
	H = [_|_] , !, 
	    maplist(map_head_conjunction(B),H,R), 
	    owl_write_prolog_code(R,Options) % rewrite rule (a,b) :- c ==> a :- c and b:-c

	; 
	H = (H1 :- H2) , !, 
	    owl_write_prolog_code(:-(H1,(H2,B)),Options) % rewrite rule a :- b) :- c ==> a :- b,c
	; 
	H = (H1 ; H2) , !, 
	    owl_write_prolog_head_disjunction(H,Options)
	;  
	B = none, !, % It is a fact (no body).
	    owl_write_prolog_code(H,Options), write('.'), nl
	; 
	owl_write_prolog_code(H,Options), write(':-'), % normal rule H:-B. 
	    nl, write('     '), 
	    owl_write_prolog_code(B,Options), 
	    write('.'), nl
	).

%
% Generate code for a 'class' predicate:  C(X) or C(individual). 
% 

owl_write_prolog_code(class(X,Y),Options) :- !,
	collapse_ns(X,X1,'_',Options),collapse_ns(Y,Y1,':',[]),
	(   var(Y), !, 	
	    writeq(X1), write('(X)') 
	;
	Y = y , !,  
	    writeq(X1), write('('), write('Y'), write(')')
	;
	writeq(X1), write('('), writeq(Y1), write(')')
	).



%
% Generate code for a 'property' predicate: P(X,Y) or
% P(class,individual) or P(individual, individual).
% 

owl_write_prolog_code(property(_,_,literal(_)),Options) :-
        member(suppress_literals(true),Options),
        write(true),
        !.
owl_write_prolog_code(property(P,X,Y),Options) :- !, 
	collapse_ns(P,P1,'_',Options),
	writeq(P1),  write('('),
	(   X = x, !, write('X') 
        ;   X = y, !, write('Y')  
        ;   X = z, !, write('Z')  
        ;   X = v(NX), !, write('V'),write(NX)  
        ;   X = var , !, write('_') 
        ;   collapse_ns(X,X1,':',[]),
            writeq(X1)
	),	
	write(','),
	(   Y = x, !, write('X')  
	;   Y = y, !, print('Y')  
	;   Y = z, !, print('Z')  
        ;   Y = v(NY), !, write('V'),write(NY)  
	;   Y = var , !, print('_') 
	;   collapse_ns(Y,Y1,':',[]),
            writeq(Y1)
	),
	write(')').

owl_write_prolog_code(swrlproperty(P,X,Y),Options) :- !, 
	collapse_ns(P,P1,'_',Options),collapse_ns(Y,Y1,'_',[no_base(_)]),collapse_ns(X,X1,'_',[no_base(_)]),
        upcase_atom(X1,X2),
        upcase_atom(Y1,Y2),
	writeq(P1),  write('('),
	write(X2),
        write(','),
	write(Y2),
	write(')').

owl_write_prolog_code(swrldescription(P,X),Options) :- !, 
	collapse_ns(P,P1,'_',Options),collapse_ns(X,X1,'_',[no_base(_)]),
        upcase_atom(X1,X2),
	writeq(P1),  write('('),
	write(X2),
	write(')').

%
% none generates nothing.
%

owl_write_prolog_code(none,_Options) :- !.

%
% otherwise generate the Term itself
%

owl_write_prolog_code(Term,_Options) :-
	writeq(Term).

owl_write_prolog_head_disjunction((H1;H2),Options) :-
        member(head_disjunction_symbol(Op),Options),
        !,
        owl_write_prolog_code(H1,Options),
        write(' '),
        write(Op),
        write(' '),
        owl_write_prolog_head_disjunction(H2,Options).
owl_write_prolog_head_disjunction(H,Options) :-
        !,
        owl_write_prolog_code(H,Options).







%
% used in case of conjunction in the head. Used in rewrite rule
%( a,b) :- c ==> a :- c and b:-c 
%

map_head_conjunction(B,H, :-(H,B)).



%% owl_as2prolog(+OwlAsTerm, -ResultTerm, ?Mode) 
%
%	 Predicate to convert a Thea prolog OWL abstract term into
%	 the intermediate term used for prolog (logic) code generation.
%	 The Mode is used to differentiate the convertion depending on
%	 wether the OWL construct appears in the head or in a body of a
%	 prolog rule. It can be on of head, body and fact.
%
%	 The mappings for the class descriptions are summarised in the
%	 following table for each mode.
%   
% Description	  Head                 Body	        Fact
% -----------------------------------------------------------------
% intersectionOf a,b,c, +rewrite rule  a,b,c            -
% unionOf        -                     a;b;c            a. b. c.
% compl          -                     -                -  
% one of                                                -
% restr value    p(ID,V)               p(ID,V)          p(ID,V) 
% restr all      C(Y):-P(X,Y),D(X)     -                C(Y):-P(ID,Y). 
% restr some     -		       C(X):-P(X,Y),D(Y) -
%
% Mode = head | body | fact

owl_as2prolog(class(_),none,_) :- !.


% 
% A class with no description generates none (no code).
%

%owl_as2prolog(class(_,_,complete,_,[intersectionOf([])]),none,_) :- !.
owl_as2prolog(equivalentClasses([_]),none,_) :- !.

% this is illegal, but pass-through silently anyway
owl_as2prolog(equivalentClasses([]),none,_) :- !.

% 
% A complete class declaration with a single descrption element is
% equivalent to this description 
%

owl_as2prolog(equivalentClasses([C,D]),[R1,R2],_) :- !,
	% equivalent
	owl_as2prolog(subClassOf(C,D),R1,_),
	owl_as2prolog(subClassOf(D,C),R2,_).

% more general form of above clause
owl_as2prolog(equivalentClasses(L),RL,_) :- !,
        findall(R,
                (   member(C,L),
                    member(D,L),
                    C\=D,
                    owl_as2prolog(subclassOf(C,D),R,_)),
                RL).

owl_as2prolog(disjointClasses(L),RL,_) :- !,
        findall(R,
                (   member(C,L),
                    member(D,L),
                    C@<D,
                    owl_as2prolog(subClassOf(intersectionOf([C,D]),'owl:nothing'),R,_)),
                RL).

owl_as2prolog(differentIndividuals(_),none,_) :- !.


% 
% Subclass(Class,Superclass) ==> C(X) implies S(X) or S(X) :- C(X).
%

owl_as2prolog(subClassOf(A,B),R,_) :- 
     owl_as2prolog(description(A,_),Rb,body),
     owl_as2prolog(description(B,_),Rh,head),
     !,     
     R = (:-(Rh,Rb)).

% 
% Intersection of descriptions does not generate anything in fact mode.
%

owl_as2prolog(description(intersectionOf(_),_),false,fact):- !.

% TODO: introduced in OWL2
owl_as2prolog(description(hasSelf(_),_),false,fact):- !.


% 
% Intersection of descriptions generates a comma separated list of
% descriptions in either head or body modes. 
%

owl_as2prolog(description(intersectionOf(DL),X),R,Param):- !,
	owl_as2prolog(description_list(DL,X,','),R,Param).


% 
% Union (use of Or) cannot be handled in the head of a rule in prolog.
% However, we allow the possibility of translation to extensions such as disjunctive datalog.
% Here we create a prolog term with a disjunction in the head; it is up to the prolog writing
% part whether to exclude this or not
%

%owl_as2prolog(description(unionOf(_),_),false,head):-!.
owl_as2prolog(description(unionOf(DL),X),R,head):-!,
        owl_as2prolog(description_list(DL,X,';'),R,body). % hacky-trick to treat head as body for disjunctions

% 
% Union generates ; separated terms.
%

owl_as2prolog(description(unionOf(DL),X),R,body):-!,
	owl_as2prolog(description_list(DL,X,';'),R,body).

owl_as2prolog(description(unionOf(DL),X),R,fact):-!,
	owl_as2prolog(description_list(DL,X,';'),R,fact).


%
% Complement of (Not) is not handled in this conversion
%

% TODO: add hook for LP engines that support negation..
% but be careful re open/closed world
owl_as2prolog(description(complementOf(_),_),false,_) :- !.


%
% OneOf is handled with membership only in body of rules.
%

owl_as2prolog(description(oneOf(DL),_),member(_,DL),body) :- !.
owl_as2prolog(description(oneOf(_),_),false,_) :- !.


%
% Value property description generates a property term (predicate)
%

owl_as2prolog(description(hasValue(PropertyID,Value),X),R,_) :- 
	R = property(PropertyID,X,Value),!.

%
% Universal property description. See table above
%

owl_as2prolog(description(allValuesFrom(_,_),_),false,body) :-  !.
%owl_as2prolog(description(restriction(_,allValuesFrom(_)),_),false,body) :-  !.


owl_as2prolog(description(allValuesFrom(PropertyID,Descr),_),R,head) :-  !,
	owl_as2prolog(description(Descr,y),D,head),
	R =  :-(D,property(PropertyID,x,y)).

owl_as2prolog(description(allValuesFrom(PropertyID,Descr),ID),R,fact) :-  !,
	owl_as2prolog(description(Descr,_),D,head),
	R =  :-(D,property(PropertyID,ID,x)).


%
% Existential property description. See table above
%

owl_as2prolog(description(someValuesFrom(_,_),_),false,head) :-  !.

owl_as2prolog(description(someValuesFrom(PropertyID,Descr),_),R,body) :-  !,
	owl_as2prolog(description(Descr,y),D,body),
	R =  (D,property(PropertyID,x,y)).

%
% Cardinalities are not handled in this conversion
%

owl_as2prolog(description(maxCardinality(_,_),_),false,_) :-  !.
owl_as2prolog(description(minCardinality(_,_),_),false,_) :-  !.
owl_as2prolog(description(exactCardinality(_,_),_),false,_) :-  !.

% QCRs: added in OWL2
owl_as2prolog(description(maxCardinality(_,_,_),_),false,_) :-  !.
owl_as2prolog(description(minCardinality(_,_,_),_),false,_) :-  !.
owl_as2prolog(description(exactCardinality(_,_,_),_),false,_) :-  !.


%
% Any other description is taken to be a named class
%

owl_as2prolog(description(Any,X),class(Any,X),_) :- !.



%
% Handling of description lists in head and bodies of rules
%

owl_as2prolog(description_list([],_,_),[],_) :- !.

owl_as2prolog(description_list([Descr],X,_),R,body) :- !,
	owl_as2prolog(description(Descr,X),R,body).

owl_as2prolog(description_list([Descr|Rest],X,Separator),T,Param) :-
	owl_as2prolog(description(Descr,X),H,Param),!,	
	owl_as2prolog(description_list(Rest,X,Separator),Tail,Param),
	(   Param = body , ! ,  
	    (H = false, !, T = [false] ; Tail = false, !, T = false
	    ; 
	    T =.. [Separator,H,Tail]
	    ) ; 	    
	T = [H|Tail]
	).

% 
%  Mapping properties. 
%  a. Generate a s(X,Y) :- p(X,Y). for each super property p
%  b. Generate a C(X) :- P(X,Y) for each C in the property domain
%  c. Generate a c(Y) :- p(X,Y) for each range C
%  d. Handle property attributes in process_pt_list predicate
%

% TODO: property expressions - inverseOf, propertyChain; other places e.g. inverseProperties
%owl_as2prolog(subPropertyOf(P,inverseOf(SuperP)),(property(SuperP,y,x) :- property(P,x,y)),_).
%owl_as2prolog(subPropertyOf(inverseOf(P),SuperP),(property(SuperP,x,y) :- property(P,y,x)),_).
%owl_as2prolog(subPropertyOf(P,SuperP),(property(SuperP,x,y) :- property(P,x,y)),_).

owl_as2prolog(subPropertyOf(P,SuperP),(PE :- SPE),_) :- !,
        owl_as2prolog(propertyExpression(P),PE,head),
        owl_as2prolog(propertyExpression(SuperP),SPE,body).

owl_as2prolog(propertyExpression(inverseOf(P)),property(P,y,x), _) :- !.

%owl_as2prolog(propertyExpression(propertyChain([P1,P2])),(property(P1,x,z),property(P2,z,y)), _) :- !.
owl_as2prolog(propertyExpression(propertyChain(PL)),ChainGoal, _) :-
        chain_to_goal(PL,ChainGoal).


owl_as2prolog(propertyExpression(P),property(P,x,y), _) :- !.


owl_as2prolog(propertyDomain(P,D),(L :- property(P,x,var)), _) :- !,
        map_description(head,_,D,L).

owl_as2prolog(propertyRange(P,D),(L :- property(P,var,x)), _) :- !,
        map_description(head,_,D,L).


owl_as2prolog(objectProperty(_),[],_) :- !.
owl_as2prolog(dataProperty(_),[],_) :- !.
owl_as2prolog(annotationProperty(_),[],_) :- !.

% 
%  Mapping individuals
%  a. Generate a C(ID) for each desccription C in the Types list
%  b. Generate a p(ID,Value) for each value declaration in the Property
%  list. 
%

owl_as2prolog(classAssertion(C,I),L,_) :- !,
        map_description(fact,I,C,L).

owl_as2prolog(propertyAssertion(P,I,J), :-(property(P,I,J),none),_) :- !.

owl_as2prolog(owl(_,_,_,_),[],_) :- !.
owl_as2prolog(ontology(_,_),[],_) :- !.

owl_as2prolog(annotationAssertion(_,_,_), [], _) :- !.


	
% 
%  Mappings generated from the attributes of a property.
%  a. Functional and inverse functionals generate a 
%       sameIndividuals(X,Y) :- p(Z,X), P(Z,Y)
%  Transitive: p(X,Z) :- p(X,Y), p(Y,Z). 
%  Symmetric: p(X,Y) :- p(Y,X).
%  Inverse  : p(X,Y) :- inv(Y,X) and inv(X,Y) :- p(Y,X).
%

owl_as2prolog(functionalProperty(P), (property(sameIndividuals,x,y) :- (property(P,z,x),property(P,z,y))),_) :- !.
owl_as2prolog(inverseFunctionalProperty(P), (property(sameIndividuals,x,y) :- (property(P,z,x),property(P,z,y))),_) :- !.
owl_as2prolog(transitiveProperty(P), (property(P,x,y) :- (property(P,x,z),property(P,z,y))),_) :- !.
owl_as2prolog(symmetricProperty(P), (property(P,x,y) :- property(P,y,x)),_) :- !.
owl_as2prolog(reflexiveProperty(P), (property(P,x,x) :- property(P,x,y)),_) :- !. % TODO -- check
%owl_as2prolog(inverseProperties(P,Inv),[(property(P,x,y) :- property(Inv,y,x)),
%                                        (property(Inv,x,y) :- property(P,y,x))],_) :- !.
owl_as2prolog(inverseProperties(P,inverseOf(P)),none,_) :- !. % REDUNDANT - do nothing
owl_as2prolog(inverseProperties(inverseOf(P),P),none,_) :- !. % REDUNDANT - do nothing
owl_as2prolog(inverseProperties(P,Inv),[(PE :- IPE),(IPE2 :- PE2)], _) :- !,
        owl_as2prolog(propertyExpression(P),PE,head),
        owl_as2prolog(propertyExpression(inverseOf(Inv)),IPE,body),
        owl_as2prolog(propertyExpression(inverseOf(P)),PE2,body),
        owl_as2prolog(propertyExpression(Inv),IPE2,head).



% TODO: new OWL2 properties

% SWRL

owl_as2prolog(implies(A,C),(CP :- AP), _) :- !,
              owl_as2prolog(swrl(A),AP,body),
              owl_as2prolog(swrl(C),CP,head).

owl_as2prolog(swrl([]), true, _Type) :- !.
owl_as2prolog(swrl([A]), G, Type) :-
         !,
         owl_as2prolog(swrl(A),G,Type). % TODO: body list
owl_as2prolog(swrl([A|AL]), (G,Gs), Type) :-
         !,
         owl_as2prolog(swrl(A),G,Type),
         owl_as2prolog(swrl(AL),Gs,Type).

owl_as2prolog(swrl(description(C,X)),swrldescription(C,PX), Type) :-
        !,
        owl_as2prolog(swrl(X),PX,Type).


owl_as2prolog(swrl(A),swrlproperty(P,PX,PY), Type) :-
        A=..[P,X,Y],
        !,
        owl_as2prolog(swrl(X),PX,Type),
        owl_as2prolog(swrl(Y),PY,Type).
owl_as2prolog(swrl(i(V)),V,_) :- !.

% propertyChains
chain_to_goal(PL,ChainGoal) :-
        chain_to_goal(PL,x,v(1),ChainGoal).

chain_to_goal([P],V,_,Goal) :-
        !,
        (   P=inverseOf(PI)
        ->  Goal=property(PI,y,V)
        ;   Goal=property(P,V,y)).
chain_to_goal([P|PL],V,VN,(Goal,ChainGoal)) :-
        !,
        (   P=inverseOf(PI)
        ->  Goal=property(PI,VN,V)
        ;   Goal=property(P,V,VN)),
        VN=v(N),
        NPlus1 is N+1,
        chain_to_goal(PL,VN,v(NPlus1),ChainGoal).


% 
% Mapping functions (Perform convert operations on each element in a
% list).
% 

map_description(fact,X,Description,:-(DMap,none)) :- !,
	owl_as2prolog(description(Description,X),DMap,fact).

map_description(Type,X,Description,DMap) :- !,
	owl_as2prolog(description(Description,X),DMap,Type).
                 

% TODO

/** <module> generates logic programs from OWL2 ontologies

  ---+ Synopsis

==
:- use_module(bio(owl2_to_prolog_dlp)).

% 
demo:-
  nl.
  

==

---+ Details


This submodule converts an OWL ontology represented as OWL
abstract syntax terms into a Prolog program. The mapping implements the
idea of Description Logic Programs [Grossof]. Similar work has been also
done in the dlpconvert tool.

This extends Thea1 and the original Grossof rules to allow for certain
OWL2 features, currently limited to property expressions (inverse
properties and role chains)

---++ Options

 * disjunctive_datalog(DDL:boolean) - if true, will write rules in which head contains disjunctions
 * head_disjunction_symbol(Op:atom) - if true, and if disjunctive_datalog(true) then writes disjunctive head rules using Op as separator. For DLV, set Op='v'

*/
