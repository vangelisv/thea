/* -*- Mode: Prolog -*- */

:- module(owl2_fol,
          [
           ]).

:- use_module(owl2_model).
:- use_module(owl2_from_rdf,[collapse_ns/4]).

:- multifile owl2_io:save_axioms_hook/3.
owl2_io:save_axioms_hook(File,fol,Opts) :-
        (   nonvar(File) -> tell(File) ; true),
        write_owlaxioms_as_fol(Opts),
        told.

write_owlaxioms_as_fol(Opts) :-
        forall(axiom(Axiom),
               write_owlaxiom_as_fol(Axiom,Opts)).

write_owlaxiom_as_fol(Ax,_):-
        owlaxiom_to_fol(Ax,Fol,_),
        writeln(Fol).



%% owlaxiom_to_fol(+OwlAsTerm, -ResultTerm, ?Mode) 
%
%	 Predicate to convert a Thea prolog OWL abstract term into
%	 FOL sentence term

owlaxiom_to_fol(class(_),none,_) :- !.

owlaxiom_to_fol(equivalentClasses([_]),none,_) :- !.

% this is illegal, but pass-through silently anyway
owlaxiom_to_fol(equivalentClasses([]),none,_) :- !.


% more general form of above clause
owlaxiom_to_fol(equivalentClasses(L),SL,_) :- !,
        findall(forall([X],iff(TC,TD)),
                (   member(C,L),
                    member(D,L),
                    C@<D,
                    %X=x,
                    owlaxiom_to_fol(description(C,X),TC,_),
                    owlaxiom_to_fol(description(D,X),TD,_)),
                SL).

owlaxiom_to_fol(disjointClasses(L),RL,_) :- !,
        findall(not(exists([X],and(TC,TD))),
                (   member(C,L),
                    member(D,L),
                    C@<D,
                    %X=x,
                    owlaxiom_to_fol(description(C,X),TC,_),
                    owlaxiom_to_fol(description(D,X),TD,_)),
                RL).

owlaxiom_to_fol(differentIndividuals(_),none,_) :- !.


% 
% Subclass(Class,Superclass) ==> C(X) implies S(X) or S(X) :- C(X).
%

owlaxiom_to_fol(subClassOf(A,B),R,_) :-
        X=x,
        owlaxiom_to_fol(description(A,X),Rb,_),
        owlaxiom_to_fol(description(B,X),Rh,_),
        !,     
        R = forall([X],if(Rb,Rh)).

% TODO: introduced in OWL2
owlaxiom_to_fol(description(hasSelf(_),_),false,fact):- !.

owlaxiom_to_fol(description(intersectionOf(DL),X),R,Param):- !,
	owlaxiom_to_fol(description_list(DL,X,and),R,Param).

owlaxiom_to_fol(description(unionOf(DL),X),R,Param):- !,
        owlaxiom_to_fol(description_list(DL,X,or),R,Param). 


%
% Complement of (Not) is not handled in this conversion
%
% TODO: 
owlaxiom_to_fol(description(complementOf(_),_),false,_) :- !.


%
% OneOf TODO
%

owlaxiom_to_fol(description(oneOf(_),_),false,_) :- !.


%
% Value property description generates a property term (predicate)
%

owlaxiom_to_fol(description(hasValue(PropertyID,Value),X),R,_) :- 
	R = [PropertyID,X,Value],!.

%
% Universal property description. See table above
%


owlaxiom_to_fol(description(allValuesFrom(PropertyID,Descr),_),R,_) :-  !,
	owlaxiom_to_fol(description(Descr,y),D,_),
	R =  forall([y],if([PropertyID,x,y]), D).


%
% Existential property description. See table above
%

owlaxiom_to_fol(description(someValuesFrom(PropertyID,Descr),_),R,body) :-  !,
	owlaxiom_to_fol(description(Descr,y),D,body),
	R =  exists([y],and(D,[PropertyID,x,y])).

%
% Cardinalities are not handled in this conversion
%

owlaxiom_to_fol(description(maxCardinality(_,_),_),false,_) :-  !.
owlaxiom_to_fol(description(minCardinality(_,_),_),false,_) :-  !.
owlaxiom_to_fol(description(exactCardinality(_,_),_),false,_) :-  !.

% QCRs: added in OWL2
owlaxiom_to_fol(description(maxCardinality(_,_,_),_),false,_) :-  !.
owlaxiom_to_fol(description(minCardinality(_,_,_),_),false,_) :-  !.
owlaxiom_to_fol(description(exactCardinality(_,_,_),_),false,_) :-  !.


%
% Any other description is taken to be a named class
%

owlaxiom_to_fol(description(Any,X),[Any,X],_) :- !.



%
% Handling of description lists in head and bodies of rules
%

owlaxiom_to_fol(description_list([],_,_),[],_) :- !.

owlaxiom_to_fol(description_list([Descr],X,_),R,body) :- !,
	owlaxiom_to_fol(description(Descr,X),R,body).

owlaxiom_to_fol(description_list([Descr|Rest],X,Separator),T,Param) :-
	owlaxiom_to_fol(description(Descr,X),H,Param),!,	
	owlaxiom_to_fol(description_list(Rest,X,Separator),Tail,Param),
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

owlaxiom_to_fol(subPropertyOf(P,SuperP),forall([x,y],if(SPE,PE)),_) :- !,
        owlaxiom_to_fol(propertyExpression(P),PE,head),
        owlaxiom_to_fol(propertyExpression(SuperP),SPE,body).

owlaxiom_to_fol(propertyExpression(inverseOf(P)),[P,y,x], _) :- !.

owlaxiom_to_fol(propertyExpression(propertyChain(PL)),ChainGoal, _) :-
        chain_to_goal(PL,ChainGoal).


owlaxiom_to_fol(propertyExpression(P),[P,x,y], _) :- !.


owlaxiom_to_fol(propertyDomain(P,D),forall([x,var],(if([P,x,var],DT))), _) :- !,
        owlaxiom_to_fol(description(D,x),DT,_).


owlaxiom_to_fol(propertyRange(P,D),forall([x,var],(if([P,var,x],DT))), _) :- !,
        owlaxiom_to_fol(description(D,x),DT,_).


map_description(head,_,D,L).


owlaxiom_to_fol(objectProperty(_),[],_) :- !.
owlaxiom_to_fol(dataProperty(_),[],_) :- !.
owlaxiom_to_fol(annotationProperty(_),[],_) :- !.

% 
%  Mapping individuals
%  a. Generate a C(ID) for each desccription C in the Types list
%  b. Generate a p(ID,Value) for each value declaration in the Property
%  list. 
%

owlaxiom_to_fol(classAssertion(C,I),[C,I],_) :- !.

owlaxiom_to_fol(propertyAssertion(P,I,J), [P,I,J],_) :- !.

owlaxiom_to_fol(owl(_,_,_,_),[],_) :- !.
owlaxiom_to_fol(ontology(_,_),[],_) :- !.

owlaxiom_to_fol(annotationAssertion(_,_,_), [], _) :- !.


	
% 
%  Mappings generated from the attributes of a property.
%  a. Functional and inverse functionals generate a 
%       sameIndividuals(X,Y) :- p(Z,X), P(Z,Y)
%  Transitive: p(X,Z) :- p(X,Y), p(Y,Z). 
%  Symmetric: p(X,Y) :- p(Y,X).
%  Inverse  : p(X,Y) :- inv(Y,X) and inv(X,Y) :- p(Y,X).
%

owlaxiom_to_fol(functionalProperty(P), forall([x,y,z],if(and([P,z,x],[P,z,y]), x=y)),_) :- !.
owlaxiom_to_fol(inverseFunctionalProperty(P), forall([x,y,z],if(and([P,x,z],[P,y,z]), x=y)),_) :- !.
owlaxiom_to_fol(transitiveProperty(P), forall([x,y,z],if(and([P,x,z],[P,z,y]), [P,x,y])),_) :- !.
owlaxiom_to_fol(symmetricProperty(P), forall([x,y],if([P,x,y],[P,y,x])),_) :- !.
%owlaxiom_to_fol(reflexiveProperty(P), (property(P,x,x) :- property(P,x,y)),_) :- !. % TODO -- check
%owlaxiom_to_fol(inverseProperties(P,Inv),[(property(P,x,y) :- property(Inv,y,x)),
%                                        (property(Inv,x,y) :- property(P,y,x))],_) :- !.
owlaxiom_to_fol(inverseProperties(P,inverseOf(P)),none,_) :- !. % REDUNDANT - do nothing
owlaxiom_to_fol(inverseProperties(inverseOf(P),P),none,_) :- !. % REDUNDANT - do nothing
%owlaxiom_to_fol(inverseProperties(P,Inv),and(PE :- IPE),(IPE2 :- PE2)], _) :- !,
%        owlaxiom_to_fol(propertyExpression(P),PE,head),
%        owlaxiom_to_fol(propertyExpression(inverseOf(Inv)),IPE,body),
%        owlaxiom_to_fol(propertyExpression(inverseOf(P)),PE2,body),
%        owlaxiom_to_fol(propertyExpression(Inv),IPE2,head).



% TODO: new OWL2 properties

% SWRL

owlaxiom_to_fol(implies(A,C),(CP :- AP), _) :- !,
              owlaxiom_to_fol(swrl(A),AP,body),
              owlaxiom_to_fol(swrl(C),CP,head).

owlaxiom_to_fol(swrl(L),PL, Type) :- !,
         is_list(L),
         !,
         findall(P,(member(A,L),owlaxiom_to_fol(swrl(A),P,Type)),PL). % TODO: body list

owlaxiom_to_fol(swrl(A),swrlproperty(P,PX,PY), Type) :- !,
        A=..[P,X,Y],
        !,
        owlaxiom_to_fol(swrl(X),PX,Type),
        owlaxiom_to_fol(swrl(Y),PY,Type).
owlaxiom_to_fol(swrl(i(V)),V,_) :- !.

% propertyChains
chain_to_goal(PL,Goal) :-
        chain_to_goal(PL,x,v(1),ChainGoal),
        Goal =.. [and|ChainGoal].

chain_to_goal([P],V,_,[Goal]) :-
        !,
        (   P=inverseOf(PI)
        ->  Goal=[PI,y,V]
        ;   Goal=[P,V,y]).
chain_to_goal([P|PL],V,VN,[Goal|ChainGoal]) :-
        !,
        (   P=inverseOf(PI)
        ->  Goal=[PI,VN,V]
        ;   Goal=[P,V,VN]),
        VN=v(N),
        NPlus1 is N+1,
        chain_to_goal(PL,VN,v(NPlus1),ChainGoal).


% 
% Mapping functions (Perform convert operations on each element in a
% list).
% 

map_description(fact,X,Description,:-(DMap,none)) :- !,
	owlaxiom_to_fol(description(Description,X),DMap,fact).

map_description(Type,X,Description,DMap) :- !,
	owlaxiom_to_fol(description(Description,X),DMap,Type).
                 

% TODO

/** <module> generates logic programs from OWL2 ontologies

  ---+ Synopsis

==
:- use_module(bio(owl2_fol)).

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
