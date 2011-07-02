/* -*- Mode: Prolog -*- */

:- use_module(library('thea/owl2_model')).
:- use_module(library('thea/owl2_io')).
:- use_module(library('thea/owl2_basic_reasoner')).

class_freq(C,Num) :- aggregate(count,I,entailed(classAssertion(C,I)),Num).
num_individuals(Num) :- aggregate(count,I,C^classAssertion(C,I),Num).
class_prob(C,P) :- class_freq(C,Class_freq),num_individuals(Total),P is Class_freq/Total.
class_ic(C,IC) :- class_prob(C,P),IC is -log(P)/log(2).

common_ancestor(X,Y,A) :-
  entailed(subClassOf(X,A)),
  entailed(subClassOf(Y,A)).

least_common_ancestor(X,Y,A) :-
  common_ancestor(X,Y,A),
  \+ ((common_ancestor(X,Y,A2),
       A2\=A,
       entailed(subClassOf(A2,A)))).

individual_pair_class_ic(X,Y,CZ,IC) :-
        classAssertion(CX,X),
        classAssertion(CY,Y),
        least_common_ancestor(CX,CY,CZ),
        class_ic(CZ,IC).

semsim(X,Y,maxIC,MaxIC,MaxC) :-
        setof(IC-CZ,individual_pair_class_ic(X,Y,CZ,IC),ICClassPairsR),
        reverse(ICClassPairsR,MaxIC-MaxC).
        
        


