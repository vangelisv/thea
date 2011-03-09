% :- assert(library_directory('C:/sw/development/pl')).
:- use_module(library(thea2/owl2_model)).
:- use_module(library(thea2/owl2_io)).

%
% rdf:List axiom
%
g :- load_axioms('list-example.owl').
