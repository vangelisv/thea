% :- assert(library_directory('C:/sw/development/pl')).
:- use_module(library(thea2/owl2_model)).
:- use_module(library(thea2/owl2_io)).

%
% Problem when loading two OWL files.
%

% Restart SWI, load testcase, execute load_1st... Ok...
load_1st :-
	load_axioms('http://testbed1.gprt.ufpe.br/~ascman/owls-tc4-swrl/services/1.2/BookToPublisherService.owl', owl, [imports(true)]).

% Restart SWI, load testcase, execute load_2nd... Ok...
load_2nd :-
	load_axioms('http://testbed1.gprt.ufpe.br/~ascman/owls-tc4-swrl/services/1.2/WaysOfOrderService.owl', owl, [imports(true)]).

% Restart SWI, load testcase, execute load_both... Fail...
load_both :-
	load_1st,
	print('now load 2nd'),nl,
	load_2nd.
