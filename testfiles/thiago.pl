% library(win_menu) compiled into win_menu 0.02 sec, 21,016 bytes
% library(swi_hooks) compiled into pce_swi_hooks 0.00 sec, 3,784 bytes
% The graphical front-end will be used for subsequent tracing
% c:/users/thiago/appdata/roaming/swi-prolog/pl.ini compiled 0.08 sec, 1,499,784 bytes

/*
XPCE 6.6.66, July 2009 for Win64: XP 64-bit edition
Copyright (C) 1993-2009 University of Amsterdam.
XPCE comes with ABSOLUTELY NO WARRANTY. This is free software,
and you are welcome to redistribute it under certain conditions.
The host-language is SWI-Prolog version 5.11.7

For HELP on prolog, please type help. or apropos(topic).
         on xpce, please type manpce.

1 ?- use_module(library(thea2/owl2_model)).
% library(thea2/owl2_model) compiled into owl2_model 0.02 sec, 109,240 bytes
true.

2 ?- use_module(library(thea2/owl2_io)).
% library(thea2/owl2_io) compiled into owl2_io 0.02 sec, 16,824 bytes
true.

3 ?- load_axioms('http://testbed1.gprt.ufpe.br/~ascman/owls-tc4-swrl/ontology/books.owl', owl, [imports(true)]).
%     library(option) compiled into swi_option 0.00 sec, 14,216 bytes
%    library(sgml) compiled into sgml 0.00 sec, 53,560 bytes
%     rewrite compiled into rewrite 0.00 sec, 12,392 bytes
%     library(uri) compiled into uri 0.00 sec, 13,952 bytes
%     library(record) compiled into record 0.00 sec, 28,624 bytes
%    rdf_parser compiled into rdf_parser 0.02 sec, 120,208 bytes
%     library(gensym) compiled into gensym 0.00 sec, 4,600 bytes
%    rdf_triple compiled into rdf_triple 0.02 sec, 36,648 bytes
%   library(rdf) compiled into rdf 0.03 sec, 243,576 bytes
%    library(assoc) compiled into assoc 0.00 sec, 36,032 bytes
%   library(sgml_write) compiled into sgml_write 0.00 sec, 95,152 bytes
%   library(nb_set) compiled into nb_set 0.00 sec, 6,184 bytes
%   rdf_cache compiled into rdf_cache 0.00 sec, 18,608 bytes
%  library(semweb/rdf_db) compiled into rdf_db 0.06 sec, 567,552 bytes
%  library(semweb/rdf_edit) compiled into rdf_edit 0.02 sec, 77,896 bytes
%  library(semweb/rdfs) compiled into rdfs 0.02 sec, 28,048 bytes
%   library(utf8) compiled into utf8 0.00 sec, 13,808 bytes
%  library(url) compiled into url 0.02 sec, 123,552 bytes
%   library(readutil) compiled into read_util 0.00 sec, 16,776 bytes
%   library(socket) compiled into socket 0.02 sec, 11,392 bytes
%   library(base64) compiled into base64 0.00 sec, 17,688 bytes
%  library(http/http_open) compiled into http_open 0.03 sec, 93,688 bytes
% library(thea2/owl2_from_rdf) compiled into owl2_from_rdf 0.16 sec, 1,016,160 bytes
% Parsed "books.owl" in 0.02 sec; 0 triples
% Parsed "simplified_sumo.owl" in 0.02 sec; 0 triples
true.

4 ?- class(X), sub_string(X, Start, _, _, 'books.owl').
false.

5 ?- class(X), sub_string(X, Start, _, _, 'sumo.owl').
X = 'http://testbed1.gprt.ufpe.br/~ascman/owls-tc4-swrl/ontology/simplified_sumo.owl#AgriculturalProduct',
Start = 71 ;
X = 'http://testbed1.gprt.ufpe.br/~ascman/owls-tc4-swrl/ontology/simplified_sumo.owl#Product',
Start = 71 ;
X = 'http://testbed1.gprt.ufpe.br/~ascman/owls-tc4-swrl/ontology/simplified_sumo.owl#Artifact',
Start = 71 .

6 ?- load_axioms('http://testbed1.gprt.ufpe.br/~ascman/owls-tc4-swrl/ontology/books.owl', owl, [imports(false)]).
% Parsed "books.owl" in 0.02 sec; 0 triples
true.

7 ?- class(X), sub_string(X, Start, _, _, 'books.owl').
X = 'http://testbed1.gprt.ufpe.br/~ascman/owls-tc4-swrl/ontology/books.owl#A',
Start = 60 ;
X = 'http://testbed1.gprt.ufpe.br/~ascman/owls-tc4-swrl/ontology/books.owl#Article',
Start = 60 ;
X = 'http://testbed1.gprt.ufpe.br/~ascman/owls-tc4-swrl/ontology/books.owl#Author',
Start = 60 ;
X = 'http://testbed1.gprt.ufpe.br/~ascman/owls-tc4-swrl/ontology/books.owl#B',
Start = 60 .
*/

% :- assert(library_directory('C:/SW/Development/PL/')).

:- use_module(library(thea2/owl2_model)).
:- use_module(library(thea2/owl2_io)).

import :-
	load_axioms('http://testbed1.gprt.ufpe.br/~ascman/owls-tc4-swrl/ontology/books.owl', owl, [imports(true)]).

no_import:-
	load_axioms('http://testbed1.gprt.ufpe.br/~ascman/owls-tc4-swrl/ontology/books.owl', owl, [imports(false)]).

import1 :-
	owl_parse('http://testbed1.gprt.ufpe.br/~ascman/owls-tc4-swrl/ontology/books.owl', complete, complete, true).
