/* -*- Mode: Prolog -*- */

:- module(owl2_io,[load_axioms/2,
                   save_axioms/2
                   ]).

:- multifile load_axioms_hook/2.
:- multifile save_axioms_hook/2.
load_axioms(File,Fmt) :-
        load_axioms_hook(File,Fmt),
        !.
load_axioms(File,Fmt) :-
        throw(owl2_io('cannot parse fmt for',File,Fmt)).
save_axioms(File,Fmt) :-
        save_axioms_hook(File,Fmt),
        !.
save_axioms(File,Fmt) :-
        throw(owl2_io('cannot save fmt for',File,Fmt)).


/** <module> 

  ---+ Synopsis

==
:- use_module(bio(owl2_io)).

% 
demo:-
  nl.
  

==

---+ Details

Extensible: format-specific modules can define hooks

*/
