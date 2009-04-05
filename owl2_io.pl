/* -*- Mode: Prolog -*- */

:- module(owl2_io,
          [load_axioms/1,
           load_axioms/2,
           load_axioms/3,
           save_axioms/2,
           save_axioms/3
          ]).

:- multifile load_axioms_hook/3.
:- multifile save_axioms_hook/3.

load_axioms(File) :-
        load_axioms(File,_).
load_axioms(File,Fmt) :-
        load_axioms(File,Fmt,[]).
load_axioms(File,Fmt,Opts) :-
        load_handler(read,Fmt),
        load_axioms_hook(File,Fmt,Opts),
        !.
load_axioms(File,Fmt,Opts) :-
        throw(owl2_io('cannot parse fmt for',File,Fmt,Opts)).

save_axioms(File,Fmt) :-
        load_handler(write,Fmt),
        save_axioms(File,Fmt,[]).
save_axioms(File,Fmt,Opts) :-
        save_axioms_hook(File,Fmt,Opts),
        !.
save_axioms(File,Fmt,Opts) :-
        throw(owl2_io('cannot save fmt for',File,Fmt,Opts)).

load_handler(Dir,Fmt) :-
        forall(format_module(Dir,Fmt,Mod),
               ensure_loaded(Mod)).

:- multifile format_module/3.
format_module(read,rdf,owl2_from_rdf).
format_module(read,owl,owl2_from_rdf).
format_module(read,xml,owl2_xml).


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
