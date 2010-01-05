/* -*- Mode: Prolog -*- */

:- module(owl2_io,
          [load_axioms/1,
           load_axioms/2,
           load_axioms/3,
           save_axioms/2,
           save_axioms/3,
           convert_axioms/5
          ]).

:- use_module(owl2_model,[consult_axioms/1,axiom/1]).

:- multifile load_axioms_hook/3.
:- multifile save_axioms_hook/3.

%% load_axioms(+File)
% populates owl2_model axioms from File. Attempts to guess format from extension
load_axioms(File) :-
        load_axioms(File,_).

%% load_axioms(+File,+Fmt)
% populates owl2_model axioms from File.
% Fmt = rdf | owlx | prolog | ...
% (for non-standard fmts you may have to ensure the required io model is loaded
%  so the hooks are visible)
load_axioms(File,Fmt) :-
        load_axioms(File,Fmt,[]).

%% load_axioms(+File,+Fmt,+Opts)
% as load_axioms/2 with options
% Opts are Fmt specific - see individual modules for details
load_axioms(File,Fmt,Opts) :-
        var(Fmt),
        guess_format(File,Fmt,Opts),
        !,
        load_axioms(File,Fmt,Opts).
load_axioms(File,Fmt,_Opts) :-
        nonvar(Fmt),
        (   Fmt=prolog
        ;   Fmt=owlpl
        ;   Fmt=pl),
        !,
	load_prolog_axioms(File).
load_axioms(File,Fmt,Opts) :-
        load_handler(read,Fmt),
        load_axioms_hook(File,Fmt,Opts),
        !.
load_axioms(File,Fmt,Opts) :-
        throw(owl2_io('cannot parse fmt for',File,Fmt,Opts)).

load_prolog_axioms(File) :-
	\+ predicate_property(qcompile(_),_), % e.g. Yap
	!,
        style_check(-discontiguous),
	consult_axioms(File).
load_prolog_axioms(File) :-
        style_check(-discontiguous),
	style_check(-atom),	
	file_name_extension(Base, _Ext, File),
	file_name_extension(Base, qlf, QlfFile),
        debug(load,'checking for: ~w',[QlfFile]),
	(   exists_file(QlfFile),
	    time_file(QlfFile, QlfTime),
	    time_file(File, PlTime),
	    QlfTime >= PlTime
	->  consult_axioms(QlfFile)
	;   access_file(QlfFile, write)
	->  qcompile(File),
            consult_axioms(QlfFile)
        ;   debug(load,'  cannot write to qlf (permission problem?), loading from: ~w',[File]),
            consult_axioms(File)
	).

%% save_axioms(+File,+Fmt)
% saves owl2_model axioms to File.
% Fmt = rdf | owlx | prolog | ...
% (for non-standard fmts you may have to ensure the required io model is loaded
%  so the hooks are visible)
save_axioms(File,Fmt) :-
        save_axioms(File,Fmt,[]).

%% save_axioms(+File,+Fmt,+Opts)
% as save_axioms/2 with options
% Opts are Fmt specific - see individual modules for details
save_axioms(File,Fmt,_Opts) :-
        nonvar(Fmt),
        (   Fmt=prolog
        ;   Fmt=owlpl
        ;   Fmt=pl),
        !,
        (   nonvar(File)
        ->  tell(File)
        ;   true),
        forall(axiom(A),
               (   A=implies(_,_)
               ->  format('swrl:~q.~n',[A]) % ugly hack - assume owl2_model module for everything except this
               ;   format('~q.~n',[A]))),
        told.
save_axioms(File,Fmt,Opts) :-
        load_handler(write,Fmt),
        save_axioms_hook(File,Fmt,Opts),
        !.
save_axioms(File,Fmt,Opts) :-
        throw(owl2_io('cannot save fmt for',File,Fmt,Opts)).

convert_axioms(FileIn,FmtIn,FileOut,FmtOut,Opts) :-
        load_axioms(FileIn,FmtIn,Opts),
        save_axioms(FileOut,FmtOut,Opts).

% TODO - check if this is the best way of doing this
load_handler(Dir,Fmt) :-
        forall(format_module(Dir,Fmt,Mod),
	       ensure_loaded(library(thea2/Mod))).

guess_format(File,Fmt,_Opts) :-
        concat_atom(Toks,'.',File),
        reverse(Toks,[Suffix,_|_]),
        suffix_format(Suffix,Fmt).

suffix_format(pro,prolog).
suffix_format(prolog,prolog).
suffix_format(pl,prolog).
suffix_format(owlpl,prolog).
suffix_format(plsyn,plsyn).
suffix_format(owl,owl).
suffix_format(ttl,owl).
suffix_format(owlx,owlx).


:- multifile format_module/3.
format_module(read,rdf,owl2_from_rdf).
format_module(read,owl,owl2_from_rdf).
format_module(read,ttl,owl2_from_rdf).
format_module(read,xml,owl2_xml).
format_module(read,owlx,owl2_xml).
format_module(read,pl_swrl,swrl).
format_module(read,pl_swrl_owl,swrl).
format_module(read,plsyn,owl2_plsyn).

format_module(write,owl,owl2_export_rdf).
format_module(write,owlx,owl2_xml).
format_module(write,plsyn,owl2_plsyn).
format_module(write,dl_syntax,owl2_dl_syntax).
format_module(write,dlp,owl2_to_prolog_dlp).


/** <module>

  ---+ Synopsis

==
:- use_module(library('thea2/owl2_io')).
:- use_module(library('thea2/owl2_model')).

% reads in RDF/OWL and serializes to other formats
test :-
        load_axioms('testfiles/wine.owl'), % auto-detects RDF serialization
        save_axioms('testfiles/wine.owlpl',prolog),
        save_axioms('testfiles/wine.pl',plsyn),
        save_axioms('testfiles/wine.owlx',owlx),
        save_axioms('testfiles/wine.dlp',dlp),
        save_axioms('testfiles/wine.owlms',manchester).

==

---+ Details

Extensible: format-specific modules can define hooks

*/
