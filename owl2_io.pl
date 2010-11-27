/* -*- Mode: Prolog -*- */

:- module(owl2_io,
          [load_axioms/1,
           load_axioms/2,
           load_axioms/3,
           save_axioms/2,
           save_axioms/3,
           convert_axioms/5
          ]).

:- use_module(library(debug)).
:- use_module(owl2_model,[consult_axioms/1, axiom/1]).

%% load_axioms_hook(+File,+Fmt,+Opts)
% define this multifile predicate to add a new parser
:- multifile load_axioms_hook/3.
%% save_axioms_hook(+File,+Fmt,+Opts)
% define this multifile predicate to add a new parser
:- multifile save_axioms_hook/3.

%% load_axioms(+File)
% populates owl2_model axioms from File. 
% as load_axioms/3, guesses format based on file extension
load_axioms(File) :-
        load_axioms(File,_).

%% load_axioms(+File,+Fmt)
% as load_axioms/3, with no options
load_axioms(File,Fmt) :-
        load_axioms(File,Fmt,[]).

%% load_axioms(+File,+Fmt,+Opts)
%
% populates owl2_model axioms from File.
% ==
% Fmt = owl | owlx | prolog | plsyn | dlp | owlapi(_) | ...
% ==
% (for non-standard fmts you may have to ensure the required io model is loaded
%  so the hooks are visible)
%
% Opts are Fmt specific - see individual modules for details.
load_axioms(File,Fmt,Opts) :-
        var(Fmt),
        guess_format(File,Fmt,Opts),
        !,
        load_axioms(File,Fmt,Opts).
load_axioms(File,Fmt,Opts) :-
        nonvar(Fmt),
        (   Fmt=prolog
        ;   Fmt=owlpl
        ;   Fmt=pl),
        !,
	load_prolog_axioms(File,Opts).
load_axioms(File,Fmt,Opts) :-
        load_handler(read,Fmt),
        load_axioms_hook(File,Fmt,Opts),
        debug(load,'no hook for: ~w',[Fmt]),
        !.
load_axioms(File,Fmt,Opts) :-
        throw(owl2_io('cannot parse fmt for',File,Fmt,Opts)).

load_prolog_axioms(File) :-
        load_prolog_axioms(File,[]).
load_prolog_axioms(File,Opts) :-
	\+ predicate_property(qcompile(_),_), % e.g. Yap
	!,
        style_check(-discontiguous),
	consult_axioms(File),
        post_process_prolog_axioms(Opts).
load_prolog_axioms(File,Opts) :-
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
	),
        post_process_prolog_axioms(Opts).

post_process_prolog_axioms(Opts) :-
        \+ member(noOntologyAxiom(true),Opts),
        ontology(Ont),
        \+ ontologyAxiom(_,_),
        !,
        forall(axiom(A),
               assert_axiom(A,Ont)).
post_process_prolog_axioms(_).


%% save_axioms(+File,+Fmt)
% see save_axioms/3
save_axioms(File,Fmt) :-
        save_axioms(File,Fmt,[]).

%% save_axioms(+File,+Fmt,+Opts)
%
% saves owl2_model axioms to File.
% see load_axioms/3 for list of formats
%
% Some Supported Options:
% * ontology(Ont) - only save this ontology
% * exclude(ontologyAxiom)
save_axioms(File,Fmt,Opts) :-
        nonvar(Fmt),
        (   Fmt=prolog
        ;   Fmt=owlpl
        ;   Fmt=pl),
        !,
        (   nonvar(File)
        ->  tell(File)
        ;   true),
        option(ontology(Ont),Opts,_),
        forall(ontologyAxiom(Ont,A),
               (   A=implies(_,_)
               ->  format('swrl:~q.~n',[A]) % ugly hack - assume owl2_model module for everything except this
               ;   format('~q.~n',[A]))),
        % write orphans
        (   var(Ont)
        ->  forall((axiom(A),\+ontologyAxiom(_,A)),
                   format('~q.~n',[A]))
        ;   true),
        % write ontologyAxiom/2
	(   member(exclude(ontologyAxiom),Opts)
	->  true
	;   forall(owl2_model:ontologyAxiom(Ont,A),
		   format('~q.~n',[ontologyAxiom(Ont,A)]))),
        told.
save_axioms(File,Fmt,Opts) :-
        load_handler(write,Fmt),
        save_axioms_hook(File,Fmt,Opts),
        !.
save_axioms(File,Fmt,Opts) :-
        throw(owl2_io('cannot save fmt for',File,Fmt,Opts)).

%% convert_axioms(+FileIn,+FmtIn,+FileOut,+FmtOut,+Opts)
% combines load_axioms/3 with save_axioms/3
convert_axioms(FileIn,FmtIn,FileOut,FmtOut,Opts) :-
        load_axioms(FileIn,FmtIn,Opts),
        save_axioms(FileOut,FmtOut,Opts).

% TODO - check if this is the best way of doing this
load_handler(Dir,Fmt) :-
        forall(format_module(Dir,Fmt,Mod),
	       ensure_loaded(library(thea2/Mod))).

guess_format(File,Fmt,_Opts) :-
        atomic_list_concat(Toks,'.',File),
        reverse(Toks,[Suffix,_|_]),
        suffix_format(Suffix,Fmt).

:- multifile suffix_format/2.
suffix_format(pro,prolog).
suffix_format(prolog,prolog).
suffix_format(pl,prolog).
suffix_format(owlpl,prolog).
suffix_format(plsyn,plsyn).
suffix_format(owl,owl).
suffix_format(owl2,owl).
suffix_format(ttl,ttl).
suffix_format(owlx,owlx).
suffix_format(owlxml,owlx).
suffix_format(owlms,owlms).
suffix_format(owlapi(F),owlapi(F)).
suffix_format(owlapi,owlapi).
suffix_format(obo,obo).

:- multifile format_module/3.
format_module(read,rdf,owl2_from_rdf).
format_module(read,owl,owl2_from_rdf).
format_module(read,owl2,owl2_from_rdf).
format_module(read,ttl,owl2_from_rdf).
format_module(read,xml,owl2_xml).
format_module(read,owlx,owl2_xml).
format_module(read,owlms,owl2_manchester_parser).
format_module(read,pl_swrl,swrl).
format_module(read,pl_swrl_owl,swrl).
format_module(read,plsyn,owl2_plsyn).
format_module(read,owlapi,owl2_java_owlapi).
format_module(read,owlapi(_),owl2_java_owlapi).
format_module(read,obo,owl2_obo_parser).

format_module(write,owl,owl2_export_rdf).
format_module(write,owlx,owl2_xml).
format_module(write,ttl,owl2_export_rdf).
format_module(write,plsyn,owl2_plsyn).
format_module(write,dl_syntax,owl2_dl_syntax).
format_module(write,dlp,owl2_to_prolog_dlp).
format_module(write,owlapi(_),owl2_java_owlapi).


/** <module> Input/Output of OWL files

  ---+ Synopsis

Allows loading into or saving from an owl2_model.pl database
  
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

There are a variety of surface forms for OWL2. The goal is to
eventually support all of these for both parsing and writing. So far
we have stable support for -


 * RDF-OWL (read)  - owl2_from_rdf.pl
 * RDF-OWL (write) - owl2_export_rdf.pl
 * OWL-XML (read/write) - owl2_xml.pl


Note that in general you do not need to use these modules
directly. Instead use this module and predicates such as
load_axioms/1.

---++ Prolog OWL Syntax

In addition to the above standard formats, Thea allow reading and
writing to and from native prolog databases conforming to the
owl2_model.pl model. To see examples, execute the following two commands:

==
load_axioms('testfiles/wine.owl').
save_axioms('testfiles/wine.owlpl',prolog).
==

The resulting database will be conformant with owl2_model.pl

In addition, thea provides optional infix declarations to allow
embedding of manchester-style syntax directly in prolog. See the
owl2_plsyn.pl module for more details.

Use the format 'dlp' to write a description logic prolog program using
the owl2_to_prolog_dlp.pl module. This translates a subset of the
ontology to prolog rules, rather than embedding the OWL axioms as prolog facts.


---++ Java Hooks

If you need to read or write from other syntaxes, the
owl2_java_owlapi.pl provides seamless access to java parsers and
writers. Use a term owlapi(Format) to specify that the format should
be handled by the OWL API. For example:
 

==
save_axioms('wine.owl',owlapi(manchester)).
==



---++ Hooks



*/
