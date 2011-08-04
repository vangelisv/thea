/* -*- Mode: Prolog -*- */

:- module(owl2_obo_writer,
          [
           owl_write_obo_syntax_file/2
          ]).

:- use_module(owl2_io).

:- multifile owl2_io:suffix_format/2.
owl2_io:suffix_format(obo,obo).

:- multifile owl2_io:save_axioms_hook/3.
owl2_io:save_axioms_hook(File,obo,Opts) :-
        owl_write_obo_syntax_file(File,Opts).

%% owl_write_obo_syntax_file(+OboFile,+Opts:list)
%
%  First write a file OboFile.owl, then use the owl2obo
%  converter to generate obo
owl_write_obo_syntax_file(File,Opts) :-
        var(File),
        tmp_file(obo,File),
        !,
        owl_write_obo_syntax_file(File,Opts),
        atomic_list_concat([cat,File],' ',Cmd),
        shell(Cmd).

owl_write_obo_syntax_file(File,Opts) :-
        atom_concat(File,'.owl',OwlFile),
        save_axioms(OwlFile,rdf_direct,Opts),
        concat_atom(['obolib-owl2obo','-o',File,OwlFile,'>/dev/null'],' ',Cmd),
        shell(Cmd).



