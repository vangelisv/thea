/* -*- Mode: Prolog -*- */

:- module(owl2_obo_parser,
          [
           owl_parse_obo_syntax_file/2
           ]).

:- use_module(owl2_io).

:- multifile owl2_io:suffix_format/2.
owl2_io:suffix_format(obo,obo).


:- multifile owl2_io:load_axioms_hook/3.
owl2_io:load_axioms_hook(File,obo,Opts) :-
        owl_parse_obo_syntax_file(File,Opts).

owl_parse_obo_syntax_file(File,Opts) :-
        absolute_file_name(File,Abs),
        atom_concat(Abs,'.owl',OwlFile),
        %atom_concat('file://',OwlFile,OwlURI),
        debug(obo,'obo2owl ~w ==> ~w URI: ~w',[File,OwlFile,OwlFile]),
        obo2owl(File,OwlFile,OwlFile,Opts).

%% obo2owl(+In,+Out,URI,Opts) :-
obo2owl(In,Out,URI,Opts) :-
        \+ exists_file(Out),
        !,
        debug(obo,'creating new owl file: ~w',[URI]),
        run_obo2owl(In,Out,URI,Opts).
obo2owl(In,Out,_URI,Opts) :-
        time_file(In,InTime),
        time_file(Out,OutTime),
        debug(obo,'comparing ~w < ~w',[InTime,OutTime]),
        InTime < OutTime,
        \+ member(force(true),Opts),
        !,
        debug(obo,'~w < ~w -- using existing owl file',[InTime,OutTime]),
        load_axioms(Out,rdf_direct,Opts).  % USE NEW PARSER
obo2owl(In,Out,URI,Opts) :-
        run_obo2owl(In,Out,URI,Opts).

run_obo2owl(In,Out,URI,Opts) :-
        concat_atom(['obolib-obo2owl --default-ontology default --allow-dangling','-o',URI,In,'>/dev/null'],' ',Cmd),
        debug(obo,'CMD: ~w',[Cmd]),
        shell(Cmd),
        load_axioms(Out,rdf_direct,Opts). % USE NEW PARSER

