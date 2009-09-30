
%  #!/usr/bin/swipl -L0 -G0 -A0 -T0 -q -g main -t halt -s

:- use_module('../owl2_util.pl').
:- use_module('../swrl_rdf_hooks.pl').
:- use_module('../swrl.pl').
:- use_module('../owl2_io.pl').
:- use_module('../owl2_from_rdf.pl').

main :-
        current_prolog_flag(argv, Arguments),
        % append(_SytemArgs, [--|Args], Arguments), !,
        set_prolog_flag(verbose,normal),
	Args = ['--format', 'owl', '--debug', bench, '--import', '--statistics', '../benchmarks/FBbt.owl'],
        parse_args(Args,Opts),
        findall(X,
                member(loadopt(X),Opts),
                LoadOpts),
        (   member(format(Fmt),LoadOpts)
        ->  true
        ;   true),
        get_time(T1),
        forall(member(rest(F),Opts),
               % load_axioms(F,Fmt,LoadOpts)),
	owl_parse_rdf(F,LoadOpts)),
        get_time(T2),
        LoadTime is T2-T1,
        debug(bench,'load_time: ~w',[LoadTime]),
        (   member(statistics(true),Opts)
        ->  statistics
        ;   true),
        (   member(prolog(true),Opts)
        ->  prolog
        ;   true).


parse_args([],[]).
parse_args(Args,[Opt|Opts]) :-
        parse_arg(Args,Rest,Opt),
        !,
        parse_args(Rest,Opts).
parse_args([A|Args],[rest(A)|Opts]) :-
        parse_args(Args,Opts).

parse_arg(['--debug',D|L],L,null) :- debug(D).
parse_arg(['--prolog'|L],L,prolog(true)).
parse_arg(['--statistics'|L],L,statistics(true)).
parse_arg(['--import'|L],L,loadopt(imports(true))).
parse_arg(['--format',Fmt|L],L,loadopt(format(Fmt))).
parse_arg(['-to',Fmt|L],L,fmt(Fmt)).
