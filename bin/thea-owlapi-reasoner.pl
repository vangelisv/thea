:- use_module(library('thea/owl2_util')).
:- use_module(library('thea/swrl_rdf_hooks')).
:- use_module(library('thea/swrl')).
:- use_module(library('thea/owl2_io')).
:- use_module(library('thea/owl2_java_owlapi')).


run(FilesAtom) :-
        concat_atom(Args,' ',FilesAtom),
        %debug(owl2),
        forall(member(Arg,Args),
               load_axioms(Arg,_,[])),
        create_reasoner(Man,pellet,Reasoner),
        create_factory(Man,Fac),
        build_ontology(Man,Fac,Ont),
        (   is_consistent(Reasoner)
        ->  format(user_error,'Is Consistent. Reasoning...~n',[]),
            reasoner_classify(Reasoner,Man,Ont),
            format(user_error,'Done Reasoning~n',[]),
            forall(inferred_axiom(Reasoner,Fac,Ax),
                   format('~q.~n',[Ax]))
        ;   format('**INCONSISTENT**~n')),
        format(user_error,'Done!~n',[]).


