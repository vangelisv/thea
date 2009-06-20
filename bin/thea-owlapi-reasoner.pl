:- use_module(library('thea2/owl2_util')).
:- use_module(library('thea2/swrl_rdf_hooks')).
:- use_module(library('thea2/swrl')).
:- use_module(library('thea2/owl2_io')).
:- use_module(library('thea2/owl2_java_owlapi')).


run(Args) :-
        debug(owl2),
        forall(member(Arg,Args),
               load_axioms(Arg,_,[])),
        create_reasoner(Man,pellet,Reasoner),
        create_factory(Man,Fac),
        build_ontology(Man,Fac,Ont),
        reasoner_classify(Reasoner,Man,Ont),
        forall(inferred_axiom(Reasoner,Fac,Ax),
               format('~q.~n',[Ax])).

