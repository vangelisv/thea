/* -*- Mode: Prolog -*- */

:- use_module(owl2_io).
:- use_module(owl2_lcs).
:- use_module(owl2_reasoner).
:- use_module(owl2_model).
:- use_module(owl2_util).
:- use_module(owl2_plsyn).
:- use_module(owl2_graph_reasoner).

:- begin_tests(owl2_lcs,[setup(init_axioms),cleanup(retract_all_axioms)]).

init_axioms :-
            load_axioms('testfiles/lctest.plsyn',plsyn),
            assume_entity_declarations.


/*
test(cs, [forall(cs_test_case(A,B,ExpectedResult)),
          true(Result = pass(_))]) :-
        run_cs_test(A,B,ExpectedResult,Result).
*/

test(cs, [forall(cs_test_case(A,B,ExpectedResult)),
          true(Result = ExpectedResult)]) :-
        run_cs_test(A,B,ExpectedResult,Result).

test(lcs, [forall(lcs_test_case(A,B,ExpectedResult)),
          true(Result = ExpectedResult)]) :-
        run_lcs_test(A,B,Result).

test(pivot1) :-
        equivalent_to(probe_2,X),
        findall(Y,description_pivot(X,Y),Ys)
        maplist(writeln,Ys),
        length(Ys,6).

test(pivot2) :-
        description_pivot(someValuesFrom(p,x),intersectionOf(L)),
        select(x,L,[someValuesFrom(inverseOf(p),'owl:Thing')]).

test(lcs_i) :-
        run_lcs_test(o1,o2,_).

test(sub) :-
        owl2_lcs:is_subsumed_by_chk(intersectionOf([someValuesFrom(has_part,
                                                                   intersectionOf([axon_terminal,someValuesFrom(has_quality,degenerated)])),
                                                    someValuesFrom(part_of,hippocampus)]),
                                    someValuesFrom(has_part,intersectionOf([axon_terminal,someValuesFrom(has_quality,degenerated)])),
                                    []
                                   ).



run_lcs_test(A,B,LCS) :-
        plsyn_owl(A,Ax),
        plsyn_owl(B,Bx),
        class_pair_least_common_subsumer(Ax,Bx,LCSx,[]),
        plsyn_owl(LCS,LCSx),
        debug(test,'lcs(~w,~w) = ~w',[A,B,LCS]).

run_cs_test(A,B,ExpectedResult,LCS) :-
        plsyn_owl(A,Ax),
        plsyn_owl(B,Bx),
        class_pair_common_subsumer_ext(Ax,Bx,LCSx,[]),
        plsyn_owl(LCS,LCSx),
        debug(test,'lcs(~w,~w) = ~w',[A,B,LCS]),
        LCS = ExpectedResult,
        !.

lcs_test_case(small_green_left_eye,
              deformed_blue_right_eye,
              eye and has_quality some color and has_quality some morphology).

lcs_test_case(axon_terminals_degenerated_in_ca2,
              axon_terminals_degenerated_in_ca3,
              has_part some (axon_terminal and has_quality some degenerated) and part_of some hippocampus).

% we can only infer the weaker overlaps relation here..
lcs_test_case(organism and has_part some axon_terminals_degenerated_in_ca2,
              organism and has_part some axon_terminals_degenerated_in_ca3,
              organism and has_part some (axon_terminal and has_quality some degenerated) and overlaps some hippocampus).

lcs_test_case(organism and bearer_of some axon_terminals_degenerated_in_ca2,
              organism and bearer_of some axon_terminals_degenerated_in_ca3,
              z).


cs_test_case(organism and has_part some axon_terminals_degenerated_in_ca2,
             organism and has_part some axon_terminals_degenerated_in_ca2,
             organism and has_part some axon_terminals_degenerated_in_ca2).


:- end_tests(owl2_lcs).


% ----------------------------------------
% TEST SET 2
% ----------------------------------------

:- begin_tests(owl2_lcs2,[setup(init_axioms),cleanup(retract_all_axioms)]).

init_axioms :-
            load_axioms('testfiles/lctest2.plsyn',plsyn),
            assume_entity_declarations.



test(lcs_i) :-
        run_lcs_test(o1,o2,_).


run_lcs_test(A,B,LCS) :-
        plsyn_owl(A,Ax),
        plsyn_owl(B,Bx),
        class_pair_least_common_subsumer(Ax,Bx,LCSx,[]),
        plsyn_owl(LCS,LCSx),
        debug(test,'lcs(~w,~w) = ~w',[A,B,LCS]).



:- end_tests(owl2_lcs2).
