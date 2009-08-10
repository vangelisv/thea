
% Consider a hypothetical ontology that by default follows a strict
% jointly-exhaustive pairwise-disjoint paradigm (JEPD), but with
% occasional exceptions that are explicitly declared using a specified
% annotation property. We can automate the generation of these axioms
% using the following goal, which can be evaluated in a failure-driven
% loop:

enforce_jepd :-
        % no exceptions
        enforce_jepd_unless(_,fail).

enforce_jepd_unless_unvetted :-
        % default exception rule:
        enforce_jepd_unless(X,annotationAssertion('http://example.org#status',X,'http://example.org#unvetted')).


%% enforce_jepd_unless(+Template:variable, +ExceptionGoal:goal) is det
enforce_jepd_unless(X,ExceptionGoal) :-
        setof(X,(subClassOf(X,Y),
                 \+ ExceptionGoal)
             Xs),
        Xs=[_,_|_],             % at least 2 subclasses
        assert_axiom(disjointUnion(Y,Xs)),
        format(user_error,'Enforced JEPD for subclasses of ~w~n',[Y]),
        fail.                   % failure-driven loop
enforce_jepd_unless(_,_).



