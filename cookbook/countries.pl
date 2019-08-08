% Start thea using
%
%     thea-jpl --prolog

l :-
    load_axioms('testfiles/country.owl').
r :-
    initialize_reasoner(pellet,Reasoner),
    reasoner_ask(Reasoner,propertyAssertion('http://www.co-ode.org/roberts/country.owl#adjacentTo',C1,C2)),
    retract_axiom(propertyAssertion(C1,'c:adjacentTo',C2)),
    Desc = someValuesFrom('c:hasLandBoundry',intersectionOf('c:LandBoundryFragment',
                                                            hasValue('c:boundaryOf',C2))),
    assert_axiom(classAssertion(Desc,C2)),
    fail.

s :-
    save_axioms('countries2.owl',owl).

