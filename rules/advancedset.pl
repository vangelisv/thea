


% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
% level 2:
%  
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

% X<Y if X=^DL and X<all D in DL
entailed_2(subClassOf(X,Y),EL) :-
        nonvar(Y),
        pairwise_equivalent_class(Y,intersectionOf(DL)),
        debug(owl2_basic_reasoner,'testing for subclasses of named class ~w = ~w',[Y,DL]),
        \+ member(X<Y,EL),
        DL=[D|DL2],
        entailed(subClassOf(X,D),EL),
        (   DL2=[]
        ->  true
        ;   entailed_2(subClassOf(X,intersectionOf(DL2)),EL)).
%        forall(member(D,DL),
%               entailed(subClassOf(X,D),EL)).

entailed_2(subClassOf(X,Y),EL) :-
        nonvar(Y),
        nonvar(X),
        Y=intersectionOf(DL),
        debug(owl2_basic_reasoner,'testing for subclasses of class expression ~w',[DL]),
        \+ member(X<Y,EL),
        DL=[D|DL2],
        entailed(subClassOf(X,D),EL),
        (   DL2=[]
        ->  true
        ;   entailed_2(subClassOf(X,intersectionOf(DL2)),EL)).

entailed_2(subClassOf(X,Y),EL) :-
        nonvar(X),
        X=intersectionOf(DL),
        debug(owl2_basic_reasoner,'testing for superclasses of class expression ~w',[DL]),
        %nonvar(Y),
        %\+ member(X<Y,EL),
        member(D,DL),
        entailed(subClassOfReflexive(D,Y),EL).

entailed_2(subClassOf(X,Y),EL) :-
        nonvar(Y),
        nonvar(X),
        X=someValuesFrom(P,DX),
        Y=someValuesFrom(P,DY),
        debug(owl2_basic_reasoner,'testing for subsumption between existential restrictions ~w ~w',[X,Y]),
        entailed(subClassOf(DX,DY),EL).


entailed_2(subClassOf(X,Y),_) :-
        (   nonvar(X)
        ->  true
        ;   class(X)),
        debug(owl2_basic_reasoner,'testing for X=X ==> X < X... ~w ~w',[X,Y]),
        pairwise_equivalent_class(X,Y).

entailed_2(classAssertion(C,I),EL) :-
        propertyDomain(P,C),
        entailed(propertyAssertion(P,I,_),EL).

entailed_2(classAssertion(C,I),EL) :-
        propertyRange(P,C),
        entailed(propertyAssertion(P,_,I),EL).


entailed_2(classAssertion(C,I),EL) :-
        pairwise_equivalent_class(C,intersectionOf(DL)),
        entailed_2(classAssertion(DL,I),EL).

xxentailed_2(classAssertion(C,I),EL) :-
        nonvar(C),
        C=intersectionOf(DL),
        debug(owl2_basic_reasoner,'~w(~w) if all ~w satisfied',[C,I,DL]),
        entailed(individual(I)),
        % note: instead of forall we could enumerate all
        forall(member(D,DL),
               entailed_2(classAssertion(D,I),EL)).

entailed_2(classAssertion(C,I),EL) :-
        nonvar(C),
        C=intersectionOf(DL),
        debug(owl2_basic_reasoner,'~w(~w) if all ~w satisfied',[C,I,DL]),
        entailed(individual(I)),
        DL=[D|DL2],
        % note: instead of forall we could enumerate all
        entailed_2(classAssertion(D,I),EL),
        (   DL2=[]
        ->  true
        ;   entailed_2(classAssertion(intersectionOf(DL2),I),EL)).
        

entailed_2(classAssertion(C,I),EL) :-
        nonvar(C),
        C=someValuesFrom(P,Y),
        debug(owl2_basic_reasoner,'~w some ~w satisfied for ~w if exists...',[P,Y,I]),
        propertyAssertion(P,I,YI),
        entailed(classAssertion(Y,YI),EL).


entailed_2(classAssertion(Y,I),EL) :-
        entailed(individual(I)),
        pairwise_equivalent_class(Y,intersectionOf(DL)),
        forall(member(D,DL),
               entailed(classAssertion(D,I),EL)).
