
disjoint_classes_to_pairwise(Set,Symmetric) :-
        forall((member(X,Set),
                member(Y,Set),
                (   Symmetric
                ->  X \= Y
                ;   X @< Y)),
               assert_axiom(disjointClasses([X,Y]))).

        
