% use swrl.pl to convert these rules to a mixture of SWRL and OWL

% hasParent(?x1,?x2) ∧ hasBrother(?x2,?x3) ⇒ hasUncle(?x1,?x3) 
hasUncle(X,Z) :- hasParent(X,Y), hasBrother(Y,Z).

% role chain
hasGrandparent(X,Z) :- hasParent(X,Y), hasParent(Y,Z).

% subPropertyOf
hasSibling(X,Y) :- hasBrother(X,Y).
hasSibling(X,Y) :- hasSister(X,Y).

% FACTS
% translated to antecedent-free rules, which can then be converted to OWL2 individual axioms
hasBrother(f,u).
hasFather(c,f).
