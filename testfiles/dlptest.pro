y(A) :- x(A).

ab(X) :- a(X) ; b(X).


a(X) :- ab(X).
b(X) :- ab(X).

efg(X) :- e(X),f(X),g(X).

s(X,Y) :- r(X,Y).
t(X,Y) :- r(Y,X). % invert order

uncle_of(X,Y) :- brother_of(X,Z),father_of(Z,Y).



/*
equivalentClasses([ab,unionOf([a,b])]).
subClassOf(ab,unionOf([a,b])).
subClassOf(unionOf([a,b]),ab).
subClassOf(x,y).
equivalentClasses([efg,intersectionOf([e,f,g])]).

subPropertyOf(r,s).
subPropertyOf(r,inverseOf(t)).
subPropertyOf(r,propertyChain([s,t])).
subPropertyOf(r,propertyChain([s,inverseOf(t)])).
subPropertyOf(r,propertyChain([s,u,w])).
subPropertyOf(r,propertyChain([s,t,u,v,inverseOf(w),x])).

propertyDomain(qualityOf,quality).
propertyRange(hasQuality,quality).

subClassOf(nuc,someValuesFrom(partOf,cell)).
subClassOf(mice,allValuesFrom(eats,unionOf([cheese,cookies]))).

classAssertion(foo,x).
propertyAssertion(r,a,b).
*/


