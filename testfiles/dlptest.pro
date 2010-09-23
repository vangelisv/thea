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


