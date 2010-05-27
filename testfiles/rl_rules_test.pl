subClassOf(a,b).

	% subClassOf(b,c).
	% subClassOf(c,b).
equivalentClasses([c,b]).

subClassOf(a,a).
subClassOf(b,b).
subClassOf(c,d).
subClassOf(d,e).

classAssertion(c,i).

sameIndividual([i2,i1]).
sameIndividual([i1,i3]).
propertyAssertion(p,i1,v).

symmetricProperty(p).
sameIndividual([b,a]).
differentIndividuals([a,b]).

transitiveProperty(tp).
propertyAssertion(tp,a1,b1).
propertyAssertion(tp,b1,c1).

subPropertyOf(tp,supertp).
equivalentProperties([tp,tp2]).

classAssertion(c,x1).
equivalentClasses([c,intersectionOf([c1,c2])]).
equivalentClasses([b,unionOf([b1,b2,b3])])).
