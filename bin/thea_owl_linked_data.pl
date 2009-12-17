
%  #!/usr/bin/swipl -L0 -G0 -A0 -T0 -q -g main -t halt -s

:- use_module('../owl2_io.pl').
:- use_module('../owl2_from_rdf.pl').

main :-
	owl_parse_rdf('http://dbpedia.org/data/Velodrom',[imports(true),clear(complete)]).


deref(URI) :-
	owl_parse_rdf(URI,[imports(true),clear(complete)]).

what_axioms(All,ClassAssertions,PropertyAssertions) :-
	aggregate_all(count,owl2_model:axiom(classAssertion(_,_)),ClassAssertions),
	aggregate_all(count,owl2_model:axiom(propertyAssertion(_,_,_)),PropertyAssertions),
	aggregate_all(count,owl2_model:axiom(_),All).


