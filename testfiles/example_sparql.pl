:- use_module(library(semweb/sparql_client)).

go(Row) :-
      sparql_query('select * where { ?x rdfs:label "Thessaloniki" }', Row,
		   [ host('dbpedia.org'), path('/sparql')]).

go1(Row) :-
	sparql_query('PREFIX fb: <http://rdf.freebase.com/ns/> PREFIX dbpedia: <http://dbpedia.org/resource/> PREFIX geo-ont: <http://www.geonames.org/ontology#> CONSTRUCT {?loc ?person ?stateM} WHERE {     ?loc fb:location.location.people_born_here ?person .   ?person fb:people.deceased_person.cause_of_death dbpedia:Vascular_disease .     ?loc geo-ont:parentFeature ?stateM .     ?stateM geo-ont:name "Massachusetts" ; geo-ont:featureClass geo-ont:A }',Row,[ host('factforge.net'), path('/sparql'), search([format=rdf])]).

/*
http://factforge.net/sparql?query=PREFIX+fb%3A+%3Chttp%3A%2F%2Frdf.freebase.com%2Fns%2F%3E%0D%0APREFIX+dbpedia%3A+%3Chttp%3A%2F%2Fdbpedia.org%2Fresource%2F%3E%0D%0APREFIX+geo-ont%3A+%3Chttp%3A%2F%2Fwww.geonames.org%2Fontology%23%3E%0D%0A%0D%0ACONSTRUCT+%7B%3Floc+%3Fperson+%3FstateM%7D%0D%0AWHERE+%7B%0D%0A++++%3Floc+fb%3Alocation.location.people_born_here+%3Fperson+.%0D%0A++++%3Fperson+fb%3Apeople.deceased_person.cause_of_death+dbpedia%3AVascular_disease+.%0D%0A++++%3Floc+geo-ont%3AparentFeature+%3FstateM+.%0D%0A++++%3FstateM+geo-ont%3Aname+%22Massachusetts%22+%3B+geo-ont%3AfeatureClass+geo-ont%3AA+%0D%0A%7D&_implicit=false&implicit=true&_equivalent=false&format=srx

*/

go2(Row) :- sparql_query(' CONSTRUCT {?s ?p ?o} WHERE {?s ?p ?o .} LIMIT 100',
			 Row,
			 [ host('factforge.net'), path('/sparql'), search([format=rdf])]).

