/*
    lkjkj lk

*/

:- module(thea_owllink,[main/1]).

:- use_module('../owl2_util.pl').
:- use_module('../owl2_model.pl').
:- use_module('../owl2_from_rdf.pl').
:- use_module('../owl2_owllink.pl').
:- use_module(library('http/http_client')).


/** <module> Test code for OWLLink interface

This module processes structured comments and generates both formal
mode declarations from them as well as documentation in the form of
HTML or LaTeX
zdfsa

 @author  Vangelis Vassiliadis
 @license GPL

*/




main(URL) :-

    % OWLLink Example 1 - Get Descriptions
    owl_link(URL,[getDescription],Response1,
	     [reasoner,
	      request_file='../examples/owllink/thea-owllink-example-GetDescription-request.xml',
	      response_file='../examples/owllink/owllink-example-GetDescription-response-20091016.xml']),
    print('--------------------'),nl,print(Response1),nl,

    % OWLLink Example 2 - Create KB
    owl_link(URL,[createKB([kb='http://owllink.org/examples/KB_1'],[]),
		  tell('http://owllink.org/examples/KB_1',
		       [subClassOf('A','B'),
			subClassOf('B','C')]),
		  isClassSatisfiable('http://owllink.org/examples/KB_1','A'),
		  releaseKB('http://owllink.org/examples/KB_1')
		 ],Response2,
	     [reasoner,
	      request_file='../examples/owllink/thea-owllink-example-CreateKB-request.xml',
	      response_file='../examples/owllink/owllink-example-CreateKB-response-20091016.xml']),
    print('--------------------'),nl,print(Response2),nl,

    % OWLLink Example 3	- Get Settings

    owl_link(URL,[createKB([kb='http://owllink.org/examples/KB_1'],[]),
		  getSettings('http://owllink.org/examples/KB_1'),
		  releaseKB('http://owllink.org/examples/KB_1')],Response3,
	     [reasoner,
	      request_file='../examples/owllink/thea-owllink-example-RetrieveSettings-request.xml',
	      response_file='../examples/owllink/owllink-example-GetSettings-response-20091016.xml'
	     ]),
    print('--------------------'),nl,print(Response3),nl,


    % OWLLink Example 4 - Set
    owl_link(URL,[createKB([kb='http://owllink.org/examples/KB_1'],[]),
		  set('http://owllink.org/examples/KB_1','abbreviatesIRIs',[false]),
		  getSettings('http://owllink.org/examples/KB_1'),
		  releaseKB('http://owllink.org/examples/KB_1')],Response4,
	     [reasoner,
	      request_file='../examples/owllink/thea-owllink-example-SetSettings-request.xml',
	      response_file='../examples/owllink/owllink-example-Set-response-20091016.xml']),
    print('--------------------'),nl,print(Response4),nl,

    % OWLLink Example 5 - Prefixes
    owl_link(URL,[createKB([kb='http://owllink.org/examples/KB_2'],
					['Prefix'(name='test:',fullIRI='http://www.owllink.orgtest/ont'),
					'Prefix'(name='ont:',fullIRI='http://owllink.org/examples/ontology')]),
		  tell('http://owllink.org/examples/KB_2',
		       [subClassOf('test:ClassA','ont:ClassA')]),
		  set('http://owllink.org/examples/KB_2','abbreviatesIRIs',[false]),
		  getSubClasses('http://owllink.org/examples/KB_2','http://owllink.org/examples/ontology#ClassA',false),
		  getSubClasses('http://owllink.org/examples/KB_2','ont:ClassA',false),
		  set('http://owllink.org/examples/KB_2','abbreviatesIRIs',[true]),
		  getSubClasses('http://owllink.org/examples/KB_2','http://owllink.org/examples/ontology#ClassA',false),
		  releaseKB('http://owllink.org/examples/KB_2')
		 ],
	     Response5,
	     [reasoner,
	      request_file='../examples/owllink/thea-owllink-example-Prefixes-request.xml',
	      response_file='../examples/owllink/owllink-example-Prefix-response-20091016.xml']),
    print('--------------------'),nl,print(Response5),nl,

    % OWLLink Example 6
    owl_link(URL,[createKB([kb='http://owllink.org/examples/KB_3'],
					['Prefix'(name='test:',fullIRI='http://www.owllink.orgtest/ont'),
					'Prefix'(name='ont:',fullIRI='http://owllink.org/examples/ontology')]),
			       loadOntology('http://owllink.org/examples/KB_3',
					   [ontologyIRI('IRI'='http://www.owllink.org/examples/ontologies/myOntology'),
					    ontologyIRI('IRI'='http://www.owllink.org/examples/ontologies/myOntology2'),							    'IRIMapping'(key='http://www.owlllink.org/examples/ontologies/myOntology',										 'IRI'='localhost://examples/ontologies/myOntology')
					   ]),
			       tell('http://owllink.org/examples/KB_3',
				    [subClassOf('ont:ClassA','test:ClassA')])
			      ],Response6,
	     [reasoner,
	      request_file='../examples/owllink/thea-owllink-example-LoadOntology-request.xml',
	      response_file='../examples/owllink/owllink-example-LoadOntology-response-20091016.xml']),
    print('--------------------'),nl,print(Response6),nl,

    % OWLLink Example 7
    owl_link(URL,[createKB([kb='http://owllink.org/examples/KB_1'],[]),
			       tell('http://owllink.org/examples/KB_1',
				    [subClassOf('B','A'),
				     subClassOf('C','A'),
				     equivalentClasses(['D','E']),
				     classAssertion('A','iA'),
				     subClassOf('C','A')
				    ]),
			       getAllClasses('http://owllink.org/examples/KB_1'),
			       getEquivalentClasses('http://owllink.org/examples/KB_1','D'),
			       isClassSubsumedBy('http://owllink.org/examples/KB_1',
						 'http://www.w3.org/2002/07/owl#Thing',
						 'http://www.w3.org/2002/07/owl#Nothing'),
			       getSubClasses('http://owllink.org/examples/KB_1','C'),
			       createKB([kb='http://owllink.org/examples/KB_2'],[]),
			       tell('http://owllink.org/examples/KB_2',
				    [subClassOf('A','B')]),
			       releaseKB('http://owllink.org/examples/KB_1'),
			       getAllClasses('http://owllink.org/examples/KB_1')
			      ],Response7,
	     	     [reasoner,
		      request_file='../examples/owllink/thea-owllink-example-PoolingRequests-request.xml',
		      response_file='../examples/owllink/owllink-example-poolingrequests-response-20091016.xml']),
    print('--------------------'),nl,print(Response7),nl,

% OWLLink Example 8 - Taxonomy Request
%
%
    owl_link(URL,[createKB([kb='http://owllink.org/examples/KB_1'],[]),
			       getSubClassHierarchy('http://owllink.org/examples/KB_1',_),
			       createKB([kb='http://owllink.org/examples/KB_2'],[]),
			       tell('http://owllink.org/examples/KB_2',
				    [subClassOf('A','http://www.w3.org/2002/07/owl#Thing')
				    ]),
			       createKB([kb='http://owllink.org/examples/KB_3'],[]),
			       tell('http://owllink.org/examples/KB_3',
				    [subClassOf('A','http://www.w3.org/2002/07/owl#Thing'),
				     subClassOf('B','A'),
				     subClassOf('D','A')
				    ]),
			       createKB([kb='http://owllink.org/examples/KB_4'],[]),
			       tell('http://owllink.org/examples/KB_4',
				    [equivalentClasses(['A','http://www.w3.org/2002/07/owl#Thing']),
				     equivalentClasses(['C','http://www.w3.org/2002/07/owl#NoThing']),
				     subClassOf('B','http://www.w3.org/2002/07/owl#NoThing')
				    ]),
			       getSubClassHierarchy('http://owllink.org/examples/KB_2',_),
			       getSubClassHierarchy('http://owllink.org/examples/KB_3',_),
			       getSubClassHierarchy('http://owllink.org/examples/KB_4',_),

			       releaseKB('http://owllink.org/examples/KB_1'),
			       releaseKB('http://owllink.org/examples/KB_2'),
			       releaseKB('http://owllink.org/examples/KB_3'),
			       releaseKB('http://owllink.org/examples/KB_4')

			      ],Response8,
	     [reasoner,
	      request_file='../examples/owllink/thea-owllink-example-Taxonomy-request.xml',
	      response_file='../examples/owllink/owllink-example-taxonomy-response-20091016.xml']),
    print('--------------------'),nl,   print(Response8),nl.


test :-
	owl_link('http://localhost:8080',[createKB([kb='http://owllink.org/examples/KB_1'],[]),
			       tell('http://owllink.org/examples/KB_1',
				    [subClassOf('B','A'),
				     subClassOf('C','A'),
				     equivalentClasses(['D','E']),
				     % classAssertion('A','iA'),
				     subClassOf('C','A')
				    ]),
				tell('http://owllink.org/examples/KB_1',
				    [ classAssertion('A','iA')]),
			       getAllClasses('http://owllink.org/examples/KB_1'),
					  getSubClassHierarchy('http://owllink.org/examples/KB_1','A'),
			       getEquivalentClasses('http://owllink.org/examples/KB_1','D'),
			       isClassSubsumedBy('http://owllink.org/examples/KB_1',
						 'http://www.w3.org/2002/07/owl#Thing',
						 'http://www.w3.org/2002/07/owl#Nothing'),
			       getSubClasses('http://owllink.org/examples/KB_1','C'),
					  getAllIndividuals('http://owllink.org/examples/KB_1'),
			       createKB([kb='http://owllink.org/examples/KB_2'],[]),
			       tell('http://owllink.org/examples/KB_2',
				    [subClassOf('A','B')]),
			       releaseKB('http://owllink.org/examples/KB_1'),
			       getAllClasses('http://owllink.org/examples/KB_1')
			      ],Response7,
	     	     [reasoner,
		      request_file='../examples/owllink/thea-owllink-example-PoolingRequests-request.xml',
		      response_file='../examples/owllink/owllink-example-poolingrequests-response-20091016.xml']),
    print('--------------------'),nl,print(Response7),nl.


load_wine(Response) :-
	owl_link('http://localhost:8080',
		 [createKB([kb='http://owllink.org/examples/wine'],[]),
		  loadOntologies('http://owllink.org/examples/wine',
				 ['OntologyIRI'('IRI'='http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine')],[],false)
		 ],
		 Response,
		 [reasoner]).

release_wine(Response) :- owl_link('http://localhost:8080',[releaseKB('http://owlink.org/examples/wine')],Response,[reasoner]).

wine_classes(Response) :- owl_link('http://localhost:8080',[getAllClasses('http://owllink.org/examples/wine')],Response,[reasoner]).

wine_ask(Ask,Response) :-
	owl_link('http://localhost:8080',Ask,Response,[reasoner]).














