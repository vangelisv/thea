/** <module> Test code for OWLLink interface

This module processes structured comments and generates both formal
mode declarations from them as well as documentation in the form of
HTML or LaTeX.
  test code for owllink
  VV 25/9/09


@author Vangelis Vassiliadis
@license GPL

*/


:- use_module('../owl2_util.pl').
:- use_module('../owl2_model.pl').
:- use_module('../owl2_from_rdf.pl').
:- use_module('../owl2_owllink.pl').
:-use_module(library('http/http_client')).




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
/*
       [kb(http://owllink.org/examples/KB_4, []),
	syntaxError(Ignored non-valid OWLlink Tell requests: ((ClassAssertion                                            (Class A)                                            (Class iA)))),
	setOfClasses([], [owl:Thing, C, B, E, A, D]), setOfClasses([], [E, D]),
	booleanResponse(false, []), element(SetOfClassSynsets, [], []),
	kbError(KB http://owllink.org/examples/KB_5 already exists, request denied),
	ok([]),
	ok([]),
	kbError(KB http://owllink.org/examples/KB_4 not found. Request denied)]
    */
    % OWLLink Example 8 - Taxonomy Request
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



/*



------------------------------
[kb(http://owllink.org/examples/KB_1, []),
 ok([]),
 booleanResponse(true, []), ok([])
]
--------------------
[kb(http://owllink.org/examples/KB_1, []),
 settings([], [setting(uniqueNameAssumption, xsd:boolean, false),
	       setting(verbose, xsd:boolean, false),
	       setting(leanMode, xsd:boolean, false),
	       setting(keepsAxioms, xsd:boolean, true),
	       setting(usesLessMemory, xsd:boolean, false),
	       setting(ignoresAnnotations, xsd:boolean, false),
	       setting(ignoresDeclarations, xsd:boolean, false),
	       setting(incremental, xsd:boolean, true),
	       setting(abbreviatesIRIs, xsd:boolean, true)]),
 ok([])
]
--------------------
[kb(http://owllink.org/examples/KB_1, []),
 ok([]),
 settings([], [setting(uniqueNameAssumption, http://www.w3.org/2001/XMLSchema#boolean, false),
	       setting(verbose, http://www.w3.org/2001/XMLSchema#boolean, false),
	       setting(leanMode, http://www.w3.org/2001/XMLSchema#boolean, false),
	       setting(keepsAxioms, http://www.w3.org/2001/XMLSchema#boolean, true),
	       setting(usesLessMemory, http://www.w3.org/2001/XMLSchema#boolean, false),
	       setting(ignoresAnnotations, http://www.w3.org/2001/XMLSchema#boolean, false),
	       setting(ignoresDeclarations, http://www.w3.org/2001/XMLSchema#boolean, false),
	       setting(incremental, http://www.w3.org/2001/XMLSchema#boolean, true),
	       setting(abbreviatesIRIs, http://www.w3.org/2001/XMLSchema#boolean, false)]),
 ok([])
]
--------------------
[kb(http://owllink.org/examples/KB_2, []),
 ok([]),
 ok([]),
 semanticError(Reasoning error 'Undefined concept name                  http://owllink.org/examples/ontology#ClassA in TBox                  http://owllink.org/examples/KB_2' occured),
 element(SetOfClassSynsets, [], [element(ClassSynset, [], [element(owl:Class, [IRI=http://www.owllink.orgtest/ontClassA], [])])]), ok([]),
 semanticError(Reasoning error 'Undefined concept name                  http://owllink.org/examples/ontology#ClassA in TBox                  http://owllink.org/examples/KB_2' occured),
 ok([])]
--------------------
[kb(http://owllink.org/examples/KB_3, []),
 syntaxError(No valid OWLlink KBRequest request: LoadOntology),
 ok([])]
--------------------
[kb(http://owllink.org/examples/KB_1, []),
 syntaxError(Ignored non-valid OWLlink Tell requests: ((ClassAssertion                                            (Class A)                                            (Class iA)))),
 setOfClasses([], [owl:Thing, C, B, E, A, D]),
 setOfClasses([], [E, D]),
 booleanResponse(false, []),
 element(SetOfClassSynsets, [], []), kb(http://owllink.org/examples/KB_2, []),
 ok([]),
 ok([]),
 kbError(KB http://owllink.org/examples/KB_1 not found. Request denied)
]
--------------------
[kb(http://owllink.org/examples/KB_1, []),
 classHierarchy([], [classSubClassesPair(synset([owl:Thing]), [])]), kbError(KB http://owllink.org/examples/KB_2 already exists, request denied),
 ok([]),
 kbError(KB http://owllink.org/examples/KB_3 already exists, request denied),
 ok([]),
 kb(http://owllink.org/examples/KB_4, []),
 ok([]),
 classHierarchy([], [classSubClassesPair(synset([owl:Thing]), []), classSubClassesPair(synset([B]), [])]),
 classHierarchy([], [classSubClassesPair(synset([owl:Thing]), []), classSubClassesPair(synset([http://www.owllink.orgtest/ontClassA]), [])]),
 classHierarchy([], [classSubClassesPair(synset([owl:Thing, A]), []), classSubClassesPair(synset([owl:NoThing, C]), [])]),
 ok([]),
 ok([]),
 ok([]),
 ok([])]

*/


















