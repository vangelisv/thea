
%  test code for owllink
%  VV 25/9/09
%  

:- use_module('../owl2_util.pl').
:- use_module('../owl2_model.pl').
:- use_module('../owl2_from_rdf.pl').
:- use_module('../owl2_owllink.pl').
:-use_module(library('http/http_client')).




main(URL) :-

    % OWLLink Example 1
    owl_link(URL,[getDescription],Response1,
	     [reasoner,
	      request_file='../examples/owllink/thea-owllink-example-GetDescription-request.xml',
	      response_file='../examples/owllink/owllink-example-GetDescription-response-20091016.xml']),
    print(Response1),nl,
    
    % OWLLink Example 2
    owl_link(URL,[createKB([kb='http://owllink.org/examples/KB_1'],[]),
		  tell('http://owllink.org/examples/KB_1',
		       [subClassOf('A','B'),
			subClassOf('B','C')]),
		  isClassSatisfiable('http://owllink.org/examples/KB_1','A')
		 ],Response2,
	     [reasoner,
	      request_file='../examples/owllink/thea-owllink-example-CreateKB-request.xml',
	      response_file='../examples/owllink/owllink-example-CreateKB-response-20091016.xml']),
    print(Response2),nl,

    % OWLLink Example 3      
   
    owl_link(URL,[getSettings('http://owllink.org/examples/KB_1')],Response3,
	     [reasoner,
	      request_file='../examples/owllink/thea-owllink-example-RetrieveSettings-request.xml',
	      response_file='../examples/owllink/owllink-example-GetSettings-response-20091016.xml'
	     ]),
    print(Response3),nl,
   

    % OWLLink Example 4      
    owl_link(URL,[set('http://owllink.org/examples/KB_1','selectedProfile',['OWL DL'])],Response4,
	     [reasoner,
	      request_file='../examples/owllink/thea-owllink-example-SetSettings-request.xml',
	      response_file='../examples/owllink/owllink-example-Set-response-20091016.xml']),
    print(Response4),nl,
  
    % OWLLink Example 5     
    owl_link(URL,[createKB([kb='http://owllink.org/examples/KB_2'],
					['Prefix'(name='test:',fullIRI='http://www.owllink.orgtest/ont'),
					'Prefix'(name='ont:',fullIRI='http://owllink.org/examples/ontology')]),
			       tell('http://owllink.org/examples/KB_2',
				    [subClassOf('ont:ClassA','test:ClassA')])		       			       
			      ],Response5,
	     [reasoner,
	      request_file='../examples/owllink/thea-owllink-example-Prefixes-request.xml',
	      response_file='../examples/owllink/owllink-example-Prefix-response-20091016.xml']),
    print(Response5),nl,
    
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
    print(Response6),nl,
    
    % OWLLink Example 7
    owl_link(URL,[createKB([kb='http://owllink.org/examples/KB_4'],[]),
			       tell('http://owllink.org/examples/KB_4',
				    [subClassOf('B','A'),
				     subClassOf('C','A'),
				     equivalentClasses(['D','E']),
				     classAssertion('A','iA')				     
				    ]),
			       getAllClasses('http://owllink.org/examples/KB_4'),
			       getEquivalentClasses('http://owllink.org/examples/KB_4','D'),
			       isClassSubsumedBy('http://owllink.org/examples/KB_4',
						 'http://www.w3.org/2002/07/owl#Thing',
						 'http://www.w3.org/2002/07/owl#Nothing'),
			       getSubClasses('http://owllink.org/examples/KB_4','C'),
			       createKB([kb='http://owllink.org/examples/KB_5'],[]),
			       tell('http://owllink.org/examples/KB_5',
				    [subClassOf('A','B')]),
			       releaseKB('http://owllink.org/examples/KB_4'),
			       getAllClasses('http://owllink.org/examples/KB_4')
			      ],Response7,
	     	     [reasoner,
		      request_file='../examples/owllink/thea-owllink-example-PoolingRequests-request.xml',
		      response_file='../examples/owllink/owllink-example-poolingrequests-response-20091016.xml']),
    print(Response7),nl,

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
    print(Response8),nl.







