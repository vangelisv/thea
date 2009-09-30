
%  test code for owllink
%  VV 25/9/09
%  

:- use_module('../owl2_util.pl').
:- use_module('../owl2_model.pl').
:- use_module('../owl2_from_rdf.pl').
:- use_module('../owl2_owllink.pl').



main :-

    % OWLLink Example 1
    owl_link('localhost:8082',[getDescription],_,[request_file='thea-owllink-example-GetDescription-request.xml']),

    % OWLLink Example 2
    owl_link('localhost:8082',[createKB([kb='http://owllink.org/examples/KB_1'],[]),
			       tell('http://owllink.org/examples/KB_1',
				    [subClassOf('A','B'),
				    subClassOf('B','C')]),
			       isClassSatisfiable('http://owllink.org/examples/KB_1','A')
			      ],_,[request_file='thea-owllink-example-CreateKB-request.xml']),


    % OWLLink Example 3      
    owl_link('localhost:8082',[getSettings('http://owllink.org/examples/KB_1')],_,
	     [request_file='thea-owllink-example-RetrieveSettings-request.xml']),

    % OWLLink Example 4      
    owl_link('localhost:8082',[set('http://owllink.org/examples/KB_1','selectedProfile',['OWL DL'])],_,
	     [request_file='thea-owllink-example-SettSettings-request.xml']),

    % OWLLink Example 5     
    owl_link('localhost:8082',[createKB([kb='http://owllink.org/examples/KB_2'],
					['Prefix'(name='test:',fullIRI='http://www.owllink.orgtest/ont'),
					'Prefix'(name='ont:',fullIRI='http://owllink.org/examples/ontology')]),
			       tell('http://owllink.org/examples/KB_2',
				    [subClassOf('ont:ClassA','test:ClassA')])		       			       
			      ],_,
	     [request_file='thea-owllink-example-Prefixes-request.xml']),

    % OWLLink Example 6     
    owl_link('localhost:8082',[createKB([kb='http://owllink.org/examples/KB_3'],
					['Prefix'(name='test:',fullIRI='http://www.owllink.orgtest/ont'),
					'Prefix'(name='ont:',fullIRI='http://owllink.org/examples/ontology')]),
			       loadOntology('http://owllink.org/examples/KB_3',
					   [ontologyIRI('IRI'='http://www.owllink.org/examples/ontologies/myOntology'),
					    ontologyIRI('IRI'='http://www.owllink.org/examples/ontologies/myOntology2'),							    'IRIMapping'(key='http://www.owlllink.org/examples/ontologies/myOntology',										 'IRI'='localhost://examples/ontologies/myOntology')
					   ]),
			       tell('http://owllink.org/examples/KB_3',
				    [subClassOf('ont:ClassA','test:ClassA')])		       			       
			      ],_,
	     [request_file='thea-owllink-example-LoadOntology-request.xml']),
			      
    % OWLLink Example 7
    owl_link('localhost:8082',[createKB([kb='http://owllink.org/examples/KB_4'],[]),
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
			      ],_,
	     	     [request_file='thea-owllink-example-PoolingRequests-request.xml']),
		      

    % OWLLink Example 8 - Taxonomy Request
    owl_link('localhost:8082',[createKB([kb='http://owllink.org/examples/KB_1'],[]),
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

			      ],_,
	     [request_file='thea-owllink-example-Taxonomy-request.xml']).












