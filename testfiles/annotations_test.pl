:- use_module('../owl2_model').
:- use_module('../owl2_io').
:- use_module('../owl2_from_rdf').
:- use_module('../owl2_export_rdf').

thea_test :-
   assert_axiom(class('aw:Animal')),
   assert_axiom(objectProperty('aw:preys_on')),
   assert_axiom(namedIndividual('aw:Lion')),
   assert_axiom(classAssertion('aw:Lion', 'aw:Animal')),
   assert_axiom(namedIndividual('aw:Zebra')),
   assert_axiom(classAssertion('aw:Zebra', 'aw:Animal')),
   assert_axiom(propertyAssertion('aw:preys_on', 'aw:Lion1',
'aw:Zebra1')),

   assert_axiom( annotation( propertyAssertion( 'aw:preys_on', 'aw:Lion1', 'aw:Zebra1' ), 'rdfs:comment', literal('Comment on the preys_on property between Lion and Zebra'))),

   save_axioms('ontology-test.owl', owl, [write_xml_base(true)]),
   retract_axiom(_),
   retract_all_axioms.
