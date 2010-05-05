owl_parse(URL, RDF_Load_Mode, OWL_Parse_Mode,ImportFlag) :-
	> If RDF_Load_Mode=complete Then retract all rdf/3, rdf/4, rdf_source/4
	> If OWL_Parse_Mode=complete Then 
		owl_clear_as,
		retractall(blanknode(_,_,_)), retractall(owl(_,_,_,_))
      End if  
        
	owl_canonical_parse_2([URL],URL,ImportFlag,[],ProcessedIRIs),
		> 
	owl2_model_init,
	owl_canonical_parse_3(ProcessedIRIs).
	
	
	