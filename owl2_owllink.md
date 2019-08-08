%TOC%

# Thea OWLLink interface

The OWLlink protocol specification (<http://www.owlink.org>) is an implementation-neutral 
mechanism for communication between OWL 2 components. Thea's OWLLink interface module 
enables a Prolog program to act as a client to an external OWL reasoner that supports the OWLLink interface. 
The interface is implemented via the owl_link/4 predicate:

 * [[owl_link/4]]



## Examples

See bin/thea_owllink.pl

## Requests Specification 
```
RequestTerm ::= 
		ManagementRequest | ReasonerInvocationRequest | GeneralEntailmentRequest | RetrieveingKBEntitiesRequest |
		ClassAsksRequests | ClassQueriesRequests | ClassHierarchyRequests | IndividualClassQuerysynsetsRequests | 
		IndividualPropertyQueriesRequests | IndividualDataPropertyQueriesRequests | IndividualIndividualQueriesRequests | 
		IndividualIndividualQueriesFlattenRequests | IndividualIndividualDataQueriesSynsetsRequests | 
		IndividualIndividualDataQueriesFlattenRequests | ObjectPropQueriesRequests | ObjectPropHierarchyRequests
		ObjectPropAsksRequests | DataPropAsksRequests | DataPropQueriesRequests | DataPropHierarchyRequests |
		tell(KB,Axioms)
		
ManagementRequest ::= 
		getDescription | getSettings(KB) | getPrefixes(KB) | createKB(KB_Name_Attrs,Prefixes) | releaseKB(KB) |
		set(KB,Key,Settings) | isKBSatisfiable(KB) | isKBStructurallyConsistent(KB) | isTBoxConsistent(KB) |
		loadOntologies(KB,IRIs,IRIMappings,Imports)
						
ReasonerInvocationRequest ::=  
		classify(KB) | realize(KB)

GeneralEntailmentRequest ::= 	
		isEntailed(KB,Axioms,Options) | isEntailedDirect(KB,Axioms,Options) 

RetrieveingKBEntitiesRequest ::= 
		getAllAnnotationProperties(KB) | getAllObjectProperties(KB) | getAllDatatypes(KB) |
		getAllIndividuals(KB) |  etAllDataProperties(KB) | getAllClasses(KB)

ClassAsksRequests ::= 
		isClassSatisfiable(KB,Class) | isClassSubsumedBy(KB,Class1,Class2) | areClassesDisjoint(KB,Classes) | reClassesEquivalent(KB,Classes)

ClassQueriesRequests ::= 
		getSubClasses(KB,Class) | 	getSubClasses(KB,Class,Direct) | etSuperClasses(KB,Class) | 
		getSuperClasses(KB,Class,Direct) | getDisjointClasses(KB,Class),

ClassHierarchyRequests ::= 
		getEquivalentClasses(KB,Class) | getSubClassHierarchy(KB,Class)

IndividuualClassQuerysynsetsRequests ::=
		getTypes(KB,Individual,Direct) | getFlattenedTypes(KB,Individual,Direct) | getSameIndividuals(KB,Individual,Direct) |
		getDifferentIndividuals(KB,Individual,Direct) | getFlattenedDifferentIndividuals(KB,Individual,Direct) | 		 
		getEquivalentIndividuals(KB,Individual) | getDisjointIndividuals(KB,Individual) | getFlattenDisjointIndividuals(KB,Individual)
		
IndividualPropertyQueriesRequests ::=
		getObjectPropertiesOfSource(KB,Individual,Negative) | getObjectPropertiesBetween(KB,I1,I2,Negative) | 
		getObjectPropertiesOfTarget(KB,Individual,Negative)

IndividualDataPropertyQueriesRequests ::=
		getDataPropertiesOfSource(KB,Individual,Negative) | getDataPropertiesBetween(KB,I1,Literal,Negative) |
		getDataPropertiesOfLiteral(KB,Literal,Negative)

IndividualIndividualQueriesRequests ::= 
		getInstances(KB,Class,Direct) | getObjectPropertyTargets(KB,ObjectProperty,Individual,Negative) | 
		getObjectPropertySources(KB,ObjectProperty,Individual,Negative)

IndividualIndividualQueriesFlattenRequests ::= 
		getFlattenedInstances(KB,Class,Direct) | getFlattenedObjectPropertyTargets(KB,ObjectProperty,Individual,Negative) | 
		getFlattenedObjectPropertySources(KB,ObjectProperty,Individual,Negative)

IndividualIndividualDataQueriesSynsetsRequests ::= 
		getDataPropertyTargets(KB,DataProperty,Individual,Negative) | getDataPropertySources(KB,DataProperty,Literal,Negative)

IndividualIndividualDataQueriesFlattenRequests ::=  getFlattenedDataPropertySources(KB,ObjectProperty,Literal,Negative)

ObjectPropQueriesRequests ::= 
		getSubObjectProperties(KB,ObjectProperty,Direct) | getSuperObjectProperties(KB,ObjectProperty,Direct) | 		 
		getEquivalentObjectProperties(KB,ObjectProperty,Direct) | getDisjointObjectProperties(KB,ObjectProperty,Direct)
		 
ObjectPropHierarchyRequests ::= getSubObjectPropertyHierarchy(KB,ObjectProperty)
		 
ObjectPropAsksRequests ::= isObjectPropertySatisfiable(KB,ObjectProperty)
		 
DataPropAsksRequests ::= isDataPropertySatisfiable(KB,DataProperty)
		 
DataPropQueriesRequests ::= 
		getSubDataProperties(KB,DataProperty,Direct) | getSuperDataProperties(KB,DataProperty,Direct) | 
		getEquivalentDataProperties(KB,DataProperty,Direct) | getDisjointDataProperties(KB,DataProperty,Direct)
		 
DataPropHierarchyRequests ::= 
		getSubDataPropertyHierarchy(KB,DataProperty)
		 
KB			::= fullIRI		 
Direct 		::= true | false
Negative 	::= true | false
Imports  	::= true | false
Options  	::= [ Attr* ]
IRIs     	::= [ 'OntologyIRI' ('IRI' = IRI ) ]
IRIMappings 	::= [ 'IRIMapping' (Key = Value) ]		 
Attr 		::= Name=Value		 

All other Terms (Axiom, Individual, DataProperty etc.) are the perspective OWL2 Terms
```

## Responses Specification


  * TODO


