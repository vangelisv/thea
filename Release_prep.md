# Proposal: Bundle Thea2 core with SWI Prolog (and eventually Yap)

We propose that Thea2 is bundled with SWI-Prolog. This document
describes some necessary preparation, such that the bundling is
seamless, and that changes can be kept in sync.

At this time the Thea github repository contains a number of modules
and applications that are not yet fully mature and not required for
core functionality. We will probably split some of these out.

The core functionality delivered by Thea is the ability to read an OWL
file serialized as RDF/XML and allow the user to query the ontology
using the predicates defined in owl2_model.pl

For example:

```
use_module(library(thea2/owl_io)).
use_module(library(thea2/owl_model)).

load_axioms('testfiles/pizza.owl').
forall(subClassOf(A,B),
       format('~w subClassOf ~w',[A,b])).
```

This split will involve some change in the current github
organization. One solution is to have 3 github projects:

* thea2_core
* thea2_contrib
* thea2

The final one would be an empty project that uses git submodules to
provide the full functionality in one project.

SWI would have the option of only including thea2_core - or of
bundling selected contributed modules.

# Module split

As of 2010/07/01

The following should be in the core:

* owl2_model - mature, core
* owl2_metamodel - mature, required for core
* owl2_io - mature, core
* owl2_from_rdf - mature (still minor bugs?), core
* owl2_export_to_rdf - mature (still minor bugs?), core
* swrl
* bin/thea - mature
 * handy command line wrapper

The following are optionally in core:

* owl2_catalogs - for registering local paths for owl:imports
 * make this something more generic in SWI?
* owl2_plsyn - mature. provides infix syntax. not essential, but nice
* owl2_xml - not yet well tested?
* owl2_owllink - mature?
 * requires: owl2_xml
* owl2_popl - for processing OWL files
 * requires: owl2_plsyn
* owl2_manchester_parser.pl - not yet mature enough for core?
* owl2_java_owlapi
 * requires JPL
 * bridge to java OWLAPI
 * currently a good way of running reasoners such as pellet, hermit 

The following would be in extras/contrib:

* owl2_util
 * mostly a random-grab bag, some useful stuff
 * many of the predicates could be refactored into other modules or documentation/examples

* apps/
  
# Directory organization

It would be good to have a set of recommendations and best practices
for developing modules intended to be part of SWI, or intended to be
used by others.

* thea is currently managed in git
* all the main modules are in the top-level directory
* full plunit test files are provided (suffix .plt, top-level directory)
* a Makefile.in and default configuration file is provided (what about Windows?)
* all documentation is in pldoc format
 * no entry-point latex .doc file is provided yet

# Open questions

should the project / top level dir be called "thea" or "thea2"?

Much of the code already uses "use_module(library(thea2/<MOD>))". I
think it is good to sync the project name with the version of OWL. If
there is ever an OWL3 then thea3 would be a not necessarily backward
compatible library.

The git project is currently called "thea" but maybe if we re-organize
(see above) it could explicitly be thea2?

# TODO

Make it easier to run the tests

Add a few more plunit tests - anything including must be thoroughly
tested

rationalize documentation a bit

