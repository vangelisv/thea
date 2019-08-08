
# Using Thea for Reasoning

An ontology specified in OWL is amenable to reasoning. Some of the
uses of reasoning are:

 * To check for inconsistencies in an ontology
 * To classify the ontology (i.e. find all implied subClassOf/2 relationships)
 * To classify instances using the ontology (i.e. find all implied classAssertuon/2 relationships)

Thea2 offers a number of different options for reasoning. These can be
broken down into the following categories:

 * Use of an external reasoning engine
 * Direct use of prolog engine

There are 2 main ways of accessing external reasoning engines - via
the Java OWLAPI (which requires the JPL bridge) or across a network
via OWLLink. A third way is via an external Logic Programming engine.

The alternative to using an external reasoning engine is to reason
using Prolog. At this time, this means SWI-Prolog, and
backward-chaining based reasoning.

## Use of a Java reasoning engine via JPL

  * Module: owl2_java_owlapi.pl

SWI-Prolog can interact with Java via the JPL library. The main OWL2
library for Java is the Manchester OWLAPI. One of the advantages of
the OWLAPI is that it provides a common reasoner API that allows for
seamless integration with powerful reasoners such as Pellet and
FaCT++. Thea2 has a bridge to the OWLAPI called owl2_java_owlapi. This
module exports predicates such as

  * reasoner_classify/1
  * reasoner_subClassOf/4

See the module documentation for more details

At this time the bridge is to v2 of the OWLAPI. At some point we will
switch to v3, but this switch should be seamless to Thea users.

The implementation here should be complete and comprehensive - you
should have access to the full power of these reasoners directly from
Thea2. However, this has only been tested on a subset of ontologies
which are of interest to the authors.

## Use of a reasoning engine over the network

  * Module: owl2_owllink.pl

This module implements the OWLLink standard. See http://www.owllink.org/

Currently this supports using Thea as a client to an external reasoner
running as an OWLLink server. This implementation is complete
In the future we may support using Thea as an OWLLink
server too.

[VV]: See module documentation and owl2_owllink.txt

Perhaps this should also export more finer grained ask/tell predicates?

## Reasoning using Prolog

There are two opposing approaches here:

  * Rewrite the ontology as a logic program
  * Use rules in conjunction with Thea predicates

The first approach is based on rules defined by Grosof, and will
rewrite axioms such as:

```
subClassOf(cat, mammal).
classAssertion(cat, mr_whiskers).
```

to programs such as:

```
mammal(X) :- cat(X).
cat(mr_whiskers).
```

See owl2_to_prolog_dlp.pl module documentation for details.

This is useful for reasoning over ABoxes (individual), but not TBoxes.

A tabled prolog is recommended here, to avoid non-termination.

The second approach can actually be further subdivided depending on
whether backward chaining or a tabled prolog is used.

If you are using a tabled prolog (e.g. Yap or XSB), you can use the
module owl2_reasoner_rules.pl which defines rules such as this:

```
subClassOf(X,Y) :- subClassOf(X,Z),subClassOf(Z,Y).
```

If you are using SWI-Prolog, you can use owl2_basic_reasoner.pl which
has backwards chaining rules. Caveat emptor: this module is
experimental, liable to change and could be non-terminating. See the
module docs for details.

Another implementation is provided in owl2_rl_rules.pl. The module
implements OWL RL Rules with simple forward and backward chaining engines
See more in module documentation

## TODO

Currently each of the modules described above provides their own
particular predicates. This isn't ideal for the application
programmer. 

We have a prototype of a common API for all reasoning engines, see
owl2_reasoner.pl (not yet functional).

This will be analagous to owl2_io.pl, which provides
load_axioms/2 and save_axioms/2 with different parsers/writers
providing hooks for these.

The predicates exported might include:

  * initialize_reasoner/3
  * reasoner_tell/1
  * reasoner_tell/2
  * reasoner_ask/2
  * reasoner_check_consistency/1

We are envisioning something whereby the application programmer can write:

```
load_axioms('myont.owl'),
initialize_reasoner(pellet,Opts,Reasoner),
reasoner_tell(Reasoner),
forall(reasoner_ask(Reasoner,subClassOf(X,Y)),
       writeln(X-Y)).
```

[VV] Querying an external reasoner via OWLLink with non-ground queries is not trivial.
OWLLink does not support variables in its Ask specification. One should write the logic
on how the system should handle free variables: 
E.g. write something like the following:

```
  reasoner_ask(owllink_external_reasoner(R),subClassOf(X,Y)) :-
	    var(X),nonvar(Y),
		owllink(R, getAllClasses(KB),classSet(CL),[]),
		member(X,CL),
		owllink(R, isClassSubsumedBy(KB,X,Y),response(true),[]).
		
```


### Modules

  * owl2_reasoner.pl
  * owl2_java_owlapi.pl
  * owl2_owllink.pl
  * owl2_to_prolog_dlp.pl
  * owl2_reasoning_rules.pl
  * owl2_rl_rules.pl
  * owl2_basic_reasoner.pl

--+++ Reasoner Cookbook

  * TODO


