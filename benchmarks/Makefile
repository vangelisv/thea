ONTS1 = CL CARO SO FBbt GO
ONTS2 = obi ido
ONTS3 = wine pizza Hydrology country NCIThesaurus

THEA_TARGETS = $(patsubst %,bench1-%.out,$(ONTS1)) $(patsubst %,bench2-%.out,$(ONTS2)) $(patsubst %,bench3-%.out,$(ONTS3))

OWLAPI_TARGETS = $(patsubst %,owlapi1-%.out,$(ONTS1)) $(patsubst %,owlapi2-%.out,$(ONTS2)) $(patsubst %,owlapi3-%.out,$(ONTS3))

all: $(THEA_TARGETS) $(OWLAPI_TARGETS)

show:
	echo $(TARGETS)

#old
dn1-%.owl:
	wget http://purl.org/obo/owl/$* -O $@
.PRECIOUS: dn1-%.owl

#new
dn2-%.owl:
	wget http://purl.obolibrary.org/obo/$*.owl -O $@
.PRECIOUS: dn2-%.owl

# old-style OBO URLs
bench1-%.out: dn1-%.owl
	../bin/thea-owl-i --format owl --debug bench --import --statistics $< > $@ 2>&1

# new-style OBO URLs
bench2-%.out: dn2-%.owl
	../bin/thea-owl-i --format owl --debug bench --import --statistics $< > $@ 2>&1

# smaller owl files that are distributed in the thea directory
bench3-%.out: 
	../bin/thea-owl-i --format owl --debug bench --import --statistics ../testfiles/$*.owl > $@ 2>&1

owlapi1-%.out: dn1-%.owl
	(time owlreasoner --no-reasoner file:$<) > $@ 2>&1 || echo ok

owlapi2-%.out: dn2-%.owl
	(time owlreasoner --no-reasoner file:$<) > $@ 2>&1 || echo ok

owlapi3-%.out:
	(time owlreasoner --no-reasoner file:../testfiles/$*.owl) > $@ 2>&1 || echo ok

