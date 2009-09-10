ONTS1 = CL CARO SO FBbt GO
ONTS2 = obi ido
ONTS3 = wine pizza Hydrology country

TARGETS = $(patsubst %,bench1-%.out,$(ONTS1)) $(patsubst %,bench2-%.out,$(ONTS2)) $(patsubst %,bench3-%.out,$(ONTS3))

all: $(TARGETS)

show:
	echo $(TARGETS)

# old-style OBO URLs
bench1-%.out:
	../bin/thea-owl-i --format owl --debug bench --import --statistics http://purl.org/obo/owl/$* > $@ 2>&1

# new-style OBO URLs
bench2-%.out:
	../bin/thea-owl-i --format owl --debug bench --import --statistics http://purl.obolibrary.org/obo/$*.owl > $@ 2>&1

# smaller owl files that are distributed in the thea directory
bench3-%.out:
	../bin/thea-owl-i --format owl --debug bench --import --statistics ../testfiles/$*.owl > $@ 2>&1

