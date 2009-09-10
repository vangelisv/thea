ONTS1 = CL CARO
ONTS2 = obi ido

TARGETS = $(patsubst %,bench1-%.out,$(ONTS1)) $(patsubst %,bench2-%.out,$(ONTS2))

all: $(TARGETS)

show:
	echo $(TARGETS)

bench1-%.out:
	../bin/thea-owl-i --format owl --debug bench --import --statistics http://purl.org/obo/owl/$* > $@ 2>&1

bench2-%.out:
	../bin/thea-owl-i --format owl --debug bench --import --statistics http://purl.obolibrary.org/obo/$*.owl > $@ 2>&1

