PLCOMP = java -cp $(PLCAFEDIR)/plcafe.jar jp.ac.kobe_u.cs.prolog.compiler.Compiler

%.pl: ../%.pl
	cd .. && bin/pl-listing $* > iso/$@

thea_pcafe.pl: compat.pl
#	cat compat.pl owl2_model.pl owl2_io.pl owl2_metamodel.pl > $@
	cat compat.pl owl2_model.pl > $@

plcomp: thea_pcafe.pl
	$(PLCOMP) $<

classes:
	javac -d . -cp $(PLCAFEDIR)/plcafe.jar:$(PLCAFEDIR)/builtin.jar *.java