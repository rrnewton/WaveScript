
bc:
	(cd ../; export PLTCOLLECTS=`pwd`:$(PLTLIB); mzc --collection-zos ./plt)

nc:
	(cd ../; export PLTCOLLECTS=`pwd`:$(PLTLIB); mzc --collection-extension ./plt)

clean: clean_byte clean_native
	rm -f _SIM_*

clean_byte:
	rm -f compiled/*.zo  compiled/*.dep

clean_native:
	(cd compiled/native; $(MAKE) clean)

#	rm -rf compiled/native/*
