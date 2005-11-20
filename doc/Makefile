
## The big problem is that schemedoc doesn't work with #;

FILES = \
  compiler.ss \
  regiment.ss \

GENERICFILES = \
  hash.ss \
  alpha_lib.ss \
  alpha_lib_scheduler_simple.ss \
  simulator_alpha_datatypes.ss \
  simulator_alpha.ss \



CHEZFILES = 

PLTFILES = 

all:
	if ! [ -d html ];         then mkdir html        ; fi
	if ! [ -d html/generic ]; then mkdir html/generic; fi
	if ! [ -d html/chez ];    then mkdir html/chez   ; fi
	if ! [ -d html/plt ];     then mkdir html/plt    ; fi
#	echo $(FILES) | xargs -n1 echo | xargs -i "sed 's/#;/\`/' $(REGIMENTD)/{} > ./{}"
#	echo $(FILES) | xargs -n1 echo | xargs -i ./clean_hashsemi $(REGIMENTD)/src/{} ./html/{}
	echo $(FILES) | xargs -n1 echo | xargs -i ./scrub_source.ss $(REGIMENTD)/src/{} ./html/{}
	echo $(GENERICFILES) | xargs -n1 echo | xargs -i ./scrub_source.ss $(REGIMENTD)/src/generic/{} ./html/generic/{}

	echo $(FILES) | xargs -n1 chez --script regiment.laml $(REGIMENTD)/doc/html/
	echo $(GENERICFILES) | xargs -n1 chez --script regiment.laml $(REGIMENTD)/doc/html/generic/

	@echo "------------------------------------------------------------------------------"
	@echo "Done generating Regiment documentation."

#       Now generate index:
	chez --script regiment_index.laml


clean: 
	$(RM) -rf html schemesource.tmp *~ 
#	$(RM) *.html *.css *.manlsp *~
