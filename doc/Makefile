
## The big problem is that schemedoc doesn't work with #;

FILES = \
  compiler.ss \
  regiment.ss \


GENERICFILES = \
  generic/helpers.ss \
  generic/hash.ss \
  generic/alpha_lib.ss \
  generic/alpha_lib_scheduler_simple.ss \
  generic/simulator_alpha_datatypes.ss \
  generic/simulator_alpha.ss \
  generic/constants.ss \

CHEZFILES = \
  chez/helpers.ss \
  chez/constants.ss \

PLTFILES = 


SRCFILES = $(FILES) $(GENERICFILES) $(CHEZFILES) $(PLTFILES)
DUPSRC   = $(SRCFILES:%=html/%)
OBJFILES = $(DUPSRC:%=%.html)


#SCRATCHFILE=.__scratch_makefile_junk

all:

#       Here we superimpose our "patches" to the standard schemedoc
#       tree.  This isn't finished yet.  There are more files than
#       star_star_chez.scm that are modified in the schemedoc/ tree.
#       Right now this is just a lame way for me to avoid actually
#       changing the tbz and wasting space in the repository.
	if ! [ -d schemedoc ]; \
	then tar xjvf schemedoc.tbz; \
	     cp patches/star_star_chez.scm schemedoc/lib/compatibility/; \
	     cp patches/time.scm schemedoc/lib/; \
	     cp patches/elucidator.scm schemedoc/styles/xml-in-laml/elucidator-2/; \
	     cp patches/schemedoc-extractor.scm schemedoc/tools/schemedoc-extractor/; \
	fi
	if ! [ -d html ];         then mkdir html        ; fi
	if ! [ -d html/generic ]; then mkdir html/generic; fi
	if ! [ -d html/chez ];    then mkdir html/chez   ; fi
	if ! [ -d html/plt ];     then mkdir html/plt    ; fi
	@echo
	@echo First scrub source:
	@echo -------------------
	$(MAKE) $(DUPSRC)
	@echo
	@echo Second build individual manuals:
	@echo --------------------------------
	$(MAKE) $(OBJFILES)
##	echo $(FILES) | xargs -n1 echo | xargs -i "sed 's/#;/\`/' $(REGIMENTD)/{} > ./{}"
##	echo $(FILES) | xargs -n1 echo | xargs -i ./clean_hashsemi $(REGIMENTD)/src/{} ./html/{}
#	echo $(FILES) | xargs -n1 echo | xargs -i ./scrub_source.ss $(REGIMENTD)/src/{} ./html/{}
#	echo $(GENERICFILES) | xargs -n1 echo | xargs -i ./scrub_source.ss $(REGIMENTD)/src/{} ./html/{}

#	echo $(FILES) | xargs -n1 chez --script regiment.laml $(REGIMENTD)/doc/html/
#	echo $(GENERICFILES) | sed 's/generic\///g' | xargs -n1 chez --script regiment.laml $(REGIMENTD)/doc/html/generic/

	@echo "------------------------------------------------------------------------------"
	@echo "Done generating Regiment documentation."

##      Now generate index:
##	echo $(GENERICFILES) | xargs -n1 | xargs -i echo "generic/"{} > $(SCRATCHFILE)
##	echo $(CHEZFILES) | xargs -n1 | xargs -i echo "generic/"{} > $(SCRATCHFILE)
	chez --script regiment_index.laml $(SRCFILES)

$(OBJFILES): html/%.html: html/%
	chez --script regiment.laml $(REGIMENTD)/doc/`dirname $@`/  `echo $< | sed 's/.*\///'`
#`echo $(<:html/%.ss=%.ss) | sed 's/generic\///'`
#$(<:html/%=%)

$(DUPSRC): html/%.ss: ../src/%.ss
	petite --script scrub_source.ss $< $@	


install:
	cp -vpr html /var/www/regimentman/
	cp ../src/README /var/www/regimentman/
	cp ../src/README.html /var/www/regimentman/
	chgrp www-data -R /var/www/regimentman/
	chmod g+rX -R /var/www/regimentman/

clean: 
	$(RM) -rf html schemesource.tmp *~ .__scratch_makefile_junk
#       This is intense, we delete and rebuild this tree every time:
	$(RM) -rf schemedoc
##	$(RM) *.html *.css *.manlsp *~
