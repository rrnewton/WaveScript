
## The big problem is that schemedoc doesn't work with #;

FILES = \
  main.ss \
  regiment.ss \


GENERICFILES = \
  generic/alpha_lib.ss \
  generic/alpha_lib_scheduler_simple.ss \
  generic/constants.ss \
  generic/grammar_checker.ss \
  generic/hash.ss \
  generic/helpers.ss \
  generic/hm_type_inference.ss \
  generic/pass00_verify-regiment.ss \
  generic/pass09_lift-letrec.ss \
  generic/pass22_desugar-macros.ss \
  generic/pass23a_find-emittoks.ss \
  generic/pass23_desugar-gradients.ss \
  generic/pass23_desugar-gradients_verbose.ss \
  generic/pass23_desugar-gradients_simple.ss \
  generic/pass23_desugar-gradients_ETX.ss \
  generic/prim_defs.ss \
  generic/regiment_helpers.ss \
  generic/simulator_alpha_datatypes.ss \
  generic/simulator_alpha.ss \
  generic/source_loader.ss \
  generic/tml_generic_traverse.ss \


CHEZFILES = \
  chez/chez_constants.ss \
  chez/helpers.ss \
  chez/regmodule.ss \

PLTFILES = 

ANALYSIS_FILES = 
#  analysis/gradients/reception.ss
#  analysis/gradients/reception_rate_vs_link_quality_wwout_acks.ss

SRCFILES = $(FILES) $(GENERICFILES) $(CHEZFILES) $(PLTFILES) $(ANALYSIS_FILES)
DUPSRC   = $(SRCFILES:%=html/%)
OBJFILES = $(DUPSRC:%=%.html)

# Here's a bit of a trick, we just use regiment for the scheme system,
# and let it worry about finding the right scheme to call.
#SCHEME = chez
#SCHEME = scheme
SCHEME = regiment i
PETITE = ../depends/petite

#SCRATCHFILE=.__scratch_makefile_junk

all: schemedoc 
#       Here we superimpose our "patches" to the standard schemedoc
#       tree.  This isn't finished yet.  There are more files than
#       star_star_chez.scm that are modified in the schemedoc/ tree.
#       Right now this is just a lame way for me to avoid actually
#       changing the tbz and wasting space in the repository.
	if ! [ -d html ];         then mkdir html        ; fi
	if ! [ -d html/generic ]; then mkdir html/generic; fi
	if ! [ -d html/chez ];    then mkdir html/chez   ; fi
	if ! [ -d html/plt ];     then mkdir html/plt    ; fi
#	if ! [ -d html/analysis ];     then mkdir html/analysis ; fi
#	if ! [ -d html/analysis/gradients ];     then mkdir html/analysis/gradients ; fi
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
	$(SCHEME) --script regiment_index.laml $(SRCFILES)

schemedoc:
	tar xjvf schemedoc.tbz; 
	$(MAKE) patch

patch:
	cp patches/schemedoc-extractor.scm ./schemedoc/tools/schemedoc-extractor/

# Incorporated these patches into a new tarball, but there might be more in the future:
#	cp patches/star_star_chez.scm schemedoc/lib/compatibility/; 
#	cp patches/time.scm schemedoc/lib/; 
#	cp patches/elucidator.scm schemedoc/styles/xml-in-laml/elucidator-2/; 
#	cp patches/schemedoc-extractor.scm schemedoc/tools/schemedoc-extractor/; 



# This invokes regiment.laml with the directory and the filename as command line arguments:
$(OBJFILES): html/%.html: html/%
	$(SCHEME) --script regiment.laml $(REGIMENTD)/doc/`dirname $@`/  `echo $< | sed 's/.*\///'`
#`echo $(<:html/%.ss=%.ss) | sed 's/generic\///'`
#$(<:html/%=%)

$(DUPSRC): html/%.ss: ../src/%.ss
	$(PETITE) --script scrub_source.ss $< $@	


install: installreadme
	cp -vpr html /var/www/regimentman/
	chgrp www-data -R /var/www/regimentman/
	chmod g+rX -R /var/www/regimentman/*

installreadme:
	cp ../src/README /var/www/regimentman/
	cp ../src/README.html /var/www/regimentman/

clean: 
	$(RM) -rf html schemesource.tmp *~ .__scratch_makefile_junk
#       This is intense, we delete and rebuild this tree every time:
	$(RM) -rf schemedoc
##	$(RM) *.html *.css *.manlsp *~
