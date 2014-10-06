
## The big problem is that schemedoc doesn't work with #;

FILES = 
#   main.ss \
#   main.ss \
#   regiment.ss \


GENERICFILES = \
   generic/compiler_components/regiment_helpers.ss \
   generic/passes/wavescope_bkend/nominalize-types.ss \
   generic/compiler_components/prim_defs.ss \
   generic/compiler_components/type_environments.ss \
   generic/passes/ocaml_bkend/shared-emit-ml.ss \

# broken: [2007.07.01]
#   generic/sim/wavescript_sim_library_push.ss \
#   generic/passes/wavescope_bkend/emit-c.ss \
#   generic/passes/ocaml_bkend/emit-caml.ss \



#   generic/compiler_components/hm_type_inference.ss \

#   generic/alpha_lib.ss \
#   generic/alpha_lib_scheduler_simple.ss \
#   generic/constants.ss \
#   generic/grammar_checker.ss \
#   generic/hash.ss \
#   generic/helpers.ss \
#   generic/hm_type_inference.ss \
#   generic/pass00_verify-regiment.ss \
#   generic/pass09_lift-letrec.ss \
#   generic/pass22_desugar-macros.ss \
#   generic/pass23a_find-emittoks.ss \
#   generic/pass23_desugar-gradients.ss \
#   generic/pass23_desugar-gradients_verbose.ss \
#   generic/pass23_desugar-gradients_simple.ss \
#   generic/pass23_desugar-gradients_ETX.ss \
#   generic/regiment_helpers.ss \
#   generic/simulator_alpha_datatypes.ss \
#   generic/simulator_alpha.ss \
#   generic/source_loader.ss \
#   generic/tml_generic_traverse.ss \


CHEZFILES = 
#   chez/chez_constants.ss \
#   chez/helpers.ss \
#   chez/regmodule.ss \

PLTFILES = 

ANALYSIS_FILES = 
#  analysis/gradients/reception.ss
#  analysis/gradients/reception_rate_vs_link_quality_wwout_acks.ss

SRCFILES = $(FILES) $(GENERICFILES) $(CHEZFILES) $(PLTFILES) $(ANALYSIS_FILES)
SRCCOPY   = $(SRCFILES:%=html/%)
SRCTEMP   = $(SRCFILES:%=html/%.tmp)
HTMLFILES = $(SRCCOPY:%.ss=%.html)

# Here's a bit of a trick, we just use regiment for the scheme system,
# and let it worry about finding the right scheme to call.
SCHEME = chez
#SCHEME = scheme $(WAVESCRIPTD)/src/chez/match.ss
#SCHEME = regiment i
#SCHEME = scheme -b ./match.ss
PETITE = ../depends/petite

# #SCRATCHFILE=.__scratch_makefile_junk

all: schemedoc plt_schemedoc #match.boot
#       Here we superimpose our "patches" to the standard schemedoc
#       tree.  This isn't finished yet.  There are more files than
#       star_star_chez.scm that are modified in the schemedoc/ tree.
#       Right now this is just a lame way for me to avoid actually
#       changing the tbz and wasting space in the repository.
#	if ! [ -d html ];                             then mkdir html        ; fi
#	if ! [ -d html/generic ];                     then mkdir html/generic; fi
	if ! [ -d html/generic/compiler_components ];    then mkdir -p html/generic/compiler_components; fi
	if ! [ -d html/generic/sim ];                    then mkdir -p html/generic/sim; fi
	if ! [ -d html/generic/passes/wavescope_bkend ]; then mkdir -p html/generic/passes/wavescope_bkend; fi
	if ! [ -d html/generic/passes/mlton_bkend ];     then mkdir -p html/generic/passes/mlton_bkend; fi
	if ! [ -d html/generic/passes/ocaml_bkend ];     then mkdir -p html/generic/passes/ocaml_bkend; fi
	if ! [ -d html/chez ];                           then mkdir -p html/chez   ; fi
	if ! [ -d html/plt ];                            then mkdir -p html/plt    ; fi
	@echo
	@echo First scrub source:
	@echo -------------------
	$(MAKE) $(SRCCOPY)
	@echo
	@echo Second build individual manuals:
	@echo --------------------------------
	$(MAKE) $(HTMLFILES)

	@echo "------------------------------------------------------------------------------"
	@echo "Done generating Regiment documentation."

##      Now generate index:
	$(SCHEME) --script regiment_index.laml $(HTMLFILES)

# Here we build our pattern matcher for use in schemedoc and scrub_source
match.boot: match.so
	echo '(make-boot-header "match.hdr" "scheme.boot" "petite.boot")' | $(SCHEME)
	cat match.hdr match.so > match.boot

match.so: 
	cp "$(WAVESCRIPTD)/src/chez/match.ss" ./match.ss
	echo '(import iu-match)' >> match.ss 
	echo '(compile-file "match.ss" "match.so")' | $(SCHEME)
#	echo '(compile-file (string-append (getenv "WAVESCRIPTD") "/src/chez/match.ss") "match.so")' | $(SCHEME)

schemedoc:
	tar xjvf schemedoc.tbz; 
	$(MAKE) patch


# Should switch this over to actually using *patch*
patch:
	cp patches_new/laml.scm schemedoc/;
	cp patches_new/laml.init schemedoc/;
	cp patches_new/star_star_chez.scm schemedoc/lib/compatibility/; 
	cp patches_new/time.scm schemedoc/lib/; 
	cp patches_new/schemedoc-extractor.scm schemedoc/tools/schemedoc-extractor/; 
#	cp patches_new/elucidator.scm schemedoc/styles/xml-in-laml/elucidator-2/; 

# And a single patch here too:
patch_plt:
	cp patches_new/schemedoc-extractor.scm plt_schemedoc/tools/schemedoc-extractor/; 
	cp patches_new/laml.init_plt plt_schemedoc/laml.init
	cp patches_new/laml.scm_plt plt_schemedoc/laml.scm

# Copy the src, scrubbing it in the process.
# $(SRCTEMP): html/%.ss.tmp: ../src/%.ss
# 	$(SCHEME) --script scrub_source.ss $< $@

# $(SRCCOPY): html/%.ss: ../src/%.ss.tmp
# 	echo Now performing additional pass to clean match and remove graph printing.
# 	cat $< | sed "s/(match/'(match/g" | sed "s/#[[:digit:]]=//g" | sed "s/#[[:digit:]]#//g" > $@ 
# 	echo ";; SCRUBBED FOR LAML SCHEMEDOC COMPATIBILITY!" >> $@ 


$(SRCCOPY): html/%.ss: ../src/%.ss
	$(SCHEME) --script scrub_source.ss $< $@.tmp
	@echo "  Now performing additional pass to clean match and remove graph printing."
	cat $@.tmp | sed "s/(match/'(match/g" | sed "s/#[[:digit:]]=//g" | sed "s/#[[:digit:]]#//g" > $@ 
	@echo ";; SCRUBBED FOR LAML SCHEMEDOC COMPATIBILITY!" >> $@ 


# # This invokes regiment.laml with the directory and the filename as command line arguments:
$(HTMLFILES): html/%.html: html/%.ss
	mzscheme -r regiment_plt.laml $(WAVESCRIPTD)/doc/`dirname $@`/  `echo $< | sed 's/.*\///'`
#	$(SCHEME) --script regiment.laml $(WAVESCRIPTD)/doc/`dirname $@`/  `echo $< | sed 's/.*\///'`


#`echo $(<:html/%.ss=%.ss) | sed 's/generic\///'`
#$(<:html/%=%)



install: installreadme
	cp -vpr html /var/www/codedoc/
	chgrp www-data -R /var/www/codedoc/
	chmod g+rX -R /var/www/codedoc/*

installreadme:
	cp ../README /var/www/regimentman/
	cp ../README.html /var/www/regimentman/

clean: 
	$(RM) -rf html schemesource.tmp *~ .__scratch_makefile_junk
#       This is intense, we delete and rebuild this tree every time:
	$(RM) -rf _temp* match.so match.boot
##	$(RM) *.html *.css *.manlsp *~

distclean: clean
	$(RM) -rf schemedoc
