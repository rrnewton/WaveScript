

#*---------------------------------------------------------------------*/
#*    Primary Compile Targets                                          */
#*---------------------------------------------------------------------*/

# The Regiment/WaveScript compiler can be ran under several scheme
# implementations.

# make          -- build wsparse and .so file
# make wsparse  -- build parser

# make [ikarus,chez,pltbc] -- build compiler under different implementations.


#export REGIMENTD=`pwd`/../

basic: 
	@echo; echo BUILDING PARSER
	@echo "======================================================================"
	$(MAKE) wsparse
	@echo; echo GENERATING AGGREGATE LIBRARIES
	@echo "======================================================================"
	$(MAKE) aggregated
	@echo; echo BUILDING COMPILER # UNDER IKARUS
	@echo "======================================================================"
	if $(MAKE) chez; then echo; else echo "  ==== FALLING BACK TO IKARUS ===="; $(MAKE) ik; fi

# 	@echo; echo; echo [OPT] Compiling Regiment Scheme Source w/ Unsafe Optimizations
# 	@echo "======================================================================"
# 	@if [ `which chez` ]; then $(MAKE) ws.opt; else echo "Full Chez compiler not found, skipping build."; fi
# 	@echo; echo; echo 'Compiling OCaml FFTW wrapper (only needed for wscaml)'
# 	@echo "======================================================================"
# 	@if [ `which ocaml` ]; then $(MAKE) ocaml; else echo Ocaml install not found.; fi

# 	@echo; echo; echo BUILDING C-code extensions.
# 	@echo "======================================================================"
# 	@echo "Don't worry if this step fails, it's optional!"
# 	@echo "======================================================================"
# 	$(MAKE) c

all: basic ws.debug ws.opt

# allheap: fullheap fullheap.opt

MT=`echo '(display (machine-type))' | ../depends/petite -q`
SVNREV=`svn info | grep Revision | sed s/Revision:\ //`
CMT=`echo '(machine-type)' | chez_threaded`

#*---------------------------------------------------------------------*/
#*   Compiling.                                                        */
#* We handle multiple scheme backends as well as other misc code.      */
#*- --------------------------------------------------------------------*/

c: 
	(cd C; $(MAKE))

caml: ocaml
ocaml:
	(cd ws/passes/ocaml_bkend/; $(MAKE))

# A couple of these aggregate scheme libraries are automatically generated:
ag: aggregated
aggregated:
	rm -f ws/common.sls main_r6rs.sls
	$(MAKE) ws/common.sls main_r6rs.sls

# [2009.10.09] Let's factor this:
#AGGREGATOR = plt-r6rs
#AGGREGATOR = $(CHEZ) --program
#AGGREGATOR = $(IKARUS) --r6rs-script

# With some Scheme implementations I'm having problems with passing in a command line argument with a space inside it.
ws/common.sls:
	plt-r6rs                ./generate_main_r6rs.ss common_import_list.sexp R6PATH ws common 
        # This is hacked to work with either scheme implementation:
#	$(IKARUS) --r6rs-script ./generate_main_r6rs.ss common_import_list.sexp R6PATH ws common || \
#	$(CHEZ)   --program     ./generate_main_r6rs.ss common_import_list.sexp R6PATH ws common 
#	$(AGGREGATOR)           ./generate_main_r6rs.ss common_import_list.sexp R6PATH ws common 

#	larceny -r6rs -path . -program  ./generate_main_r6rs.ss -- common_import_list.sexp '(ws common)'

main_r6rs.sls:
	plt-r6rs                ./generate_main_r6rs.ss 
#	$(IKARUS) --r6rs-script ./generate_main_r6rs.ss || \
#	$(CHEZ) --program ./generate_main_r6rs.ss 
#	$(AGGREGATOR)           ./generate_main_r6rs.ss 

##	larceny -r6rs -path . -program  ./generate_main_r6rs.ss


#----------------------------------------#
#    Default compiler build entrpoints   #
#----------------------------------------#

# These presently invoke the Chez version:

# [2007.04.22] Switching the default to .boot files:
ws:       chez
ws.opt:   chezopt
opt:      chezopt
ws.debug: chezdebug
debug:    chezdebug
dbg:      chezdebug


#----------------------------------------#
#    Ikarus - a good open source R6RS    #
#----------------------------------------#

IKARUS =`if [ "$(IKARUSEXEC)" = "" ]; then echo ikarus; else echo $(IKARUSEXEC); fi`

# Depends on aggregated libraries:
ikarus: ik
ik: ws/common.sls main_r6rs.sls
	$(IKARUS) -O2 --compile-dependencies regiment.ss


#----------------------------------------#
#    PLT - supporting R6RS as well       #
#----------------------------------------#

# Most common entrypoint: "pltbc"

MZSCHEME = mzscheme
MZC = mzc
# Trying to do some optimization: 
MZCFLAGS = --prim --unsafe-skip-tests  --unsafe-fixnum-arithmetic --unsafe-disable-interrupts 

DEFAULTPLTLIB=`mzscheme -e '(for-each (lambda (p) (display p) (display ":")) (current-library-collection-paths))'`

bc:pltbc
pltbc:
	find -name "._*.ss" | xargs -i rm {}  ## So they don't break our compilation.
	find -name "_*.ss" | xargs -i rm {}  ## So they don't break our compilation.	
        # Updating this to only build bytecode for WAVESCRIPT:
	$(MAKE) pltquick
pltquick:
	(export PLTCOLLECTS=$(DEFAULTPLTLIB)`pwd`; plt-r6rs --compile regiment.ss)
#	(export PLTCOLLECTS=`pwd`:$(PLTLIB); mzc $(MZCFLAGS) --collection-zos .)

# This version DOES NOT build regiment sources.  Leaner and meaner.
wsbc: ws/common.sls main_r6rs.sls 
	find -name ".__*.ss" | xargs -i rm {}  ## So they don't break our compilation.
	find -name "_*.ss" | xargs -i rm {}  ## So they don't break our compilation.	
        # Updating this to only build bytecode for WAVESCRIPT:
	(export REGIMENT_OR_WAVESCRIPT=WS; \
         export PLTCOLLECTS=`pwd`:$(PLTLIB); $(MZC) $(MZCFLAGS) --collection-zos .)


# Disabling this by default because I'm getting a 'collection not found: "mzlib"' error.
#	$(MAKE) pltexe

pltexe:
	$(MZC) --exe regiment_pltscript.exe regiment_pltscript.ss
	$(MZC) --exe-dir regiment_pltscript regiment_pltscript.exe 

nc: pltnc
pltnc:  mzscheme_native
native: mzscheme_native
mzscheme_native: 
	find -name ".__*.ss" | xargs -i rm {}  ## So they don't break our compilation.
	(export PLTCOLLECTS=`pwd`:$(PLTLIB); $(MZC) $(MZCFLAGS) --collection-extension .)

# building to native code with a collection isn't working:
nc2:
	export PLTCOLLECTS=`pwd`:$(PLTLIB)
	cat .temp_plt-source-files  | xargs -n1 $(MZC) $(MZCFLAGS) --auto-dir

#----------------------------------------#
#    Parser (PLT only)                   #
#----------------------------------------#

wsparse: wsparse_zo

# only works in PLT right now:
wsparse_native: 
	$(MZC) --exe bin/wsparse parser/wsparse.ss
# I tried embedding mzscheme to build a stand-alone... but I don't
# think it was really standalone.

wsparse_full:
	$(MZC) $(MZCFLAGS) --exe bin/wsparse parser/wsparse.ss
	$(MZC) $(MZCFLAGS) --exe-dir wsparse_full bin/wsparse 

# Hack, this script tries the absolute and the relative path.
# Err... there's a problem with symlinks and resolution of ../
# Seems like PLT and bash do it differently... not very portable besides.
# Just giving in and using REGIMENTD.
wsparse_zo: 
	$(MZC) -z --auto-dir parser/*.ss
	echo '#!/bin/bash' > bin/wsparse
	chmod +x bin/wsparse
	echo 'source `dirname $$0`/assert_regimentd' >> bin/wsparse
	echo 'mzscheme -qu "$$REGIMENTD/src/parser/wsparse.ss" $$*' >> bin/wsparse

#	echo 'DIR=`pwd`' >> bin/wsparse
# 	echo 'LOC1=`dirname $$0`/../parser/wsparse.ss' >> bin/wsparse
# 	echo 'LOC2=`dirname $$DIR/$$0`/../src/parser/wsparse.ss' >> bin/wsparse
# 	echo 'echo Checking absolute location $$LOC1' >> bin/wsparse
# 	echo 'if [ -f $$LOC1 ]; then echo ok absolute is there; else echo erk; fi' >> bin/wsparse
# 	echo 'if [ -f $$LOC1 ]; then echo absolute; mzscheme -qt "$$LOC1" $$*; ' >> bin/wsparse
# 	echo '                    else mzscheme -qt "$$LOC2" $$*; fi' >> bin/wsparse


#	echo 'echo runnig wsparse, dir $$0 `pwd` args $$*' >> bin/wsparse
#	echo 'LOC=`dirname $$DIR/$$0`/../src/parser/wsparse.ss' >> bin/wsparse



#----------------------------------------#
#    Chez                                #
#----------------------------------------#

# Most common entrypoint: "chez"

# We support Chez in two ways.  After the R6RS port I made a dirty
# hack that stripped the R6RS libraries into a form that Chez could
# consume.  But once Chez got its own R6RS support this wasn't
# necessary.  I would remove it, but the fact is that it's the only
# current [2009.03.11] way to make a single .boot file (and it loads
# twice as fast).  So the hack still has its uses.

# The new one:
CHEZ = `if (which chez > /dev/null); then echo chez; else echo ../depends/chez; fi`
#CHEZ = `if (bash -c "which chez" > /dev/null); then bash -c "which chez"; else echo ../depends/chez; fi`

# Old, possibly pre-r6 chez:
OLDCHEZ = chez74

# This is the R6RS chez:
chez: chezwhole

# Do an incremental rebuild of dirty objects:
chezincr: ws/common.sls main_r6rs.sls 
        # Build all the individual .so files:
	./experimental/r6make.ss regiment.ss
        # It would be nice to make a .boot file out of them...
#	echo '(make-boot-header "tmp.hdr" "scheme.boot" "petite.boot")' | $(CHEZ)
#	find -name "*.so"

chezpurge: ws/common.sls main_r6rs.sls 
	./experimental/r6make.ss regiment.ss purge
chezwipe: chezpurge

chezwhole:
	find . -name "*.so" | xargs $(RM)
	echo '(make-boot-header (format "build/~a/main.hdr" (machine-type)) "scheme.boot" "petite.boot")' | chez
	./experimental/r6make.ss regiment.ss list > build/so_list.txt
	make chezincr
	cat build/so_list.txt | xargs cat build/$(MT)/main.hdr > build/$(MT)/regiment.complete.boot

chezdebug:
	if [ -f build/$(MT)/regiment.complete.boot ]; then mv build/$(MT)/regiment.complete.boot __TEMP; fi
	REGDEBUGMODE=ON make chezwhole
	mv build/$(MT)/regiment.complete.boot build/$(MT)/regiment.debug.boot
	if [ -f __TEMP ]; then mv __TEMP build/$(MT)/regiment.complete.boot; fi

chezopt:
	if [ -f build/$(MT)/regiment.complete.boot ]; then mv build/$(MT)/regiment.complete.boot __TEMP; fi
	REGOPTLVL=3 make chezwhole
	mv build/$(MT)/regiment.complete.boot build/$(MT)/regiment.opt.boot
	if [ -f __TEMP ]; then mv __TEMP build/$(MT)/regiment.complete.boot; fi

chezall: chezwhole chezdebug chezopt


# This is the old HACK:

oldchez: boot

# Currently the hack requires first building ikarus.

# This converts the R6RS codebase to a Chez-palatable form.
ag_chez: aggregated_chez
aggregated_chez:
	./temporary_smoosh_to_one_chez_file.ss

# This compiles the source into loadable object files
chez_hack: ws/common.sls main_r6rs.sls chez_native ikarus
chez_native: aggregated_chez
	if ! [ -d build/$(MT) ]; then mkdir -p build/$(MT); fi
	if [ "$(REGDEBUGMODE)" == "ON" ]; \
        then echo '(compile-file "legacy_main_chez.ss" (format "build/~a/legacy_main_chez_DBG.so" (machine-type)))' | \
             (export REGIMENT_OR_WAVESCRIPT=BOTH; $(OLDCHEZ)); \
        else echo '(compile-file "legacy_main_chez.ss" (format "build/~a/legacy_main_chez.so" (machine-type)))' | \
             (export REGIMENT_OR_WAVESCRIPT=BOTH; $(OLDCHEZ)); fi

# This makes a Chez Scheme .boot file instead of a shared object file.
# After the boot file is made we can kill the shared object file.
boot: aggregated_chez boot_only
boot_only: _temp_temp.so shared 
	echo '(make-boot-header (format "build/~a/main.hdr" (machine-type)) "scheme.boot" "petite.boot")' | $(OLDCHEZ)
	cat "build/$(MT)/main.hdr" "_temp_temp.so" "build/$(MT)/legacy_main_chez.so" > "build/$(MT)/legacy_main_chez.boot" 
	rm -f _temp_temp.ss _temp_temp.so "build/$(MT)/legacy_main_chez.so"

boot.opt: aggregated_chez _temp_temp.so shared.opt 
	echo '(make-boot-header (format "build/~a/main_opt.hdr" (machine-type)) "scheme.boot" "petite.boot")' | $(OLDCHEZ)
	cat "build/$(MT)/main_opt.hdr" "_temp_temp.so" "build/$(MT)/legacy_main_chez_OPT.so" > "build/$(MT)/legacy_main_chez_OPT.boot" 
	rm -f _temp_temp.ss _temp_temp.so


# This is an extra little header:
_temp_temp.so:
	echo '(define regiment-origin "compiled .boot")' > _temp_temp.ss
	echo '(compile-file "_temp_temp.ss" "_temp_temp.so")' | $(OLDCHEZ)

## These targets just make the shared objects as necessary for the boot file.
## In this case we only worry include the WS target, not regiment.
so: shared
shared: 
	if ! [ -d "build/$(MT)" ]; then mkdir -p build/$(MT); fi
	export REGIMENT_OR_WAVESCRIPT=WS; 
	echo '(generate-inspector-information #f) (compile-file "legacy_main_chez.ss" (format "build/~a/legacy_main_chez.so" (machine-type)))' | \
         (export REGOPTLVL=2; \
          export REGDEBUGMODE=OFF; \
          export REGIMENT_OR_WAVESCRIPT=WS; \
          $(OLDCHEZ))

so.debug: shared.debug
shared.debug:
	if ! [ -d build/$(MT) ]; then mkdir -p build/$(MT); fi
	export REGIMENT_OR_WAVESCRIPT=WS; 
	echo '(compile-file "legacy_main_chez.ss" (format "build/~a/legacy_main_chez_DBG.so" (machine-type)))' | \
         (export REGOPTLVL=2; \
          export REGDEBUGMODE=ON; \
          export REGIMENT_OR_WAVESCRIPT=WS; \
          $(OLDCHEZ))

# so.opt: shared.opt
# shared.opt:
# 	MT=`echo '(machine-type)' | ../depends/petite`
# 	if ! [ -d build/$(MT) ]; then mkdir build/$(MT); fi	
# 	echo '(compile-file "main_chez.ss" (format "build/~a/main_chez_OPT.so" (machine-type)))' | \
#           (export REGOPTLVL=3; \
#            export REGDEBUGMODE=OFF; \
#            export REGIMENT_OR_WAVESCRIPT=WS; \
#            $(CHEZ))


# ws.threaded: threaded
# threaded:
# #	if ! [ -d build/$(CMT) ]; then mkdir build/$(CMT); fi
# 	export REGIMENT_OR_WAVESCRIPT=WS; 
# 	echo '(compile-file "main_chez.ss" (format "build/~a/main_chez.so" (machine-type)))' | \
#          (export REGOPTLVL=2; \
#           export REGDEBUGMODE=OFF; \
#           export REGIMENT_OR_WAVESCRIPT=WS; \
#           chez_threaded)

# # Saved heap images start up very fast, but don't work right on all platforms.
# # (doesn't work right on Mac OS or fedora core)

# # Run "make chez" before this to compile the code before grabbing a Petite heap:
# heap: builddir 
# 	depends/petite -s0 build/$(MT)/regiment.0.heap --script regiment.ss ./
# #	echo '(load "regiment.ss")' | chez -s0 build/$(MT)/regiment.0.heap
# #	$(RM) -f build/regiment.0.heap 
# #	ln -s build/$(MT)/regiment.0.heap build/regiment.0.heap

# # A fullheap includes the Chez compiler as well as the petite heap.
# fullheap: builddir
# 	$(CHEZ) -s0 build/$(MT)/regiment.0.heap --script regiment.ss ./;
# #	@if [ `which chez` ]; \
# #	echo "$(CHEZ) -s0 build/$(MT)/regiment.0.heap --script regiment.ss ./"; \
# #	     $(CHEZ) -s0 build/$(MT)/regiment.0.heap --script regiment.ss ./; \
# #	     $(RM) -f build/regiment.0.heap; \
# #	     ln -s build/$(MT)/regiment.0.heap build/regiment.0.heap; \
# #	else echo "Cannot build fullheap without full Chez Scheme (put 'chez' in path)"; \
# #	fi

# ##	echo '(load "regiment.ss")' | chez -s0 build/$(MT)/regiment.0.heap

# fullheap.opt:
# 	(export REGOPTLVL=3; \
#          export REGDEBUGMODE=OFF; \
# 	$(CHEZ) -s0 build/$(MT)/regiment.0.OPT.heap --script regiment.ss ./; )

# builddir:
# 	if [ ! -d build/$(MT) ]; then mkdir build/$(MT); fi


#----------------------------------------#
#    Larceny (never fully working)       #
#----------------------------------------#

# (main_r6rs) (main)
# Ack this takes 43 minutes... I remember it taking 5 minutes before to load from source.
larc:
	echo '(import (primitives dump-interactive-heap current-require-path compiler-switches) (rnrs)) (compiler-switches) (import (rnrs r5rs (6)) (rnrs mutable-pairs (6)) (rnrs mutable-strings (6)) (main_r6rs) (main))  (dump-interactive-heap "./build/'$(MT)'/larc.heap")' | larceny -err5rs -stopcopy -path .

# This also takes 43 minutes.
larc2:
	larceny -r6rs -stopcopy -path . -program build_larceny.ss

# How about without -stopcopy: Well that only takes 4 minutes!!
larc3:
	larceny -r6rs -path . -program build_larceny.ss

runlarc:
	larceny -r6rs -path . -program regiment.ss


#	echo '(import (primitives dump-interactive-heap current-require-path compiler-switches) (rnrs)) (compiler-switches (string->symbol "slow")) (compiler-switches) (import (rnrs r5rs (6)) (rnrs mutable-pairs (6)) (rnrs mutable-strings (6)) (main_r6rs) (main) ) (dump-interactive-heap "larc.heap")' | larceny -err5rs -stopcopy -path .

#	larceny -r6rs -program test_run.ss -path .
#	echo '(import (err5rs load)) (load "main_larceny.ss")' | larceny -err5rs -stopcopy

#	echo '(import (rnrs) (larceny compiler) (primitives compiler-switches)) (compiler-switches (string->symbol "slow")) (compile-library "'$<'")' | larceny -r6rs -path $(REGIMENTD)/src



cs:
	larceny -r6rs -path . -program scrap/compile-stale.ss

# Disable ALL larc opts
# (include-procedure-names #f) (include-variable-names #f) 
# (inline-assignment #f) (optimize-c-code #f)
NOOPTS='(control-optimization #f) (parallel-assignment-optimization #f) (lambda-optimization #f) (benchmark-mode #f) (local-optimization) (global-optimization #f) (peephole-optimization #f) (inline-allocation #f)'

# Trying to speed this up with global-optimization #f
%.slfasl : %.sls
	echo '(import (larceny compiler) (primitives global-optimization)) (global-optimization #f) (compile-library "'$<'")' | larceny -r6rs -path $(REGIMENTD)/src
#	echo '(import (rnrs) (larceny compiler) (primitives compiler-switches)) (compiler-switches (string->symbol "slow")) (compile-library "'$<'")' | larceny -r6rs -path $(REGIMENTD)/src
#	echo '(import (larceny compiler)) (compile-library "'$<'")' | larceny -r6rs -path $(REGIMENTD)/src/


#ws/constants.sls: ws/util/compat.sls
ws/globals.slfasl: ws/compat/compat.slfasl
ws/util/iu-match.slfasl: ws/compat/compat.slfasl
ws/util/reg_macros.slfasl: ws/globals.slfasl ws/util/iu-match.slfasl
ws/util/helpers.slfasl: ws/util/reg_macros.slfasl ws/util/hashtab.slfasl 

# Making these dependencies artificially linear:
# This over-sequentializes and ruins make -j
ws/util/streams.slfasl: ws/util/helpers.slfasl
ws/compiler_components/prim_defs.slfasl: ws/util/streams.slfasl
ws/compiler_components/regiment_helpers.slfasl: ws/compiler_components/prim_defs.slfasl
ws/compiler_components/type_environments.slfasl: ws/compiler_components/regiment_helpers.slfasl
ws/compiler_components/reg_core_generic_traverse.slfasl: ws/compiler_components/type_environments.slfasl
ws/compiler_components/hm_type_inference.slfasl: ws/compiler_components/reg_core_generic_traverse.slfasl
ws/grammars/grammar_checker.slfasl: ws/compiler_components/hm_type_inference.slfasl
ws/passes/pass-mechanism_basic.slfasl: ws/grammars/grammar_checker.slfasl
ws/passes/pass-mechanism.slfasl: ws/passes/pass-mechanism_basic.slfasl

## This builds all the larceny files:
## compile-stale-libraries isn't working.
#larceny: ws/util/helpers.slfasl 
larceny: ws/common.sls main_r6rs.sls \
   ws/passes/pass-mechanism.slfasl \
   ws/common.slfasl \
   ws/util/scheme_fft.slfasl  \
   ws/util/slib_fft.slfasl  \
   ws/util/fft.slfasl  \
   ws/util/tsort.slfasl  \
   ws/util/hash.slfasl   \
   ws/util/slib_hashtab.slfasl  \
   ws/util/bos_oop.slfasl  \
   ws/util/imperative_streams.slfasl  \
  ws/compiler_components/annotations.slfasl \
  ws/compiler_components/c_generator.slfasl \
  ws/compiler_components/source_loader.slfasl \
  ws/passes/partition-graph.slfasl \
  ws/passes/graphviz.slfasl \
  depends/matpak.slfasl \
  ws/langs/lang_wavescript.slfasl \

# Gets an error related to multiple expand of common.sls ...

#   ws/sim/wavescript_sim_library_push.slfasl \
#   ws/passes/normalize_source/verify-regiment.slfasl \
#   ws/passes/normalize_source/typecheck.slfasl \
#   ws/passes/normalize_source/desugar-pattern-matching.slfasl \
#   ws/passes/normalize_source/resolve-varrefs.slfasl \
#   ws/passes/normalize_source/ws-label-mutable.slfasl \
#   ws/passes/normalize_source/desugar-misc.slfasl \
#   ws/passes/normalize_source/eta-primitives.slfasl \

#  ws/passes/small-ws-passes.slfasl \

#   ws/passes/normalize_source/remove-unquoted-constant.slfasl \
#   ws/passes/normalize_query/reduce-primitives.slfasl \
#   ws/passes/normalize_query/ws-lift-let.slfasl \
#   ws/passes/normalize_query/ws-remove-letrec.slfasl \
#   ws/passes/normalize_query/remove-complex-constant.slfasl \
#   ws/passes/normalize_query/uncover-free.slfasl \
#   ws/passes/normalize_query/lift-letrec.slfasl \
#   ws/passes/normalize_query/lift-letrec-body.slfasl \
#   ws/passes/normalize_query/remove-lazy-letrec.slfasl \
#   ws/passes/normalize_query/verify-core.slfasl \
#   ws/passes/normalize_query/ws-remove-complex-opera.slfasl \
#   ws/passes/normalize_query/ws-normalize-context.slfasl \
#   ws/passes/static_elaborate/static-elaborate.slfasl \
#   ws/passes/static_elaborate/interpret-meta.slfasl \
#   ws/passes/static_elaborate/degeneralize-arithmetic.slfasl \
#   ws/passes/static_elaborate/verify-elaborated.slfasl \
#   ws/passes/static_elaborate/split-union-types.slfasl \
#   ws/passes/optimizations/smoosh-together.slfasl \
#   ws/passes/optimizations/rewrite_opts.slfasl \
#   ws/passes/optimizations/merge-iterates.slfasl \
#   ws/passes/optimizations/simple-merge-iterates.slfasl \
#   ws/passes/wavescope_bkend/nominalize-types.slfasl \
#   ws/passes/wavescope_bkend/convert-sums-to-tuples.slfasl \
#   ws/passes/wavescope_bkend/reify-certain-types.slfasl \
#   ws/passes/wavescope_bkend/type-annotate-misc.slfasl \
#   ws/passes/wavescope_bkend/flatten-iterate-spine.slfasl \
#   ws/passes/wavescope_bkend/anihilate-higher-order.slfasl \
#   ws/passes/wavescope_bkend/explicit-stream-wiring.slfasl \
#   ws/passes/wavescope_bkend/emit-c.slfasl \
#   ws/passes/wavescope_bkend/insert-refcounts.slfasl \
#   ws/passes/wavescope_bkend/emit-c2.slfasl \
#   ws/passes/ocaml_bkend/emit-caml.slfasl \
#   ws/passes/ocaml_bkend/shared-emit-ml.slfasl \
#   ws/passes/mlton_bkend/emit-mlton.slfasl \
#   ws/passes/analyze_data_rates/annotate-with-data-rates.slfasl



#  ws/passes/normalize_source/rename-vars.slfasl \


#/*--------------------------------------------------------------------*/
#                              TESTING                                 #
#/*--------------------------------------------------------------------*/

# This is basic, for someone who just downloaded WaveScript:
t: test
test:
#	$(MAKE) clean wsparse ag
	regiment test
	(cd demos/wavescope; $(MAKE) getdata)
	(cd demos/wavescope; ./testall_demos.ss)
	ws ../lib/stdlib_test.ws -exit-error -n 10;

# This a lot of tests but tries not to be fairly quick.
# This is in contrast to supertest.ss, which is supposed to be as
# exhaustive as possible.
fulltest: 
	rm -rf /tmp/wsparse_server_*
#killall mzscheme
	$(MAKE) clean 
	$(MAKE) wsparse
	regiment test
        # Now build it to speed up these subsequent tests:
	if [ `which chez` ]; then $(MAKE) boot; fi 
	(cd ../apps/pipeline-web/; $(MAKE) test)
# NOT WORKING RIGHT NOW:
	(cd ../apps/stockticks/; $(MAKE) test)
	(cd demos/wavescope; $(MAKE) getdata)
	(cd demos/wavescope; ./testall_demos.ss)
	@echo
	@echo "Now test libraries (stdlib.ws, matrix.ws)"
	@echo "============================================================"
#	$(MAKE) dbg # Now build DEBUG version to speed up these subsequent tests:
	ws ../lib/stdlib_test.ws -exit-error -n 10;
	ws ../lib/test_matrix.ws -exit-error -n 10;
#	cd ../src;
	export REGDEBUGMODE=ON
	$(MAKE) bc
	export REGDEBUGMODE=OFF
#	(cd demos/wavescope; ./testall_wsc)
#	./regiment_script.ss test


# This is seperate... requires full Chez (for compiler) as well as GSL.
testgsl:
	(cd ../lib; $(MAKE));
	echo exit | ws ../lib/run_matrix_gsl_test.ws -exit-error;

supertest: super-test
super-test: 
	./supertest.ss
# 	@echo Testing different build methods.
# 	@echo
# 	@echo ======================================================================
# 	$(MAKE) clean 
# 	@echo
# 	@echo First from source:
# 	@echo ======================================================================
# 	./regiment_script.ss test
# 	echo 'ntohue' | regiment i --exit-error
# 	echo HMM $?
# 	@echo
# 	@echo Second from compiled shared-object:
# 	@echo ======================================================================


#/*--------------------------------------------------------------------*/
#*            Building Installation Packages                           */
#*---------------------------------------------------------------------*/

# [2007.04.09] Updating this, it was very old:
PKGDIR=$(REGIMENTD)/binary_pkg
#clean ws ws.opt wsparse.full
fullpackage: pkg
fullpkg: 
	@echo
	@echo PACKAGING REGIMENT/WAVESCRIPT FOR BINARY INSTALLATION
	@echo =================================================================
	rm -rf $(PKGDIR)
	mkdir $(PKGDIR)
	mkdir $(PKGDIR)/depends
	cp -pr $(REGIMENTD)/install_environment_vars $(PKGDIR)/
	cp -pr $(REGIMENTD)/src/bin        $(PKGDIR)/
	cp -pr $(REGIMENTD)/src/build      $(PKGDIR)/
	cp -pr $(REGIMENTD)/src/wsparse_full    $(PKGDIR)/
	cp -pr $(REGIMENTD)/depends/chez   $(PKGDIR)/depends/
	cp -pr $(REGIMENTD)/depends/csv7.3 $(PKGDIR)/depends/
	rm -rf $(PKGDIR)/bin/wsparse
	ln -s $(PKGDIR)/wsparse_full/bin/wsparse $(PKGDIR)/bin/wsparse


## TODO - set the version automatically!

#SVNREV=$(shell svn info https://svn.csail.mit.edu/wavescript/branches/wavescope | grep Revision | awk '{ print $2 }')
#SVNREV=$(shell svn info | grep Revision | awk '{ print $$2 }' )
SVNREV=$(shell if [ "`svn info`" == "" ]; then svn info https://svn.csail.mit.edu/wavescript/branches/wavescope; else svn info; fi | grep Revision | awk '{ print $$2 }' )

WSVER=$(shell cat version).$(SVNREV)
DEBPKG=$(REGIMENTD)/wavescript_$(WSVER)
# This will build a source package.
ubuntu: deb
debian: deb
deb: 
	@echo Building deb for working directory, revision "$(SVNREV)"
	rm -rf $(DEBPKG)
	mkdir $(DEBPKG)
	cp -a debian $(DEBPKG)
	mv $(DEBPKG)/debian/package_deb.sh $(DEBPKG)/
	(cd $(DEBPKG) && export WSVER=$(WSVER); ./package_deb.sh source) 
	@echo Now run 'make' from $(DEBPKG).

#/*--------------------------------------------------------------------*/
#*    Cleaning                                                         */
#*---------------------------------------------------------------------*/
#.PHONY: clean

cleanheap:
	$(RM) build/regiment.0.heap
	$(RM) build/i3nt/regiment.0.heap build/i3le/regiment.0.heap build/ppcosx/regiment.0.heap

# Clean the compiler stuff but not wsparse.
cleanchez: cleancomp
	find . -name "*.so" | xargs $(RM)
	find ../depends/ -name "*.so" | xargs $(RM)
cleancomp:
	$(RM) build/i3nt/* build/i3le/* build/ppcosx/* build/i3osx/* build/ti3osx/* build/ti3le/*


cleanplt:
	rm -rf ./compiled
	rm -rf ./plt/compiled
	rm -rf ./ws/compiled
	rm -rf ./ws/compiler_components/compiled
	rm -rf ./ws/grammars/compiled
	rm -rf ./ws/passes/compiled
	rm -rf ./ws/passes/deglobalize/compiled
	rm -rf ./ws/passes/nesc_bkend/compiled
	rm -rf ./ws/passes/normalize_query/compiled
	rm -rf ./ws/passes/normalize_source/compiled
	rm -rf ./ws/passes/static_elaborate/compiled
	rm -rf ./ws/passes/tokmac_bkend/compiled
	rm -rf ./ws/passes/wavescope_bkend/compiled
	rm -rf ./ws/sim/compiled
	rm -rf ./ws/util/compiled
	find -name *.zo | xargs -i rm {}  ## <- Just to be sure

cleanik:
	find ../depends -name "*.ikarus-fasl" | xargs $(RM)
	find . -name "*.ikarus-fasl" | xargs $(RM)

cleanlarc:
	find -name "*.slfasl" | xargs $(RM)


clean: cleanchez cleanplt cleanik cleanlarc
	if [ -d C ]; then (cd C && $(MAKE) clean); fi
	if [ -d demos/token_machs ]; then  (cd demos/token_machs && $(MAKE) clean); fi
	if [ -d demos/regiment ];    then (cd demos/regiment && $(MAKE) clean); fi
	(cd ws/passes/ocaml_bkend/ && $(MAKE) clean)
	$(RM) *.zo *.so _SIM_* *~ 
	$(RM) __temp.log _genned_node_code.ss svn_rev.txt 
	find -name _genned_node_code.ss | xargs $(RM)
	$(RM) bin/wsparse
	$(RM) ws/common.sls main_r6rs.sls chez_aggregated.ss
	find -name ".__*" | xargs -i $(RM) {}  ## Remove these temp files
	find -name "__lang_running.tmp.ss" | xargs -i $(RM) {}  ## Remove these temp files

distclean: clean
	$(RM) -f *.log


#============================================================
# Shortcuts, utils, and junk.
#============================================================

upload:
	scp build/$(MT)/main_chez.so "newton@faith.ffh.us:/var/www/regiment_binaries/"$(SVNREV)"_"$(MT)"_main_chez.so"
	ssh newton@faith.ffh.us chgrp www-data "/var/www/regiment_binaries/"$(SVNREV)"_"$(MT)"_main_chez.so"
	ssh newton@faith.ffh.us chmod g+r "/var/www/regiment_binaries/"$(SVNREV)"_"$(MT)"_main_chez.so"
	scp bin/wsparse "newton@faith.ffh.us:/var/www/regiment_binaries/"$(SVNREV)"_"$(MT)"_wsparse"
	ssh newton@faith.ffh.us chgrp www-data "/var/www/regiment_binaries/"$(SVNREV)"_"$(MT)"_wsparse"
	ssh newton@faith.ffh.us chmod g+r "/var/www/regiment_binaries/"$(SVNREV)"_"$(MT)"_wsparse"


wc:        wordcount
count:     wordcount
linecount: wordcount
wordcount:
	echo;echo "total for Scheme code:" > .wc_tmp.txt
	find -name "*.ss" | grep -v "/parser/" | grep -v "old" | grep -v "scrap" | grep -v "\.__" | grep -v "\.#" | xargs wc >> .wc_tmp.txt
	echo;echo "total for r6rs Scheme code:" >> .wc_tmp.txt
	find -type f -name "*.sls" | grep -v "/parser/" | xargs wc >> .wc_tmp.txt
	echo;echo "total for .rs code:" >> .wc_tmp.txt
	find -name "*.rs" | xargs wc >> .wc_tmp.txt
	echo;echo "total for token machine code:" >> .wc_tmp.txt
	find -name "*.tm" | xargs wc >> .wc_tmp.txt
	echo;echo "total for wavescript code:" >> .wc_tmp.txt
	find ../ -type f -name "*.ws" | grep -v "\.#" | xargs wc >> .wc_tmp.txt
	echo;echo "total for c/c++ code:" >> .wc_tmp.txt
	(find ../ -name "*.h";  find ../ -name "*.hpp"; find ../ -name "*.cpp"; find ../ -name "*.c";) | grep -v "\.#" | grep -v "query" | xargs wc >> .wc_tmp.txt
	@echo;echo;
	@cat .wc_tmp.txt | grep total
	@echo;echo TOTAL total:
	@cat .wc_tmp.txt | ./depends/add_first_column.sh
# Can't figure out the quoting for $1!!
#	cat .wc_tmp.txt | grep total | grep -v code | awk '{ print $1" +"}' > .add_totals.txt
#	(echo 0; cat .add_totals; echo p)  | dc

#	cat .wc_tmp.txt | grep total | grep -v code | awk '{ print $1" +"}' > .add_totals.txt

wcnb:
	sh -c "ls *.ss ws/*.ss chez/*.ss plt/*.ss ws/*.tests haskell/*.hs | grep -v '^_' | xargs cat | grep -v '^[ ]*$'"

sortcount:
	ls -rS *.ss ws/*.ss chez/*.ss plt/*.ss ws/*.tests haskell/*.hs | xargs wc

go:
	(cd plt/compiled; make; cd ~/cur; make;) #cd plt;  mred -Z -f temp.ss)
#	(cd plt/compiled; make; cd ~/cur; make; cd plt; mred -z -f temp.ss)

