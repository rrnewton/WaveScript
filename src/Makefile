

#*---------------------------------------------------------------------*/
#*    Primary Compile Targets                                          */
#*---------------------------------------------------------------------*/

# There are three ways to run Regiment/WaveScript.
#  (*) From source
#  (*) From compiled shared object (.so) file.
#  (*) From saved memory heap. (Fast but not portable.)

# make wsparse  -- build parser
# make chez     -- build regiment .so file
# make          -- build wsparse and .so file
# make heap     -- build regiment heap file
# make fullheap -- build regiment heap file with compiler

#*---------------------------------------------------------------------*/
#*    Objects and sources                                              */
#*---------------------------------------------------------------------*/

CHEZ_MAIN_FILE = main_chez.ss
PLT_MAIN_FILE = main_plt.ss

#*---------------------------------------------------------------------*/
#*    Commands                                                         */
#*---------------------------------------------------------------------*/
MZSCHEME = mzscheme
MZC = mzc
# Trying to do some optimization: 
#   
MZCFLAGS = --prim --unsafe-skip-tests  --unsafe-fixnum-arithmetic --unsafe-disable-interrupts 


CHEZ = `if (which chez > /dev/null); then echo chez; else echo ../depends/chez; fi`

#CHEZ = petite
#CHEZ = fullpetite

SWL = swl
DEST = a.out

# This builds the .so.  Run "make heap/fullheap" separately.
basic:
	@echo; echo BUILDING PARSER
	@echo "======================================================================"
	$(MAKE) wsparse
	@echo; echo; echo Compiling Regiment Scheme Source
	@echo "======================================================================"
	@if [ `which chez` ]; then $(MAKE) ws; else echo "Full Chez compiler not found, skipping build."; fi
	@echo; echo; echo [OPT] Compiling Regiment Scheme Source w/ Unsafe Optimizations
	@echo "======================================================================"
	@if [ `which chez` ]; then $(MAKE) ws.opt; else echo "Full Chez compiler not found, skipping build."; fi
	@echo; echo; echo 'Compiling OCaml FFTW wrapper (only needed for wscaml)'
	@echo "======================================================================"
	@if [ `which ocaml` ]; then $(MAKE) ocaml; else echo Ocaml install not found.; fi

# 	@echo; echo; echo BUILDING C-code extensions.
# 	@echo "======================================================================"
# 	@echo "Don't worry if this step fails, it's optional!"
# 	@echo "======================================================================"
# 	$(MAKE) c

all: basic ws.debug

allheap: fullheap fullheap.opt

old_all: reg
	(cd plt; $(MAKE) nc)
	(cd haskell; $(MAKE))

MT=`echo '(display (machine-type))' | ../depends/petite -q`
SVNREV=`svn info | grep Revision | sed s/Revision:\ //`
CMT=`echo '(machine-type)' | chez_threaded`

#*---------------------------------------------------------------------*/
#*   Compiling.                                                        */
#*---------------------------------------------------------------------*/

c: 
	(cd C; $(MAKE))

caml: ocaml
ocaml:
	(cd generic/passes/ocaml_bkend/; $(MAKE))

# Two scheme back-ends, PLT or Chez.

# PLT: 
#----------------------------------------
bc:pltbc
pltbc:
	find -name ".__*.ss" | xargs -i rm {}  ## So they don't break our compilation.
	find -name "_*.ss" | xargs -i rm {}  ## So they don't break our compilation.	
        # Updating this to only build bytecode for WAVESCRIPT:
	(export PLTCOLLECTS=`pwd`:$(PLTLIB); mzc $(MZCFLAGS) --collection-zos .)

# This version DOES NOT build regiment sources.  Leaner and meaner.
wsbc:
	find -name ".__*.ss" | xargs -i rm {}  ## So they don't break our compilation.
	find -name "_*.ss" | xargs -i rm {}  ## So they don't break our compilation.	
        # Updating this to only build bytecode for WAVESCRIPT:
	(export REGIMENT_OR_WAVESCRIPT=WS; \
         export PLTCOLLECTS=`pwd`:$(PLTLIB); mzc $(MZCFLAGS) --collection-zos .)


# Disabling this by default because I'm getting a 'collection not found: "mzlib"' error.
#	$(MAKE) pltexe

pltexe:
	mzc --exe regiment_pltscript.exe regiment_pltscript.ss
	mzc --exe-dir regiment_pltscript regiment_pltscript.exe 

nc: pltnc
pltnc:  mzscheme_native
native: mzscheme_native
mzscheme_native: 
	find -name ".__*.ss" | xargs -i rm {}  ## So they don't break our compilation.
	(export PLTCOLLECTS=`pwd`:$(PLTLIB); mzc $(MZCFLAGS) --collection-extension .)

# building to native code with a collection isn't working:
nc2:
	export PLTCOLLECTS=`pwd`:$(PLTLIB)
	cat .temp_plt-source-files  | xargs -n1 mzc $(MZCFLAGS) --auto-dir

# only works in PLT right now:
wsparse: 
	mzc --exe bin/wsparse plt/wsparse.ss
# I tried embedding mzscheme to build a stand-alone... but I don't
# think it was really standalone.

wsparse_full:
	mzc $(MZCFLAGS) --exe bin/wsparse plt/wsparse.ss
	mzc $(MZCFLAGS) --exe-dir wsparse_full bin/wsparse 

#----------------------------------------

# This compiles the source into loadable object files
chez: chez_native
chez_native:
	if ! [ -d build/$(MT) ]; then mkdir build/$(MT); fi
	if [ "$(REGDEBUGMODE)" == "ON" ]; \
        then echo '(compile-file "main_chez.ss" (format "build/~a/main_chez_DBG.so" (machine-type)))' | \
             (export REGIMENT_OR_WAVESCRIPT=BOTH; $(CHEZ)); \
        else echo '(compile-file "main_chez.ss" (format "build/~a/main_chez.so" (machine-type)))' | \
             (export REGIMENT_OR_WAVESCRIPT=BOTH; $(CHEZ)); fi

# [2007.04.22] Switching the default to .boot files:

ws:       boot
ws.opt:   boot.opt
opt:      boot.opt
ws.debug: so.debug
debug:    so.debug
dbg:      so.debug

# This makes a Chez Scheme .boot file instead of a shared object file.
boot: _temp_temp.so shared
	echo '(make-boot-header (format "build/~a/main.hdr" (machine-type)) "scheme.boot" "petite.boot")' | $(CHEZ)
	cat "build/$(MT)/main.hdr" "_temp_temp.so" "build/$(MT)/main_chez.so" > "build/$(MT)/main_chez.boot" 
	rm _temp_temp.ss _temp_temp.so

boot.opt: _temp_temp.so shared.opt 
	echo '(make-boot-header (format "build/~a/main_opt.hdr" (machine-type)) "scheme.boot" "petite.boot")' | $(CHEZ)
	cat "build/$(MT)/main_opt.hdr" "_temp_temp.so" "build/$(MT)/main_chez_OPT.so" > "build/$(MT)/main_chez_OPT.boot" 
	rm _temp_temp.ss _temp_temp.so

_temp_temp.so:
	echo '(define regiment-origin "compiled .boot")' > _temp_temp.ss
	echo '(compile-file "_temp_temp.ss" "_temp_temp.so")' | $(CHEZ)


so: shared
shared: 
	if ! [ -d "build/$(MT)" ]; then mkdir build/$(MT); fi
	export REGIMENT_OR_WAVESCRIPT=WS; 
	echo '(generate-inspector-information #f) (compile-file "main_chez.ss" (format "build/~a/main_chez.so" (machine-type)))' | \
         (export REGOPTLVL=2; \
          export REGDEBUGMODE=OFF; \
          export REGIMENT_OR_WAVESCRIPT=WS; \
          $(CHEZ))

so.debug: shared.debug
shared.debug:
	if ! [ -d build/$(MT) ]; then mkdir build/$(MT); fi
	export REGIMENT_OR_WAVESCRIPT=WS; 
	echo '(compile-file "main_chez.ss" (format "build/~a/main_chez_DBG.so" (machine-type)))' | \
         (export REGOPTLVL=2; \
          export REGDEBUGMODE=ON; \
          export REGIMENT_OR_WAVESCRIPT=WS; \
          $(CHEZ))

so.opt: shared.opt
shared.opt:
	MT=`echo '(machine-type)' | ../depends/petite`
	if ! [ -d build/$(MT) ]; then mkdir build/$(MT); fi	
	echo '(compile-file "main_chez.ss" (format "build/~a/main_chez_OPT.so" (machine-type)))' | \
          (export REGOPTLVL=3; \
           export REGDEBUGMODE=OFF; \
           export REGIMENT_OR_WAVESCRIPT=WS; \
           $(CHEZ))


ws.threaded: threaded
threaded:
#	if ! [ -d build/$(CMT) ]; then mkdir build/$(CMT); fi
	export REGIMENT_OR_WAVESCRIPT=WS; 
	echo '(compile-file "main_chez.ss" (format "build/~a/main_chez.so" (machine-type)))' | \
         (export REGOPTLVL=2; \
          export REGDEBUGMODE=OFF; \
          export REGIMENT_OR_WAVESCRIPT=WS; \
          chez_threaded)

# Saved heap images start up very fast, but don't work right on all platforms.
# (doesn't work right on Mac OS or fedora core)

# Run "make chez" before this to compile the code before grabbing a Petite heap:
heap: builddir 
	depends/petite -s0 build/$(MT)/regiment.0.heap --script regiment.ss ./
#	echo '(load "regiment.ss")' | chez -s0 build/$(MT)/regiment.0.heap
#	$(RM) -f build/regiment.0.heap 
#	ln -s build/$(MT)/regiment.0.heap build/regiment.0.heap

# A fullheap includes the Chez compiler as well as the petite heap.
fullheap: builddir
	$(CHEZ) -s0 build/$(MT)/regiment.0.heap --script regiment.ss ./;
#	@if [ `which chez` ]; \
#	echo "$(CHEZ) -s0 build/$(MT)/regiment.0.heap --script regiment.ss ./"; \
#	     $(CHEZ) -s0 build/$(MT)/regiment.0.heap --script regiment.ss ./; \
#	     $(RM) -f build/regiment.0.heap; \
#	     ln -s build/$(MT)/regiment.0.heap build/regiment.0.heap; \
#	else echo "Cannot build fullheap without full Chez Scheme (put 'chez' in path)"; \
#	fi

##	echo '(load "regiment.ss")' | chez -s0 build/$(MT)/regiment.0.heap

fullheap.opt:
	(export REGOPTLVL=3; \
         export REGDEBUGMODE=OFF; \
	$(CHEZ) -s0 build/$(MT)/regiment.0.OPT.heap --script regiment.ss ./; )

builddir:
	if [ ! -d build/$(MT) ]; then mkdir build/$(MT); fi


# This is basic, for someone who just downloaded WScript:
t: test
test:
	rm -rf /tmp/wsparse_server_*
	$(MAKE) clean 
	$(MAKE) wsparse
	regiment test
	(cd demos/wavescope; $(MAKE) getdata)
	(cd demos/wavescope; ./testall_demos.ss)

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
	(cd demos/wavescope; ./testall_demos.ss)
	@echo
	@echo "Now test libraries (stdlib.ws, matrix.ws)"
	@echo "============================================================"
#	$(MAKE) dbg # Now build DEBUG version to speed up these subsequent tests:
	echo exit | ws ../lib/stdlib_test.ws -exit-error;
	echo exit | ws ../lib/matrix_test.ws -exit-error;
#	cd ../src;
	$(MAKE) bc
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
#*    Running                                                          */
#*---------------------------------------------------------------------*/

run: runplt

runchez: 
	$(CHEZ) $(CHEZ_MAIN_FILE)

runplt: 
	$(MZSCHEME) -e "(load/use-compiled \"$(PLT_MAIN_FILE)\")"


#mred:
#	$(MRED) -f $(PLT_MAIN_FILE) -e "(require compiler)"
#swl:
#	$(SWL) $(CHEZ_MAIN_FILE)

#/*--------------------------------------------------------------------*/
#*    Installation Packages                                            */
#*---------------------------------------------------------------------*/

# This makes three separate tarfile packages that must be unzipped in the same dir.
OLDPKGDIR=~/build/regiment
old_package: clean
	@echo
	@echo PACKAGING REGIMENT/WAVESCRIPT
	@echo =================================================================
	rm -rf $(OLDPKGDIR)
	mkdir $(OLDPKGDIR)
	mkdir $(OLDPKGDIR)/base
	cp -pr $(REGIMENTD)/* $(OLDPKGDIR)/base/
	$(MAKE) heap
	$(MAKE) chez
# Now clean that out a little:
	rm -f $(OLDPKGDIR)/base/depends
	rm -rf $(OLDPKGDIR)/base/old_passes
	rm -rf $(OLDPKGDIR)/base/plots
	rm -rf $(OLDPKGDIR)/base/haskell/assembler
	find $(OLDPKGDIR)/base -name "*.svn" | xargs rm -rf 
	find $(OLDPKGDIR)/base -name "*~" | xargs rm -rf 
	find $(OLDPKGDIR)/base -name "_*" | xargs rm -rf 
	@echo
	@echo BUILDING TARFILES
	@echo =================================================================
	(cd $(OLDPKGDIR)/base; tar cjf ../regiment_src.tbz *)
#	rm $(OLDPKGDIR)/base
	(cd $(REGIMENTD); tar cvjf $(OLDPKGDIR)/regiment_heap.tbz build/regiment.0.heap)
	mkdir $(OLDPKGDIR)/depends
	(cd $(REGIMENTD)/; cp -pr depends/* $(OLDPKGDIR)/depends/)
	(cd $(OLDPKGDIR); tar cjf regiment_depends.tbz depends)
	(cd $(REGIMENTD)/; tar cvjf $(OLDPKGDIR)/regiment_object.tbz main_chez.so)


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





#/*--------------------------------------------------------------------*/
#*    Cleaning                                                         */
#*---------------------------------------------------------------------*/
#.PHONY: clean

cleanheap:
	$(RM) build/regiment.0.heap
	$(RM) build/i3nt/regiment.0.heap build/i3le/regiment.0.heap build/ppcosx/regiment.0.heap

# Clean the compiler stuff but not wsparse.
cleanchez: cleancomp
cleancomp:
	$(RM) build/i3nt/* build/i3le/* build/ppcosx/* build/i3osx/* build/ti3osx/* build/ti3le/*


cleanplt:
	rm -rf ./compiled
	rm -rf ./plt/compiled
	rm -rf ./generic/compiled
	rm -rf ./generic/compiler_components/compiled
	rm -rf ./generic/grammars/compiled
	rm -rf ./generic/passes/compiled
	rm -rf ./generic/passes/deglobalize/compiled
	rm -rf ./generic/passes/nesc_bkend/compiled
	rm -rf ./generic/passes/normalize_query/compiled
	rm -rf ./generic/passes/normalize_source/compiled
	rm -rf ./generic/passes/static_elaborate/compiled
	rm -rf ./generic/passes/tokmac_bkend/compiled
	rm -rf ./generic/passes/wavescope_bkend/compiled
	rm -rf ./generic/sim/compiled
	rm -rf ./generic/util/compiled
	find -name *.zo | xargs -i rm {}  ## <- Just to be sure

clean: cleanchez cleanplt
	(cd C; $(MAKE) clean)
	(cd demos/token_machs; $(MAKE) clean)
	(cd demos/regiment; $(MAKE) clean)
	(cd generic/passes/ocaml_bkend/; $(MAKE) clean)
	$(RM) *.zo *.so _SIM_* *~ 
	$(RM) __temp.log _genned_node_code.ss
	find -name _genned_node_code.ss | xargs $(RM)
	$(RM) bin/wsparse
	find -name ".__*" | xargs -i $(RM) {}  ## Remove these temp files
	find -name "__lang_running.tmp.ss" | xargs -i $(RM) {}  ## Remove these temp files

distclean:
	$(RM) -f *.log


upload:
	scp build/$(MT)/main_chez.so "newton@faith.ffh.us:/var/www/regiment_binaries/"$(SVNREV)"_"$(MT)"_main_chez.so"
	ssh newton@faith.ffh.us chgrp www-data "/var/www/regiment_binaries/"$(SVNREV)"_"$(MT)"_main_chez.so"
	ssh newton@faith.ffh.us chmod g+r "/var/www/regiment_binaries/"$(SVNREV)"_"$(MT)"_main_chez.so"
	scp bin/wsparse "newton@faith.ffh.us:/var/www/regiment_binaries/"$(SVNREV)"_"$(MT)"_wsparse"
	ssh newton@faith.ffh.us chgrp www-data "/var/www/regiment_binaries/"$(SVNREV)"_"$(MT)"_wsparse"
	ssh newton@faith.ffh.us chmod g+r "/var/www/regiment_binaries/"$(SVNREV)"_"$(MT)"_wsparse"


#	find -name "*.zo" | xargs rm # This is excessive...
#	(cd compiled; $(MAKE) clean)


#	find . \( -name '*[~%]' \
#                       -o -name '.??*[~%]' \
#                       -o -name '#*#' \
#                       -o -name '?*#' \
#                     -type f -exec $(RM) {} \;   

#*---------------------------------------------------------------------*/
#*    Suffixes                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .ss .so 

#*---------------------------------------------------------------------*/
#*    .ss.so                                                             */
#*---------------------------------------------------------------------*/
.ss.so:
	mzc $(MZCFLAGS) --extension --destination compiled/native/ppc-darwin  $*.ss

#	$(MZC) $*.ss

#*---------------------------------------------------------------------*/
#*    .scm.o                                                           */
#*---------------------------------------------------------------------*/
#.scm.o:
#	$(BIGLOO) -c $(BFLAGS) $*.scm -o $*.o


wc:        wordcount
count:     wordcount
linecount: wordcount
wordcount:
	find -name "*.ss" | grep -v "/parser/" | grep -v "wavescript_temp" | xargs wc 
	@echo;echo
	find -name "*.rs" | xargs wc 
	@echo;echo
	find -name "*.tm" | xargs wc 
	@echo;echo
#	cd ../;
	find ../ -name "*.ws" | xargs wc 

#	ls *.ss generic/*.ss chez/*.ss plt/*.ss generic/*.tests haskell/*.hs demos/token_machs/*.tm demos/regiment/*.rs| grep -v "^_" | xargs wc


wcnb:
	sh -c "ls *.ss generic/*.ss chez/*.ss plt/*.ss generic/*.tests haskell/*.hs | grep -v '^_' | xargs cat | grep -v '^[ ]*$'"

sortcount:
	ls -rS *.ss generic/*.ss chez/*.ss plt/*.ss generic/*.tests haskell/*.hs | xargs wc

go:
	(cd plt/compiled; make; cd ~/cur; make;) #cd plt;  mred -Z -f temp.ss)
#	(cd plt/compiled; make; cd ~/cur; make; cd plt; mred -z -f temp.ss)

exec:
	 (cd plt; mred -z -f temp.ss)


