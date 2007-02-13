

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
MZC = mzc
MZSCHEME = mzscheme

CHEZ = `if (which chez > /dev/null); then echo chez; else echo ../depends/chez; fi`

#CHEZ = petite
#CHEZ = fullpetite

SWL = swl
DEST = a.out

# This builds the .so.  Run "make heap/fullheap" separately.
all: 
	@echo; echo BUILDING PARSER
	@echo "======================================================================"
	$(MAKE) wsparse
	@echo; echo; echo Compiling Regiment Scheme Source
	@echo "======================================================================"
	$(MAKE) chez
	@echo; echo; echo [OPT] Compiling Regiment Scheme Source
	@echo "======================================================================"
	$(MAKE) opt
# 	@echo; echo; echo BUILDING C-code extensions.
# 	@echo "======================================================================"
# 	@echo "Don't worry if this step fails, it's optional!"
# 	@echo "======================================================================"
# 	$(MAKE) c

old_all: reg
	(cd plt; $(MAKE) nc)
	(cd haskell; $(MAKE))

MT=`echo '(display (machine-type))' | ../depends/petite -q`
SVNREV=`svn info | grep Revision | sed s/Revision:\ //`

#*---------------------------------------------------------------------*/
#*   Compiling.                                                        */
#*---------------------------------------------------------------------*/

c: 
	(cd C; $(MAKE))

# Two scheme back-ends, PLT or Chez.

# PLT: 
#----------------------------------------
bc: plt
plt: pltbc
pltbc: mzscheme
mzc: mzscheme
mzscheme: 
	find -name ".__*.ss" | xargs -i rm {}  ## So they don't break our compilation.
	find -name "_*.ss" | xargs -i rm {}  ## So they don't break our compilation.	
	(export PLTCOLLECTS=`pwd`:$(PLTLIB); mzc --collection-zos .)

nc: pltnc
pltnc:  mzscheme_native
native: mzscheme_native
mzscheme_native: 
	find -name ".__*.ss" | xargs -i rm {}  ## So they don't break our compilation.
	(export PLTCOLLECTS=`pwd`:$(PLTLIB); mzc --collection-extension .)

# building to native code with a collection isn't working:
nc2:
	export PLTCOLLECTS=`pwd`:$(PLTLIB)
	cat .temp_plt-source-files  | xargs -n1 mzc --auto-dir

# only works in PLT right now:
wsparse: 
	mzc --exe bin/wsparse plt/wsparse.ss

#----------------------------------------

# This compiles the source into loadable object files
chez: chez_native
chez_native:
	MT=`echo '(machine-type)' | petite`
	if ! [ -d build/$(MT) ]; then mkdir build/$(MT); fi
	echo '(compile-file "main_chez.ss" (format "build/~a/main_chez.so" (machine-type)))' | $(CHEZ)

ws.opt: opt
opt:
	MT=`echo '(machine-type)' | petite`
	if ! [ -d build/$(MT) ]; then mkdir build/$(MT); fi	
	export REGOPTLVL=3; 
	export REGDEBUGMODE=OFF;
	echo '(compile-file "main_chez.ss" (format "build/~a/main_chez_OPT.so" (machine-type)))' | $(CHEZ)

# Saved heap images start up very fast, but don't work right on all platforms.
# (doesn't work right on Mac OS or fedora core)

# Run "make chez" before this to compile the code before grabbing a Petite heap:
heap: builddir chez
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

builddir:
	if [ ! -d build/$(MT) ]; then mkdir build/$(MT); fi

# This runs various tests but tries not to be fairly quick.
# This is in contrast to supertest.ss, which is supposed to be as
# exhaustive as possible.
t: test
test: 
	$(MAKE) clean 
	$(MAKE) wsparse
	regiment test
	(cd ../apps/pipeline-web/; $(MAKE) test)
	(cd demos/wavescope; ./testall_demos.ss)
	echo exit | ws lib/stdlib.ws;
	echo exit | ws lib/matrix.ws;
	$(MAKE) chez
	(cd demos/wavescope; ./testall_wsc)
#	./regiment_script.ss test


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
PKGDIR=~/build/regiment
package: clean
	@echo
	@echo PACKAGING REGIMENT
	@echo =================================================================
	rm -rf $(PKGDIR)
	mkdir $(PKGDIR)
	mkdir $(PKGDIR)/base
	cp -pr $(REGIMENTD)/* $(PKGDIR)/base/
	$(MAKE) heap
	$(MAKE) chez
# Now clean that out a little:
	rm -f $(PKGDIR)/base/depends
	rm -rf $(PKGDIR)/base/old_passes
	rm -rf $(PKGDIR)/base/plots
	rm -rf $(PKGDIR)/base/haskell/assembler
	find $(PKGDIR)/base -name "*.svn" | xargs rm -rf 
	find $(PKGDIR)/base -name "*~" | xargs rm -rf 
	find $(PKGDIR)/base -name "_*" | xargs rm -rf 
	@echo
	@echo BUILDING TARFILES
	@echo =================================================================
	(cd $(PKGDIR)/base; tar cjf ../regiment_src.tbz *)
#	rm $(PKGDIR)/base
	(cd $(REGIMENTD); tar cvjf $(PKGDIR)/regiment_heap.tbz build/regiment.0.heap)
	mkdir $(PKGDIR)/depends
	(cd $(REGIMENTD)/; cp -pr depends/* $(PKGDIR)/depends/)
	(cd $(PKGDIR); tar cjf regiment_depends.tbz depends)
	(cd $(REGIMENTD)/; tar cvjf $(PKGDIR)/regiment_object.tbz main_chez.so)


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
	$(RM) build/i3nt/* build/i3le/* build/ppcosx/*


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
	$(RM) *.zo *.so _SIM_* *~ 
	$(RM) __temp.log _genned_node_code.ss
	find -name _genned_node_code.ss | xargs $(RM)
	$(RM) bin/wsparse
	find -name ".__*" | xargs -i $(RM) {}  ## Remove these temp files
	find -name "__lang_running.tmp.ss" | xargs -i $(RM) {}  ## Remove these temp files

distclean:
	$(RM) -f *.log


# upload:
	scp build/$(MT)/main_chez.so "newton@faith.ffh.us:/var/www/regiment_binaries/"$(SVNREV)"_"$(MT)"_main_chez.so"
	ssh newton@faith.ffh.us chgrp www-data "/var/www/regiment_binaries/"$(SVNREV)"_"$(MT)"_main_chez.so"
	ssh newton@faith.ffh.us chmod g+r "/var/www/regiment_binaries/"$(SVNREV)"_"$(MT)"_main_chez.so"
# 	cp bin/wsparse "/var/www/regiment_binaries/"$(SVNREV)"_"$(MT)"_wsparse"
# 	chgrp www-data "/var/www/regiment_binaries/"$(SVNREV)"_"$(MT)"_wsparse"
# 	chmod g+r "/var/www/regiment_binaries/"$(SVNREV)"_"$(MT)"_wsparse"


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
	mzc --extension --destination compiled/native/ppc-darwin  $*.ss

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
	ls *.ss generic/*.ss chez/*.ss plt/*.ss generic/*.tests haskell/*.hs demos/token_machs/*.tm demos/regiment/*.rs| grep -v "^_" | xargs wc

wcnb:
	sh -c "ls *.ss generic/*.ss chez/*.ss plt/*.ss generic/*.tests haskell/*.hs | grep -v '^_' | xargs cat | grep -v '^[ ]*$'"

sortcount:
	ls -rS *.ss generic/*.ss chez/*.ss plt/*.ss generic/*.tests haskell/*.hs | xargs wc

go:
	(cd plt/compiled; make; cd ~/cur; make;) #cd plt;  mred -Z -f temp.ss)
#	(cd plt/compiled; make; cd ~/cur; make; cd plt; mred -z -f temp.ss)

exec:
	 (cd plt; mred -z -f temp.ss)


