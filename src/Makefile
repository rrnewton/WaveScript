
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

CHEZ = chez
#CHEZ = petite
#CHEZ = fullpetite

SWL = swl
DEST = a.out

all: reg
	(cd plt; $(MAKE) nc)
	(cd haskell; $(MAKE))

MT = `echo '(display (machine-type))' | petite -q`

#*---------------------------------------------------------------------*/
#*   Compiling.                                                        */
#*---------------------------------------------------------------------*/

# Two back-ends, PLT or Chez.

#----------------------------------------
plt: pltbc
pltbc: mzscheme
mzc: mzscheme
mzscheme: 
	(cd plt; $(MAKE))

pltnc:  mzscheme_native
native: mzscheme_native
mzscheme_native: 
	(cd plt; $(MAKE) nc)

# only works in PLT right now:
wsparse: 
	mzc --exe bin/wsparse wsparse.ss

#----------------------------------------

# This compiles the source into loadable object files
chez: chez_native
chez_native:
	echo '(compile-file "main_chez.ss" (format "build/~a/main_chez.so" (machine-type)))' | chez

# Saved heap images start up very fast, but don't work right on all platforms.
# (doesn't work right on Mac OS or fedora core)
heap: 
	depends/petite -s0 build/$(MT)/regiment.0.heap --script regiment.ss ./
#	echo '(load "regiment.ss")' | chez -s0 build/$(MT)/regiment.0.heap
	$(RM) -f build/regiment.0.heap 
#	ln -s build/$(MT)/regiment.0.heap build/regiment.0.heap

# A fullheap includes the Chez compiler as well as the petite heap.
fullheap:
	scheme -s0 build/$(MT)/regiment.0.heap --script regiment.ss ./
#	echo '(load "regiment.ss")' | chez -s0 build/$(MT)/regiment.0.heap
	$(RM) -f build/regiment.0.heap 
#	ln -s build/$(MT)/regiment.0.heap build/regiment.0.heap



test: clean chez
	./regiment_script.ss test

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

clean:
	(cd plt; $(MAKE) clean)
	(cd haskell; $(MAKE) clean)
	(cd chez/usleep; $(MAKE) clean)
	(cd demos/token_machs; $(MAKE) clean)
	(cd demos/regiment; $(MAKE) clean)
	$(RM) *.zo *.so _SIM_* *~
	$(RM) build/*.heap
	$(RM) build/i3nt/* build/i3le/* build/ppcosx/*
	$(RM) __temp.log _genned_node_code.ss
	find -name _genned_node_code.ss | xargs $(RM)
	$(RM) bin/wsparse

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


