
#*---------------------------------------------------------------------*/
#*    Objects and sources                                              */
#*---------------------------------------------------------------------*/

CHEZ_MAIN_FILE = compiler_chez.ss
PLT_MAIN_FILE = compiler_plt.ss

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

#*---------------------------------------------------------------------*/
#*   Compiling.                                                        */
#*---------------------------------------------------------------------*/

plt: pltbc
pltbc: mzscheme
mzc: mzscheme
mzscheme: 
	(cd plt; $(MAKE))

pltnc:  mzscheme_native
native: mzscheme_native
mzscheme_native: 
	(cd plt; $(MAKE) nc)

# I don't use a saved heap anymore because it doesn't work right on Mac OS
chez: chez_native
chez_native:
	echo '(compile-file "compiler_chez.ss")' | chez
#build/regimentc.1.heap: regiment.ss $(CHEZ_SRC) $(GENERIC_SRC) $(CHEZ_MAIN_FILE)
#	echo '(load "regiment.ss")' | chez -s1 build/regimentc.1.heap

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
#*    Cleaning                                                         */
#*---------------------------------------------------------------------*/
#.PHONY: clean

clean:
	(cd plt; $(MAKE) clean)
	$(RM) *.zo *.so _SIM_*

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
	ls *.ss generic/*.ss chez/*.ss plt/*.ss generic/*.tests haskell/*.hs | grep -v "^_" | xargs wc

sortcount:
	ls -rS *.ss generic/*.ss chez/*.ss plt/*.ss generic/*.tests haskell/*.hs | xargs wc

go:
	(cd plt/compiled; make; cd ~/cur; make;) #cd plt;  mred -Z -f temp.ss)
#	(cd plt/compiled; make; cd ~/cur; make; cd plt; mred -z -f temp.ss)

exec:
	 (cd plt; mred -z -f temp.ss)
