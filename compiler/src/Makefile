
#*---------------------------------------------------------------------*/
#*    Objects and sources                                              */
#*---------------------------------------------------------------------*/

# These are the generic scheme files, ideally should be r5rs + match
GENERIC_SCM_SRC = generic/helpers.ss generic/pass01.ss 
MAIN_FILE = compiler.ss

PLT_SRC = helpers tests demo_display pass00 pass01 pass07
PLT_MAIN_FILE = compiler_plt.ss
#PLT_FILES = plt/helpers plt/pass01 plt/demo_display

CHEZ_FILES = generic/helpers chez/pass01 plt/demo_display
CHEZ_SRC = $(PLT_FILES:%=%.ss)
CHEZ_MAIN_FILE = compiler_chez.ss

# All scheme source 
ALL_SCM_SRC = $(PLT_SRC) $(CHEZ_SRC) $(GENERIC_SCM_SRC) $(MAIN_FILE)

#*---------------------------------------------------------------------*/
#*    Commands                                                         */
#*---------------------------------------------------------------------*/
MZC = mzc
MZSCHEME = mzscheme
#CHEZ = chez
#CHEZ = petite
CHEZ = fullpetite
SWL = swl
DEST = a.out

#*---------------------------------------------------------------------*/
#*   Compiling.                                                        */
#*---------------------------------------------------------------------*/

mzscheme: $(GENERIC_SCM_SRC) $(CHEZ_SRC) $(CHEZ_MAIN_FILE)
	(cd plt; $(MAKE) "SRCFILES=$(PLT_SRC)")
	$(MZC) -z --auto-dir $(PLT_MAIN_FILE) 
	$(MZC) -z --auto-dir compiler.ss

#mzc:
#	$(MZC) --gui-exe $(DEST) $(PLT_MAIN_FILE)
#mred:
#	$(MRED) -f $(PLT_MAIN_FILE) -e "(require compiler)"
#swl:
#	$(SWL) $(CHEZ_MAIN_FILE)


#/*--------------------------------------------------------------------*/
#*    Running                                                          */
#*---------------------------------------------------------------------*/

run: runplt

runchez: 
	$(CHEZ) $(CHEZ_MAIN_FILE)

runplt: mzscheme
	$(MZC) -z $(PLT_MAIN_FILE)
	$(MZSCHEME) -f $(PLT_MAIN_FILE)

#/*--------------------------------------------------------------------*/
#*    Cleaning                                                         */
#*---------------------------------------------------------------------*/
.PHONY: clean

clean:
	(cd plt; $(MAKE) clean)
	$(RM) *.zo
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

