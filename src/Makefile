
#*---------------------------------------------------------------------*/
#*    flags                                                            */
#*---------------------------------------------------------------------*/
MZC = mzc
MZSCHEME = mzscheme
#CHEZ = chez
#CHEZ = petite
CHEZ = fullpetite
SWL = swl
DEST = a.out

#*---------------------------------------------------------------------*/
#*    Objects and sources                                              */
#*---------------------------------------------------------------------*/

#*--- scm -------------------------------------------------------------*/

# These are the generic scheme files, ideally should be r5rs + match
GENERIC_SCM_SRC = generic/helpers.ss generic/pass01.ss
MAIN_FILE = compiler.ss

#PLT_FILES = plt/helpers plt/pass01 plt/demo_display
#PLT_OBJ		= $(PLT_FILES:%=%.so)
#PLT_SRC		= $(PLT_OBJ:%.so=%.ss)
PLT_MAIN_FILE = compiler_plt.ss
#SRCFILES = helpers demo_display pass00 pass01 pass07

CHEZ_FILES = generic/helpers chez/pass01 plt/demo_display
CHEZ_SRC = $(PLT_FILES:%=%.ss)
CHEZ_MAIN_FILE = compiler_chez.ss

# All scheme source 
ALL_SCM_SRC = $(PLT_SRC) $(CHEZ_SRC) $(GENERIC_SCM_SRC) $(MAIN_FILE)

#*---------------------------------------------------------------------*/
#*    All objects and sources                                          */
#*---------------------------------------------------------------------*/
#OBJ		= $(SCM)
#SRC		= $(SCM_SRC)

#*---------------------------------------------------------------------*/
#*   Compiling.                                                        */
#*---------------------------------------------------------------------*/

mzscheme:
	(cd plt; $(MAKE))

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

runchez: $(GENERIC_SCM_SRC) $(CHEZ_SRC) $(CHEZ_MAIN_FILE)
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

