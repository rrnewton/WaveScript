
#*---------------------------------------------------------------------*/
#*    flags                                                            */
#*---------------------------------------------------------------------*/
MZC = mzc
MZSCHEME = mzscheme
MRED = mred
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

PLT_FILES = plt/helpers plt/pass01 plt/demo_display
PLT_OBJ		= $(PLT_FILES:%=%.so)
PLT_SRC		= $(PLT_OBJ:%.so=%.ss)
PLT_MAIN_FILE = compiler_plt.ss

CHEZ_FILES = generic/helpers chez/pass01 plt/demo_display
CHEZ_SRC = $(PLT_FILES:%=%.ss)
CHEZ_MAIN_FILE = compiler_chez.ss

# All scheme source 
ALL_SCM_SRC = $(PLT_SRC) $(CHEZ_SRC) $(GENERIC_SCM_SRC) $(MAIN_FILE)

#*---------------------------------------------------------------------*/
#*    All objects and sources                                          */
#*---------------------------------------------------------------------*/
OBJ		= $(SCM)
SRC		= $(SCM_SRC)

#*---------------------------------------------------------------------*/
#*    the goals.                                                       */
#*---------------------------------------------------------------------*/

petite: $(GENERIC_SCM_SRC) $(CHEZ_SRC) $(CHEZ_MAIN_FILE)
	$(CHEZ) $(CHEZ_MAIN_FILE)


$(DEST): $(GENERIC_SCM_SRC) $(PLT_SRC) # $(SCM_OBJ)
	$(MZC) --gui-exe $(DEST) $(PLT_MAIN_FILE)


mzc:
	$(MZC) --gui-exe $(DEST) $(PLT_MAIN_FILE)

mred:
	$(MRED) -f $(PLT_MAIN_FILE) -e "(require compiler)"

swl:
	$(SWL) $(CHEZ_MAIN_FILE)


#$(DEST): $(OBJ) 
#	$(MZC) --exe $(DEST) $(SCM_OBJ)

#test: $(DEST)
#	./$(DEST)


#/*--------------------------------------------------------------------*/
#*    Cleaning                                                         */
#*---------------------------------------------------------------------*/
.PHONY: clean

clean:
	@ find . \( -name '*[~%]' \
                       -o -name '.??*[~%]' \
                       -o -name '#*#' \
                       -o -name '?*#' \
                       -o -name \*core \) \
                     -type f -exec $(RM) {} \;   
	@- $(RM) -f $(OBJ);
	@- $(RM) -f $(DEST) 

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

