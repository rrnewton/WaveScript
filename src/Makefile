
#*---------------------------------------------------------------------*/
#*    Objects and sources                                              */
#*---------------------------------------------------------------------*/

# These are the generic scheme files, ideally should be r5rs + match
GENERIC_SCM_SRC = generic/helpers.ss \
      generic/pass00_verify-regiment.ss \
      generic/pass01_rename-var.ss
MAIN_FILE = compiler.ss


PLT_SRC = helpers \
          pass00_verify-regiment           \
          pass01_eta-primitives            \
          pass02_rename-var                \
          pass03_remove-unquoted-constant  \
          pass04_reduce-primitives         \
          pass05_remove-complex-constant   \
          pass06_uncover-free              \
          pass07_lift-letrec               \
          pass08_lift-letrec-body          \
          pass09_remove-complex-opera      \
          pass10_verify-core               \
          pass11_classify-names               \
          pass12_deglobalize               \
          flat_threads basic_graphics      \
          demo_display graphics_     \
          simulator_nought simulator_nought_graphics \
          tests driver 
#          pass09_separate-graph.ss
#          language-mechanism lang00  \


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

CHEZ = chez
#CHEZ = petite
#CHEZ = fullpetite

SWL = swl
DEST = a.out

#*---------------------------------------------------------------------*/
#*   Compiling.                                                        */
#*---------------------------------------------------------------------*/

mzscheme: compiled/compiler.zo compiled/compiler_plt.zo $(GENERIC_SCM_SRC) $(PLT_SRC) $(PLT_MAIN_FILE) 
	(cd plt; $(MAKE) "SRCFILES=$(PLT_SRC)")

native: mzscheme_native

mzscheme_native: $(GENERIC_SCM_SRC) $(CHEZ_SRC) $(CHEZ_MAIN_FILE) compiled/compiler.zo compiled/compiler_plt.zo
	(cd plt; $(MAKE) native "SRCFILES=$(PLT_SRC)")

compiled/compiler.zo:
	$(MZC) -z --auto-dir compiler.ss

compiled/compiler_plt.zo:
	$(MZC) -z --auto-dir $(PLT_MAIN_FILE) 

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

runplt: 
	$(MZSCHEME) -e "(load/use-compiled \"$(PLT_MAIN_FILE)\")"

#/*--------------------------------------------------------------------*/
#*    Cleaning                                                         */
#*---------------------------------------------------------------------*/
#.PHONY: clean

clean:
	(cd compiled; $(MAKE) clean)
	(cd plt; $(MAKE) clean "SRCFILES=$(PLT_SRC)")
	$(RM) *.zo _SIM_*
	find -name "*.zo" | xargs rm # This is excessive...
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

count: wordcount
linecount: wordcount
wordcount:
	wc *.ss generic/*.ss chez/*.ss plt/*.ss generic/*.tests

go:
	(cd plt/compiled; make; cd ~/cur; make;) #cd plt;  mred -Z -f temp.ss)
#	(cd plt/compiled; make; cd ~/cur; make; cd plt; mred -z -f temp.ss)

exec:
	 (cd plt; mred -z -f temp.ss)