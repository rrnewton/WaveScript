CHEZ_MAIN_FILE = compiler_chez.ss
PLT_MAIN_FILE = compiler_plt.ss
PLT_SRC = plt/pass_check_simple.ss plt/demo_display.ss plt/pass04_reduce-primitives.ss plt/engine.ss plt/pass02_rename-vars.ss plt/pass09_remove-complex-opera.ss plt/pass04_static-elaborate.ss plt/pass11_classify-names.ss plt/pass13_add-control-flow.ss plt/network_topologies.ss plt/constants.ss plt/lang00.ss plt/critical_section.ss plt/define-structure.ss plt/pass15_analyze-places.ss plt/language-mechanism.ss plt/pass12_add-heartbeats.ss plt/pass06_uncover-free.ss plt/basic_graphics.ss plt/temp.ss plt/graphics_stub.ss plt/pass07_lift-letrec.ss plt/pass14_add-places.ss plt/iu-exptime.ss plt/tsort.ss plt/tests.ss plt/pass05_remove-complex-constant.ss plt/test.ss plt/pass00_verify-regiment.ss plt/driver.ss plt/lang10.ss plt/passXX_separate-graph.ss plt/pass12_annotate-heartbeats.ss plt/pass19_haskellize-tokmac.ss plt/pass15_add-routing.ss plt/pass01_eta-primitives.ss plt/pass16_deglobalize.ss plt/simulator_nought.ss plt/simulator_nought_graphics.ss plt/pass03_remove-unquoted-constant.ss plt/flat_threads.ss plt/engines2.ss plt/pass13_addplaces.ss plt/helpers.ss plt/pass10_verify-core.ss plt/pass17_cleanup-token-machine.ss plt/pass08_lift-letrec-body.ss plt/iu-match.ss
CHEZ_SRC = chez/swl_flat_threads.ss chez/demo_display.ss chez/match.ss chez/constants.ss chez/basic_graphics.ss chez/graphics_stub.ss chez/simulator_nought.ss chez/simulator_nought_graphics.ss chez/flat_threads.ss chez/helpers.ss
GENERIC_SRC = compiler.ss generic/pass_check_simple.ss generic/demo_display.ss generic/pass04_reduce-primitives.ss generic/pass02_rename-vars.ss generic/passdraft_propogate-skeletons.ss generic/pass05_verify-stage2.ss generic/pass09_remove-complex-opera.ss generic/pass04_static-elaborate.ss generic/pass11_classify-names.ss generic/tests_slow.ss generic/pass13_add-control-flow.ss generic/rrn-current-test.ss generic/network_topologies.ss generic/passXX_uncover_leaves.ss generic/pass_template_core.ss generic/simulator_nought.examples.ss generic/constants.ss generic/lang07_lift-letrec.ss generic/lang00.ss generic/pass15_analyze-places.ss generic/tests_regiment.ss generic/language-mechanism.ss generic/pass12_add-heartbeats.ss generic/pass18_verify-token-machine.ss generic/lang13_control-flow.ss generic/pass06_uncover-free.ss generic/basic_graphics.ss generic/graphics_stub.ss generic/pass07_lift-letrec.ss generic/repl.ss generic/pass14_add-places.ss generic/tests.ss generic/pass05_remove-complex-constant.ss generic/tests_quick.ss generic/scrap.ss generic/pass_template_post_rename.ss generic/leader_election.ss generic/tests_noclosure.ss generic/lang12_heartbeats.ss generic/pass00_verify-regiment.ss generic/driver.ss generic/tests_medium.ss generic/topsort.ss generic/passXX_separate-graph.ss generic/lang16_deglobalize.ss generic/lang19_haskellize-tokmac.ss generic/pass19_haskellize-tokmac.ss generic/pass18_cps-tokmac.ss generic/pass15_add-routing.ss generic/pass01_eta-primitives.ss generic/pass16_deglobalize.ss generic/simulator_nought.ss generic/simulator_nought_graphics.ss generic/pass03_remove-unquoted-constant.ss generic/lang14_places.ss generic/tests_obj.ss generic/helpers.ss generic/pass10_verify-core.ss generic/lang06_uncover-free.ss generic/tests_new.ss generic/passdraft_classify-connections.ss generic/pass17_cleanup-token-machine.ss generic/pass08_lift-letrec-body.ss generic/analysis.ss generic/lang11_classify-names.ss


#*---------------------------------------------------------------------*/
#*    Objects and sources                                              */
#*---------------------------------------------------------------------*/

# These are the generic scheme files, ideally should be r5rs + match

# MAIN_FILE = compiler.ss

# PLT_MAIN_FILE = compiler_plt.ss
# #PLT_FILES = plt/helpers plt/pass01 plt/demo_display


# CHEZ_FILES = generic/helpers chez/pass01 plt/demo_display
# CHEZ_SRC = $(PLT_FILES:%=%.ss)
# CHEZ_MAIN_FILE = compiler_chez.ss

# # All scheme source 
# ALL_SCM_SRC = $(PLT_SRC) $(CHEZ_SRC) $(GENERIC_SCM_SRC) $(MAIN_FILE)

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

mzscheme: compiled/compiler.zo compiled/compiler_plt.zo $(GENERIC_SRC) $(PLT_SRC) $(PLT_MAIN_FILE)
	(cd plt; $(MAKE) "SRCFILES=$(PLT_SRC)")

native: mzscheme_native

mzscheme_native: $(GENERIC_SCM_SRC) $(CHEZ_SRC) $(CHEZ_MAIN_FILE) compiled/compiler.zo compiled/compiler_plt.zo
	(cd plt; $(MAKE) native "SRCFILES=$(PLT_SRC)")

compiled/compiler.zo:
	$(MZC) -z --auto-dir compiler.ss

compiled/compiler_plt.zo:
	$(MZC) -z --auto-dir $(PLT_MAIN_FILE) 

reg: build/regimentc.1.heap

build/regimentc.1.heap: $(CHEZ_SRC) $(GENERIC_SRC) $(CHEZ_MAIN_FILE)
	echo '(load "regiment.ss")' | chez -s1 build/regimentc.1.heap


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
	wc *.ss generic/*.ss chez/*.ss plt/*.ss generic/*.tests haskell/*.hs

sortcount:
	ls -rS *.ss generic/*.ss chez/*.ss plt/*.ss generic/*.tests haskell/*.hs | xargs wc

go:
	(cd plt/compiled; make; cd ~/cur; make;) #cd plt;  mred -Z -f temp.ss)
#	(cd plt/compiled; make; cd ~/cur; make; cd plt; mred -z -f temp.ss)

exec:
	 (cd plt; mred -z -f temp.ss)
