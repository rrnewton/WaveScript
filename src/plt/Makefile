#*---------------------------------------------------------------------*/
#*    flags                                                            */
#*---------------------------------------------------------------------*/

BINPATH =`mzscheme -m --eval '(begin (display (build-path "compiled" "native"  (system-library-subpath))) (newline) (exit))'`

#all: native bytecode

all: compiled/constants.zo compiled/flat_threads.zo compiled/graphics_stub.zo compiled/helpers.zo \
     compiled/simulator_nought.zo \
     compiled/simulator_nought_graphics.zo \
     compiled/pass00_verify-regiment.zo \
     compiled/pass01_eta-primitives.zo \
     compiled/pass02_rename-vars.zo \
     compiled/pass03_remove-unquoted-constant.zo \
     compiled/pass04_reduce-primitives.zo \
     compiled/pass05_remove-complex-constant.zo \
     compiled/pass06_uncover-free.zo \
     compiled/pass07_lift-letrec.zo \
     compiled/pass08_lift-letrec-body.zo \
     compiled/pass09_remove-complex-opera.zo \
     compiled/pass10_verify-core.zo \
     compiled/pass11_classify-names.zo \
     compiled/pass13_deglobalize.zo 


compiled/constants.zo: constants.ss ../generic/constants.ss
	mzc --auto-dir --zo constants.ss
#	mzc --auto-dir  constants.ss

compiled/flat_threads.zo: flat_threads.ss
	mzc --auto-dir --zo flat_threads.ss
#	mzc --auto-dir  flat_threads.ss

compiled/helpers.zo: helpers.ss ../generic/helpers.ss
	mzc --auto-dir --zo helpers.ss
#	mzc --auto-dir  helpers.ss

compiled/simulator_nought.zo: simulator_nought.ss ../generic/simulator_nought.ss
	mzc --auto-dir --zo simulator_nought.ss
#	mzc --auto-dir  simulator_nought.ss

compiled/graphics_stub.zo: graphics_stub.ss ../generic/graphics_stub.ss
	mzc --auto-dir --zo graphics_stub.ss
#	mzc --auto-dir  graphics_stub.ss

compiled/simulator_nought_graphics.zo: simulator_nought_graphics.ss ../generic/simulator_nought_graphics.ss
	mzc --auto-dir --zo simulator_nought_graphics.ss
#	mzc --auto-dir  simulator_nought_graphics.ss

compiled/pass00_verify-regiment.zo: pass00_verify-regiment.ss ../generic/pass00_verify-regiment.ss
	mzc --auto-dir --zo pass00_verify-regiment.ss
#	mzc --auto-dir  pass00_verify-regiment.ss
compiled/pass01_eta-primitives.zo: pass01_eta-primitives.ss ../generic/pass01_eta-primitives.ss
	mzc --auto-dir --zo pass01_eta-primitives.ss
#	mzc --auto-dir  pass01_eta-primitives.ss
compiled/pass02_rename-vars.zo: pass02_rename-vars.ss ../generic/pass02_rename-vars.ss
	mzc --auto-dir --zo pass02_rename-vars.ss
#	mzc --auto-dir  pass02_rename-vars.ss
compiled/pass03_remove-unquoted-constant.zo: pass03_remove-unquoted-constant.ss ../generic/pass03_remove-unquoted-constant.ss
	mzc --auto-dir --zo pass03_remove-unquoted-constant.ss
#	mzc --auto-dir  pass03_remove-unquoted-constant.ss
compiled/pass04_reduce-primitives.zo: pass04_reduce-primitives.ss ../generic/pass04_reduce-primitives.ss
	mzc --auto-dir --zo pass04_reduce-primitives.ss
#	mzc --auto-dir  pass04_reduce-primitives.ss
compiled/pass05_remove-complex-constant.zo: pass05_remove-complex-constant.ss ../generic/pass05_remove-complex-constant.ss
	mzc --auto-dir --zo pass05_remove-complex-constant.ss
#	mzc --auto-dir  pass05_remove-complex-constant.ss
compiled/pass06_uncover-free.zo: pass06_uncover-free.ss ../generic/pass06_uncover-free.ss
	mzc --auto-dir --zo pass06_uncover-free.ss
#	mzc --auto-dir  pass06_uncover-free.ss
compiled/pass07_lift-letrec.zo: pass07_lift-letrec.ss ../generic/pass07_lift-letrec.ss
	mzc --auto-dir --zo pass07_lift-letrec.ss
#	mzc --auto-dir  pass07_lift-letrec.ss
compiled/pass08_lift-letrec-body.zo: pass08_lift-letrec-body.ss ../generic/pass08_lift-letrec-body.ss
	mzc --auto-dir --zo pass08_lift-letrec-body.ss
#	mzc --auto-dir  pass08_lift-letrec-body.ss
compiled/pass09_remove-complex-opera.zo: pass09_remove-complex-opera.ss ../generic/pass09_remove-complex-opera.ss
	mzc --auto-dir --zo pass09_remove-complex-opera.ss
#	mzc --auto-dir  pass09_remove-complex-opera.ss
compiled/pass10_verify-core.zo: pass10_verify-core.ss ../generic/pass10_verify-core.ss
	mzc --auto-dir --zo pass10_verify-core.ss
#	mzc --auto-dir  pass10_verify-core.ss
compiled/pass11_classify-names.zo: pass11_classify-names.ss ../generic/pass11_classify-names.ss
	mzc --auto-dir --zo pass11_classify-names.ss
#	mzc --auto-dir  pass11_classify-names.ss

compiled/pass13_deglobalize.zo: pass13_deglobalize.ss ../generic/pass13_deglobalize.ss
	mzc --auto-dir --zo pass13_deglobalize.ss
#	mzc --auto-dir  pass13_deglobalize.ss


#	mzc -c helpers.ss



#bytecode:
#	(cd compiled; $(MAKE) "SRCFILES=$(SRCFILES)")

native:
	(cd $(BINPATH); $(MAKE))


clean: 
	rm -f compiled/*.zo
	rm -rf compiled/native/*

#clean:
#	(cd compiled; $(MAKE) "SRCFILES=$(SRCFILES)" clean)
#	(cd compiled/native; $(MAKE) "SRCFILES=$(SRCFILES)" clean)


helpers:
	(cd compiled; $(MAKE) helpers)
