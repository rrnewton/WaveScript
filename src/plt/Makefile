#*---------------------------------------------------------------------*/
#*    flags                                                            */
#*---------------------------------------------------------------------*/

BINPATH =`mzscheme -m --eval '(begin (display (build-path "compiled" "native"  (system-library-subpath))) (newline) (exit))'`

SRCFILES = helpers demo_display pass00 pass01 pass07

#all: native bytecode

bytecode:
	(cd compiled; $(MAKE) "SRCFILES=$(SRCFILES)")

native:
	(cd $(BINPATH); $(MAKE))

clean:
	(cd compiled; $(MAKE) "SRCFILES=$(SRCFILES)" clean)
	(cd compiled/native; $(MAKE) "SRCFILES=$(SRCFILES)" clean)
