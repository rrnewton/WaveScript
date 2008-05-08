
PKGNAME=fftw2
PKGVERSION=0.2

CC=gcc
CC_FLAGS = -fPIC
## Caml C header files location:
OCAMLH=$(shell ocamlc -where)/caml
## Caml binaries:
OCAMLC	  = ocamlc
OCAMLOPT  = ocamlopt
OCAMLDEP  = ocamldep
OCAMLDOC  = ocamldoc
OCAMLFIND = ocamlfind
OCAMLC_FLAGS   = -g -dtypes
OCAMLOPT_FLAGS = -dtypes

INCLUDE_FFTW	= /usr/include/fftw/ ## !!!
LIBFFTW		= -cclib -lfftw -cclib -lrfftw -cclib -lm
OCAML_FFTW_PATH	= $(shell pwd)
# OCAML_FFTW_PATH = /usr/lib/ocaml/$(shell $(OCAMLC) -version)/stublibs/

DISTFILES = INSTALL LICENSE META.in Makefile \
  fftw64.ml fftw64.mli fftw-float-type.c fftw2_stub.c \
  test.ml

PKG_TARBALL = $(PKGNAME)-$(PKGVERSION).tar.gz

.PHONY: default byte opt
default: byte opt
byte: fftw2.cma  fftw.cma
opt: fftw2.cmxa  fftw.cmxa

fftw2.cma: fftw2.cmo fftw2.cmi fftw2_stub.o
	$(OCAMLC) -a -o $@ $(OCAMLC_FLAGS) -custom \
	  $(OCAML_FFTW_PATH)/fftw2_stub.o $(OCAML_FFTW_PATH)/$< \
	  $(LIBOCAMLRUN) $(LIBFFTW)


fftw2.cmxa: fftw2.cmx fftw2.cmi fftw2_stub.o
	$(OCAMLOPT) -a -o $@ $(OCAMLOPT_FLAGS) $< $(LIBFFTW) \
		-ccopt -L$(OCAML_FFTW_PATH) -cclib -lfftw2_stub
	$(CC) -shared -o fftw2_stub.so $(CC_FLAGS) fftw2_stub.o -lfftw -lm
	ar rc libfftw2_stub.a fftw2_stub.o

# Deprecated versions:
fftw.cma: fftw2.cma fftw.cmi
	cp $< $@
fftw.cmxa: fftw2.cmxa fftw.cmi
	cp $< $@
	cp fftw2.a fftw.a
fftw.mli: fftw2.mli
	cp $< $@


doc: fftw2.mli fftw2.cmi
	[ -d $@ ] || mkdir $@
	$(OCAMLDOC) -d $@ -stars -html -colorize-code $<


fftw2.ml: fftw64.ml fftw64.mli fftw-float-type
## Check the float type of FFTW and modif the ML files accordingly:
	if [ "`./fftw-float-type`" -eq "32" ]; then \
		sed -e "s/float64/float32/" fftw64.ml | \
		sed -e "s/complex64/complex32/" > $@; \
	else \
		cp -f fftw64.ml $@; \
	fi

fftw2.mli: fftw64.ml fftw64.mli fftw-float-type
	if [ "`./fftw-float-type`" -eq "32" ]; then \
		sed -e "s/float64/float32/" fftw64.mli | \
		sed -e "s/complex64/complex32/" > $@; \
	else \
		cp -f fftw64.mli $@; \
	fi

fftw2-float-typexb: fftw-float-type.c
	$(CC) -o $@ $(CC_FLAGS) -I $(OCAMLH) $<


fftw2_stub.o: fftw2_stub.c
	$(CC) -c $(CC_FLAGS) -I $(OCAMLH) \
	  -DCAML_FFTW_VERSION='"$(PKGVERSION)"' $<


META: META.in
	echo "version = \"$(PKGVERSION)\"" > $@
	cat $^ >> $@


.PHONY: install uninstall
install: byte opt META
	$(OCAMLFIND) install fftw2  META libfftw2_stub.a \
	  fftw2.cmxa fftw2.a fftw2.cma fftw2.cmi fftw2.mli \
# Transitional, deprecated:
	$(OCAMLFIND) install fftw  META \
	  fftw.cmxa fftw.a fftw.cma fftw.cmi fftw.mli

uninstall:
	$(OCAMLFIND) remove fftw2
	$(OCAMLFIND) remove fftw


.PHONY: dist
TARBALL_DIR=$(PKGNAME)-$(PKGVERSION)
dist: $(DISTFILES)
	mkdir $(TARBALL_DIR)
	cp --preserve -r $(DISTFILES) $(TARBALL_DIR)/
	tar -zcvf $(PKG_TARBALL) $(TARBALL_DIR)
	rm -rf $(TARBALL_DIR)

.PHONY: pub
pub: clean
	cd ..; \
	rsync -avrz fftw2/ troestle@ftp.umh.ac.be:ftp_san/bzr/fftw2/


########################################################################

.PHONY: test
test: test.exe test.com

test.exe: fftw.cma test.ml
	$(OCAMLC) -o $@ $(OCAMLC_FLAGS) bigarray.cma $^

test.com: fftw.cmxa test.ml
	$(OCAMLOPT) -o $@ $(OCAMLOPT_FLAGS) bigarray.cmxa $^

########################################################################

%.cmi: %.mli
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

%.cmo: %.ml
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPT_FLAGS) -c $<

.PHONY: depend
depend: .depend
.depend: $(wildcard *.ml) $(wildcard *.mli)
	$(OCAMLDEP) $^ > .depend

include .depend

.PHONY: clean
clean:
	-rm -f *~ .*~ *.o *.cm[aiox] *.cmxa *.a *.annot
	-rm fftw2.ml fftw2.mli fftw.ml fftw.mli $(PKG_TARBALL)
	find . -type f -perm -u=x -exec rm -f {} \;
	-rm -rf doc/
