#*---------------------------------------------------------------------*/
#*    flags                                                            */
#*---------------------------------------------------------------------*/

BINPATH =`mzscheme -m --eval '(begin (display (build-path "compiled" "native"  (system-library-subpath))) (newline) (exit))'`


all: 
	(cd $(BINPATH); $(MAKE))

clean:
	(cd compiled/native; $(MAKE) clean)



#*--- scm -------------------------------------------------------------*/

#SRC = helpers.ss pass01.ss demo_display.ss
#SRC = pass01.ss

#OBJ = $(SRC:%.ss=$(BINPATH)/%.so)

#all: $(OBJ)
#	echo $(COMP

#all:
#	@echo cpath $(BINPATH)
#	@echo $(OBJ)

#compiled/native/i386-linux/pass01.so: pass01.ss
#	$(MZC) --extension --destination $(BINPATH) pass01.ss

#$(COMPILEDPATH)/demo_display.so: demo_display.ss
#	$(MZC) --extension --destination $(COMPILEDPATH) demo_display.ss

#$(COMPILEDPATH)/helpers.so: helpers.ss
#	$(MZC) --extension --destination $(COMPILEDPATH) helpers.ss


#*---------------------------------------------------------------------*/
#*    Suffixes                                                         */
#*---------------------------------------------------------------------*/
#.SUFFIXES:
#.SUFFIXES: .ss .so 

#*---------------------------------------------------------------------*/
#*    .ss.so                                                             */
#*---------------------------------------------------------------------*/
#.ss.so:
#	mzc --extension --destination compiled/native/ppc-darwin  $*.ss

#*---------------------------------------------------------------------*/
#.scm.o:
#	$(BIGLOO) -c $(BFLAGS) $*.scm -o $*.o

