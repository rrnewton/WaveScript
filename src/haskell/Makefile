

haskell: 
	ghc -fext-core -fwarn-incomplete-patterns --make assembler -o assembler
	cp -pr assembler ../build/assemble_tm

#	ghc -fext-core --make assembler -o assembler 
#	mv a.out assembler

#tinyos: tos
#tos:
#	$(MAKE) -f Makefile.tinyos $*

pc:
	$(MAKE) -f Makefile.tinyos pc

micaz:
	$(MAKE) -f Makefile.tinyos micaz

telos:
	$(MAKE) -f Makefile.tinyos telos


clean: 
	rm -f TestMachine.h TestMachine.nc TestMachineM.nc
	rm -rf *.o TM.hi TM.hcr Main.hi a.out assembler assembler.hcr build/pc/*
	rm -rf *.hi *.hcr 
