
#COMPONENT = TokenMachineRuntime
COMPONENT = TestMachine
#COMPONENT = BasicTMComm

haskell: 
	ghc -fext-core --make assembler -o assembler 
	mv a.out assembler

include Makerules

clean: 
	rm -f TestMachine.h TestMachine.nc TestMachineM.nc
	rm -rf *.o TM.hi TM.hcr Main.hi a.out assembler assembler.hcr build/pc/*

