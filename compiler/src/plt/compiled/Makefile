
## NOTE!  This file requires SRCFILES be defined.
##        It is generally called from another makefile.

MZC = mzc
MZSCHEME = mzscheme

SRC = $(SRCFILES:%=../%.ss)
OBJ = $(SRCFILES:%=%.zo)

all: bytecode

bytecode: $(OBJ)

%.zo: ../%.ss
	$(MZC) --destination ./ --zo $< 

clean:
	@$(RM) -f $(OBJ);

