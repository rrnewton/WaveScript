
## NOTE!  This file requires SRCFILES be defined.
##        It is generally called from another makefile.

MZC = mzc
MZSCHEME = mzscheme

SRC = $(SRCFILES:%=../%.ss)
# Not all of these will necessarily exist, but some will:
ORIGSRC = $(SRCFILES:%=../../generic/%.ss)
OBJ = $(SRCFILES:%=%.zo)

all: bytecode

bytecode: $(OBJ)

%.zo: ../%.ss ../../generic/%.ss
	$(MZC) --destination ./ --zo $< 

clean:
	@$(RM) -f $(OBJ);

