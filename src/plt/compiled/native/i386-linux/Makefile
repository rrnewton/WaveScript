
MZC = mzc
MZSCHEME = mzscheme

SRCFILES = helpers demo_display pass01
SRC = $(SRCFILES:%=../../../%.ss)
OBJ = $(SRCFILES:%=%.so)

all: $(OBJ)

%.so: ../../../%.ss
	$(MZC) --extension $<

clean:
	@$(RM) -f $(OBJ);
