
## NOTE!  This file requires SRCFILES be defined.
##        It is generally called from another makefile.

MZC = mzc
MZSCHEME = mzscheme


SRCFILES = helpers \
          pass00_verify-regiment           \
          pass01_rename-var                \
          pass02_remove-unquoted-constant  \
          pass03_remove-complex-constant   \
          pass04_uncover-free              \
          pass05_lift-letrec               \
          pass06_lift-letrec-body          \
          pass07_remove-complex-opera      \
          pass08_verify-core               \
          pass10_deglobalize               \
          flat_threads basic_graphics      \
          demo_display graphics_stub       \
          simulator_nought simulator_nought_graphics \
          tests driver 


SRC = $(SRCFILES:%=../%.ss)
# Not all of these will necessarily exist, but some will:
#ORIGSRC = $(SRCFILES:%=../../generic/%.ss)
OBJ = $(SRCFILES:%=%.zo)

all: bytecode
#	@echo $(SRC)
#	@echo $(OBJ)

bytecode: $(OBJ)

.SUFFIXES: .ss .zo 

helpers: helpers.zo
helpers.zo: ../helpers.ss
	mzc --destination ./ --zo ../helpers.ss

flat_threads.zo: ../flat_threads.ss
	mzc --destination ./ --zo ../flat_threads.ss

# NOTE: This makes it depend on the generic version also!!  
# BUT I might be missing a dependency or two, thanks to including more
# than one file in some PLT modules.
%.zo: ../%.ss ../../generic/%.ss
	$(MZC) --destination ./ --zo $< 

clean:
	@$(RM) -f $(OBJ);

