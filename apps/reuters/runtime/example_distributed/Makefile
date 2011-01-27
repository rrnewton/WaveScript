

# Try "make" then "make run" to execute two processes on the same machine.

# In its current fragile state the WS socket library requires that
# producers (servers) be started before consumers (clients).

################################################################################

MACHINE = $(shell $(REGIMENTD)/depends/get_machine_type)

UNAME = $(shell uname -s)
LIBS = -lm -ldl -lncurses -lpthread 
#LIBS = -lm -ldl -lpthread 

# Debug mode for now:
FLAGS= -g -O0 -rdynamic

ifeq ($(UNAME), Linux)
  LIBS+= -lrt
endif
ifeq ($(UNAME), Darwin)
  LIBS+= -liconv

  # Force 32 bitmode:
  FLAGS += -m32 -L/lib32 -L/usr/lib32
endif

CHEZP = $(CHEZD)/boot/$(MACHINE)

# Perhaps should consider linking these libs statically for portability.
#STATICLIBS= /usr/lib/librt.a /usr/lib/libdl.a

all: proc1 proc2 
#all: gen_proc1 gen_proc2 run_proc1 run_proc2

proc1: proc1.c port.h
	gcc $(FLAGS) -c -I $(CHEZP) proc1.c 
	gcc $(FLAGS) $(LIBS) proc1.o ../libwsq_runtime.a -o proc1

proc2: proc2.c port.h
	gcc $(FLAGS) -c -I $(CHEZP) proc2.c 
	gcc $(FLAGS) $(LIBS) proc2.o ../libwsq_runtime.a -o proc2

run: 
	 (./proc1 &) ; sleep 5; ./proc2 | head -n 60
#	 (./run_proc1 &) ; ./run_proc2 | head    

clean:
	rm -rf *.o *.a *.exe a.out *.so *proc1 *proc2 query.* *.dSYM

