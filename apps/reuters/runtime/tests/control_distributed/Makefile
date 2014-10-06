

# Try "make" then "make run" to execute two processes on the same machine.

# In its current fragile state the WS socket library requires that
# producers (servers) be started before consumers (clients).

################################################################################

MACHINE = $(shell $(WAVESCRIPTD)/depends/get_machine_type)

UNAME = $(shell uname -s)
LIBS = -lm -ldl -lncurses -lpthread 
#LIBS = -lm -ldl -lpthread 

# Debug mode for now:
FLAGS= -g -O0 -rdynamic
# FLAGS= -O3 

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

#all: proc1 proc2 
all: queries

queries: proc1query.exe proc2query.exe

proc1: proc1.c port.h
	gcc $(FLAGS) -c -I $(CHEZP) proc1.c 
	gcc $(FLAGS) $(LIBS) proc1.o ../../libwsq_runtime.a -o proc1

proc2: proc2.c port.h
	gcc $(FLAGS) -c -I $(CHEZP) proc2.c 
	gcc $(FLAGS) $(LIBS) proc2.o ../../libwsq_runtime.a -o proc2


# Because we run in "paused" mode we can run the queries as separate executables:
proc1query.exe: proc1 
	@echo; echo "Building query1."
	@echo "================================================================================"
	WS_SOCKET_ALLOWDROP=0  WSQ_VERBOSE=1 ./proc1

proc2query.exe: proc2 
	@echo; echo "Building query2."
	@echo "================================================================================"
	WS_SOCKET_ALLOWDROP=0  WSQ_VERBOSE=1 ./proc2
#	(WS_SOCKET_ALLOWDROP=0  WSQ_VERBOSE=1 ./proc2; echo "proc2 done.  TEMP: Ignoring stack smashing error on shutdown for now.")


#	 (./proc1 &) ; sleep 5; ./proc2 | head -n 60

run: queries
	@echo; echo "Query executables created, now running."
	@echo "================================================================================"
	(./proc1query.exe &) ; ./proc2query.exe | head -n 30 | tee ../9_example_distributed.out

# The socket layer should be tolerant to running the queries in either order.
run1: proc1query.exe proc2query.exe
	 (./proc1query.exe &) ; sleep 1; ./proc2query.exe | head -n 60
run2: proc1query.exe proc2query.exe
	 (./proc2query.exe &) ; sleep 1; ./proc1query.exe | head -n 60


clean:
	rm -rf *.o *.a *.exe a.out *.so *proc1 *proc2 query.* *.dSYM *.out


