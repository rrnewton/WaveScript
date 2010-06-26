
MACHINE = $(shell $(REGIMENTD)/apps/reuters/runtime/chez_machine_type_threaded)

UNAME = $(shell uname -s)
LIBS = -lm -ldl -lncurses -lpthread 
ifeq ($(UNAME), Linux)
  LIBS+= -lrt
endif
ifeq ($(UNAME), Darwin)
  LIBS+= -liconv
endif

# -lwsq_runtime
# -L$(REGIMENTD)/apps/reuters/runtime

CHEZP = $(CHEZD)/boot/$(MACHINE)

DIRS32 = -L/lib32 -L/usr/lib32

ifeq ($(shell uname -m),x86_64)
  FLAGS = -g -O0 -rdynamic
else 
  FLAGS = -g -O0 -m32 -rdynamic $(DIRS32)
endif

# Perhaps should consider linking these libs statically for portability.
#STATICLIBS= /usr/lib/librt.a /usr/lib/libdl.a

CC=gcc

all: libwsq_runtime.a example_main


libwsq_runtime.a: wsq_runtime.c 
	@if [ -d "$(CHEZD)" ]; then echo; else echo "ERROR: CHEZD ($(CHEZD)) does not exist."; exit 1; fi
	gcc $(FLAGS) -c -fPIC -I $(CHEZP) wsq_runtime.c -o wsq_runtime.o
	ar rcs libwsq_runtime.a wsq_runtime.o $(CHEZP)/kernel.o $(STATICLIBS)
	@echo;echo MADE SHARED LIBRARY; echo; echo

example_main:
	$(CC) $(FLAGS) -c -I $(CHEZP) example_main.c 
	$(CC) $(FLAGS) $(LIBS) example_main.o libwsq_runtime.a -o example_main

clean:
	rm -rf *.o *.a *.exe a.out *.so example_main query.*
	(cd example_distributed; $(MAKE) clean)


