

MACHINE = `$(REGIMENTD)/depends/get_machine_type`
#EXTRAS=`if [ "$(OSTYPE)" == "Linux" ]; then echo "-lrt"; fi` 
#LIBS = -lm -ldl -lncurses $(EXTRAS)


LIBS = -lm -ldl -lncurses 
ifeq ($(OSTYPE), linux-gnu)
  LIBS+= -lrt
endif
ifeq ($(OSTYPE), Linux)
  LIBS+= -lrt
endif


CHEZP = $(CHEZD)/boot/$(MACHINE)
DIRS32 = -L/lib32 -L/usr/lib32
FLAGS = -g -O0 -m32 -rdynamic $(DIRS32)

# Perhaps should consider linking these libs statically for portability.
#STATICLIBS= /usr/lib/librt.a /usr/lib/libdl.a

all: gen_proc1 gen_proc2 run_proc1 run_proc2


gen_proc1: proc1.c port.h
	gcc $(FLAGS) -c -I $(CHEZP) proc1.c 
	gcc $(FLAGS) $(LIBS) proc1.o ../libwsq_runtime.a -o gen_proc1

gen_proc2: proc2.c port.h
	gcc $(FLAGS) -c -I $(CHEZP) proc2.c 
	gcc $(FLAGS) $(LIBS) proc2.o ../libwsq_runtime.a -o gen_proc2

run_proc1: gen_proc1
	./gen_proc1
	mv query.exe run_proc1

run_proc2: gen_proc2
	./gen_proc2
	mv query.exe run_proc2

run: 
	 (./run_proc1 &) ; ./run_proc2 | head    

clean:
	rm -rf *.o *.a *.exe a.out *.so *proc1 *proc2 query.* *.dSYM

