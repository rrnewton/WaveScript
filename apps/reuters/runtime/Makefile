

#CHEZD = ~/bin/Linux-i686/csv7.9.2/boot/i3le

#MACHINE = `echo (machine-type) | chez -q`

########################################
# FOR MAC OS
#MACHINE = i3osx
#LIBS = -lm -ldl -lncurses 

########################################
# FOR LINUX:
MACHINE = i3le
LIBS = -lm -ldl -lncurses -lrt
# [2009.11.18] Oh, because of clock_gettime..
########################################


CHEZP = $(CHEZD)/boot/$(MACHINE)

DIRS32 = -L/lib32 -L/usr/lib32

FLAGS = -g -O0 -m32 -rdynamic $(DIRS32)

# Perhaps should consider linking these libs statically for portability.
#STATICLIBS= /usr/lib/librt.a /usr/lib/libdl.a

all: libwsq_runtime.a example_main


libwsq_runtime.a: wsq_runtime.c 
	@if [ -d "$(CHEZD)" ]; then echo; else echo "ERROR: CHEZD ($(CHEZD)) does not exist."; exit 1; fi
	gcc $(FLAGS) -c -fPIC -I $(CHEZP) wsq_runtime.c -o wsq_runtime.o
	ar rcs libwsq_runtime.a wsq_runtime.o $(CHEZP)/kernel.o $(STATICLIBS)
	@echo;echo MADE SHARED LIBRARY; echo; echo

example_main:
	gcc $(FLAGS) -c -I $(CHEZP) example_main.c 
	gcc $(FLAGS) $(LIBS) example_main.o libwsq_runtime.a -o example_main

clean:
	rm -f *.o *.a *.exe a.out *.so example_main query.*
	(cd example_distributed; $(MAKE) clean)


