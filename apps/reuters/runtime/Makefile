

#CHEZD = ~/bin/Linux-i686/csv7.9.2/boot/i3le

CHEZP = $(CHEZD)/boot/i3le

DIRS32 = -L/lib32 -L/usr/lib32

FLAGS = -m32 -rdynamic $(DIRS32)

LIBS = -lm -ldl -lncurses -lrt

# Perhaps should consider linking these libs statically for portability.
#STATICLIBS= /usr/lib/librt.a /usr/lib/libdl.a

all:
	gcc $(FLAGS) -c -fPIC -I $(CHEZP) wsq_runtime.c -o wsq_runtime.o
	ar rcs libwsq_runtime.a wsq_runtime.o $(CHEZP)/kernel.o $(STATICLIBS)

	@echo;echo MADE SHARED LIBRARY; echo; echo
	gcc $(FLAGS) -c -I $(CHEZP) example_main.c 
	gcc $(FLAGS) $(LIBS) example_main.o libwsq_runtime.a -o example_main

clean:
	rm -f *.o *.a *.exe a.out *.so example_main query.*

