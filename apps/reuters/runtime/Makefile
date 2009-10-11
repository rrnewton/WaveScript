

CHEZP = ~/bin/Linux-i686/csv7.9.2/boot/i3le


DIRS32 = -L/lib32 -L/usr/lib32

FLAGS = -m32 -rdynamic $(DIRS32)

LIBS = -lm -ldl -lncurses -lrt
# LIBS = -lm -ldl -lrt

#STATICLIBS= /usr/lib/librt.a /usr/lib/libdl.a

# $(CHEZP)/custom.o

all:
#	gcc $(FLAGS) -I $(CHEZP) $(CHEZP)/kernel.o $(LIBS) wsq_runtime.c 
	gcc $(FLAGS) -c -fPIC -I $(CHEZP) wsq_runtime.c -o wsq_runtime.o
# Shared:
#	gcc $(FLAGS) -shared -Wl,-soname,libwsq_runtime.so -o libwsq_runtime.so wsq_runtime.o $(LIBS) $(CHEZP)/kernel.o 
# Static:
#	ar rcs libwsq_runtime.a wsq_runtime.o $(LIBS) $(CHEZP)/kernel.o 
	ar rcs libwsq_runtime.a wsq_runtime.o $(CHEZP)/kernel.o $(STATICLIBS)

	@echo;echo MADE SHARED LIBRARY; echo; echo
	gcc $(FLAGS) -c -I $(CHEZP) example_main.c 
#	gcc -m32 example_main.o wsq_runtime.o -o example_main.exe
	gcc $(FLAGS) $(LIBS) example_main.o libwsq_runtime.a -o a.out
#	gcc example_main.o libwsq_runtime.so -o example_main.exe

#	gcc $(FLAGS) -c -fPIC -I $(CHEZP) $(CHEZP)/kernel.o $(LIBS) wsq_runtime.c -o wsq_runtime.o

#	gcc $(FLAGS) -I $(CHEZP) $(CHEZP)/kernel.o $(CHEZP)/custom.o $(LIBS) wsq_runtime.c

clean:
	rm -f *.o *.a *.exe a.out *.so
