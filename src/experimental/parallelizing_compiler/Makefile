


#CC=icc
#CC=gcc
CC=g++

MACHINE= $(shell echo '(machine-type)' | chez -q )


all: cas tbb


# ASSUMES TBB paths are in LIBRARY_PATH and CPATH:
tbb:
	$(CC) -fPIC -I$(CHEZD)/boot/$(MACHINE)/ -shared -o tbb_wrapper.so tbb_wrapper.cpp -lstdc++ -ltbb 

cas:
	$(CC) -fPIC -I$(CHEZD)/boot/$(MACHINE)/ -shared -o cas.so msAtomicIntel.c

clean:
	rm -f *.so
