




hmm:
	gcc  -lm -lgslcblas -lgsl -shared GSL_extras.c -o ws_gslpak.so



all: ws_gslpak.so

ws_gslpak.so:
	gcc GSL_extras.c -shared -o GSL_extras.so
	gcc  -lm -lgslcblas -lgsl GSL_extras.so -shared -o ws_gslpak.so



erk:
	g++ -c GSL_extras.c
	g++  -lm -lgslcblas -lgsl GSL_extras.o -shared -o ws_gslpak.so