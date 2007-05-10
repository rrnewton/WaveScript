

all: gsl.ws matrix_gsl.ws ws_gslpak.so

gsl.ws: gsl.ws.pp
	cpp -P gsl.ws.pp gsl.ws 

matrix_gsl.ws: matrix_gsl.ws.pp
	cpp -P matrix_gsl.ws.pp matrix_gsl.ws 


ws_gslpak.so: GSL_extras.c GSL_extras.h
	gcc  -lm -lgslcblas -lgsl -shared GSL_extras.c -o ws_gslpak.so

old:
	gcc GSL_extras.c -shared -o GSL_extras.so
	gcc  -lm -lgslcblas -lgsl GSL_extras.so -shared -o ws_gslpak.so



erk:
	g++ -c GSL_extras.c
	g++  -lm -lgslcblas -lgsl GSL_extras.o -shared -o ws_gslpak.so

clean:
	$(RM) -f ws_gslpak.so gsl.ws 