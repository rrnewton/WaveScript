
include makefile.common


all: libwsq_runtime.a example_main.exe shailendra_example.exe shailendra_server.exe shailendra_client.exe

libwsq_runtime.a: wsq_runtime.c 
	@if [ -d "$(CHEZD)" ]; then echo; else echo "ERROR: CHEZD ($(CHEZD)) does not exist."; exit 1; fi
	gcc $(FLAGS) -c -fPIC -I $(CHEZP) wsq_runtime.c -o wsq_runtime.o
	ar rcs libwsq_runtime.a wsq_runtime.o $(CHEZP)/kernel.o $(STATICLIBS)
	@echo;echo MADE SHARED LIBRARY; echo; echo

example_main.exe: example_main.c 
	$(CC) $(FLAGS) -c -I $(CHEZP) example_main.c 
	$(CC) $(FLAGS) $(LIBS) example_main.o libwsq_runtime.a -o example_main.exe

shailendra_example.exe: shailendra_example.c
	$(CC) $(FLAGS) -c -I $(CHEZP) shailendra_example.c 
	$(CC) $(FLAGS) $(LIBS) shailendra_example.o libwsq_runtime.a -o shailendra_example.exe


# DUPLICATED CODE IN THESE:
shailendra_server.exe: shailendra_server.c
	$(CC) $(FLAGS) -c -I $(CHEZP) shailendra_server.c 
	$(CC) $(FLAGS) $(LIBS) shailendra_server.o libwsq_runtime.a -o shailendra_server.exe
shailendra_client.exe: shailendra_client.c
	$(CC) $(FLAGS) -c -I $(CHEZP) shailendra_client.c 
	$(CC) $(FLAGS) $(LIBS) shailendra_client.o libwsq_runtime.a -o shailendra_client.exe


clean:
	rm -rf *.o *.a *.exe a.out *.so example_main query.*
	(cd example_distributed; $(MAKE) clean)


