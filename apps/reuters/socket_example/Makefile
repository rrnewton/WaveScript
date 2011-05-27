

all: one two

## These don't use -threads right now:
one: 
	wsc2 example1.ws -main outstrm -o server
	wsc2 example1.ws -main  instrm2 -o client

two: 
	wsc2 example2.ws -main p1 -o p1
	wsc2 example2.ws -main p2 -o p2
	wsc2 example2.ws -main p3 -o p3

# This one is aspirational -- doesn't work yet
three: 
	wsc2 example3.ws -main main1 -o proc1
	wsc2 example3.ws -main main2 -o proc2


test: run run2

# You probably want to run these in two separate windows, but this runs them together:
run:
	 (./server.exe &) ; ./client.exe 

# Similarly:
# Here I insert sleeps to make sure there's no race to connect.
run2:
	(./p1.exe &); (sleep 1;./p2.exe &); (sleep 1; ./p3.exe -n 50)


run3:
#	(./proc1.exe &); ./proc2.exe
	(./proc1.exe &); sleep 2; ./proc2.exe


clean:
	rm -rf *.exe *.c *~ *.dSYM
