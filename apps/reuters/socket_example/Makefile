


## These don't use -threads right now:
one: 
	wsc2 example1.ws -main outstrm -o server
	wsc2 example1.ws -main  instrm2 -o client

two: 
	wsc2 example2.ws -main p1 -o p1
	wsc2 example2.ws -main p2 -o p2
	wsc2 example2.ws -main p3 -o p3


# You probably want to run these in two separate windows, but this runs them together:
run:
	 (./server.exe &) ; ./client.exe 

# Similarly:
# Here I insert sleeps to make sure there's no race to connect.
# [2009.06.10] Presently the client is not robust to failing on connect.
run2:
	(./p1.exe &); (sleep 1;./p2.exe &); (sleep 1; ./p3.exe -n 50)

clean:
	rm -f *.exe *.c *~
