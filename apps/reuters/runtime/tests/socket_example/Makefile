
# Main entrypoints:

# Build executables:
all: one two three

# Run all three tests:
test: one.testrun two.testrun three.testrun 

#================================================================================

## These don't use -threads right now:
one: server.exe client.exe
server.exe: example1.ws
	wsc2 example1.ws -main outstrm -o server
client.exe: example1.ws
	wsc2 example1.ws -main  instrm2 -o client

two: p1.exe p2.exe p3.exe
p1.exe: example2.ws
	wsc2 example2.ws -main p1 -o p1
p2.exe: example2.ws
	wsc2 example2.ws -main p2 -o p2
p3.exe: example2.ws
	wsc2 example2.ws -main p3 -o p3

# This one is aspirational -- doesn't work yet
three: proc1.exe proc2.exe
proc1.exe: example3.ws
	wsc2 example3.ws -main main1 -o proc1
proc2.exe: example3.ws
	wsc2 example3.ws -main main2 -o proc2

four: e4_server.exe e4_client.exe

e4_server.exe: example4.ws
	wsc2 -realtime example4.ws -main main1 -o e4_server
e4_client.exe: example4.ws
	wsc2 -realtime example4.ws -main main2 -o e4_client



# four.out
# Not working... odd:
#	(make run) &> test
one.out: server.exe client.exe
	bash -c "make run &> /dev/stdout | tee one.raw"
	grep Unpacked one.raw > one.out
two.out: p1.exe p2.exe p3.exe
	bash -c "make run2 &> /dev/stdout | tee two.raw"
	grep received two.raw > two.out
three.out: proc1.exe proc2.exe
	bash -c "make run3 &> /dev/stdout | tee three.raw"
        # Put them in a consistent order, and take a consistent # of each:
	egrep 'Left'  three.raw | head -n7  > three.out
	egrep 'Right' three.raw | head -n7 >> three.out
#four.out:
#	bash -c "make run4 &> /dev/stdout | tee four.out"

# You probably want to run these in two separate windows, but this runs them together:
run:
	 (./server.exe &) ; ./client.exe

# Similarly:
# Here I insert sleeps to make sure there's no race to connect.
run2:
	(./p1.exe &); (sleep 1;./p2.exe &); (sleep 1; ./p3.exe -n 50)


run3:
#	(./proc1.exe &); ./proc2.exe
	(./proc1.exe &); sleep 2; ./proc2.exe -n 30


include ../make.common

clean:
	rm -rf *.exe *.c *~ *.dSYM *.out *.raw
