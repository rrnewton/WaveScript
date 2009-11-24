CXX=g++
CXXFLAGS=-Wall -Werror -g -I./include 
CTHFLAGS=-Wall -Werror -g -I./include -pthread
#CXXFLAGS=-I./include 

LIBOBJS=ESBox.o

VERIFY=app/verify
PATTERN=app/pattern

EVENT=Event.o
BUFFER=Buffer.o
SOURCE=Source.o
FILTER=Filter.o

TEST=Test.o
SOCKET=Socket.o

THREAD=Thread.o
SERVER=Server.o
CLIENT=Client.o
QUERYENGINE=QueryEngine.o
NODE=Node.o
NODE1=Node1.o	
NODE2=Node2.o	
NODE3=Node3.o

vpath %.cc $(VERIFY) $(PATTERN) 

all: Event Buffer Source Filter Server QueryEngine Thread Node
socket: Node1 Node2 Node3
verify: Event Source Socket

Event: $(EVENT) 
	$(CXX) $(CXXFLAGS) $(EVENT) -o $@EX

Buffer: $(BUFFER) 
	$(CXX) $(CXXFLAGS) $(BUFFER) -o $@EX

Source: $(SOURCE) 
	$(CXX) $(CTHFLAGS) $(SOURCE) -o $@EX

Filter: $(FILTER) 
	$(CXX) $(CTHFLAGS) $(FILTER) -o $@EX

Thread: $(THREAD) 
	$(CXX) $(CTHFLAGS) $(THREAD) -o $@EX

Server: $(SERVER) 
	$(CXX) $(CTHFLAGS) $(SERVER) -o $@EX

QueryEngine: $(QUERYENGINE) 
	$(CXX) $(CTHFLAGS) $(QUERYENGINE) -o $@EX

Client: $(CLIENT) 
	$(CXX) $(CXXFLAGS) $(CLIENT) -o $@EX

Node: $(NODE) 
	$(CXX) $(CTHFLAGS) $(NODE) -o $@EX

Node1: $(NODE1) 
	$(CXX) $(CTHFLAGS) $(NODE1) -o $@EX

Node2: $(NODE2) 
	$(CXX) $(CTHFLAGS) $(NODE2) -o $@EX

Node3: $(NODE3) 
	$(CXX) $(CTHFLAGS) $(NODE3) -o $@EX

Test: $(TEST) 
	$(CXX) $(CXXFLAGS) $(TEST) -o $@EX


.PHONY: clean
clean:
	rm -rf *EX *~ *.o *.a $(VERIFY)/*~ $(PATTERN)/*~ box/*~ include/*~ \
	$(NFAVERIFY)/*~ $(NFAPATTERN)/*~ $(WEBLOGDIR)/*~

.PHONY: ctags
ctags:
	ctags -eR .
