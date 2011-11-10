
include ../makefile.common

SOURCES = 1_TAQ_reader.c 2_TAQ_over_socket_A.c 2_TAQ_over_socket_B.c
GEN = generated_query_1.exe generated_query_2A.exe generated_query_2B.exe
EXES  = $(SOURCES:.c=.exe)

#--------------------------------------------------------------------------------
# Entrypoints:

all: $(EXES) $(GEN)

run: $(GEN)
	./run_benchmark.sh generated_query_1.exe
#	./run_dist_benchmark.sh generated_query_2A.exe generated_query_2B.exe

#--------------------------------------------------------------------------------
# Shortcuts:

1: generated_query_1.exe
2: generated_query_2A.exe generated_query_2B.exe

#--------------------------------------------------------------------------------
# Build rules:

test: run

generated_query_1.exe: 1_TAQ_reader.exe
	./build_benchmark.sh $<

generated_query_2A.exe: 2_TAQ_over_socket_A.exe 
	./build_benchmark.sh $<

generated_query_2B.exe: 2_TAQ_over_socket_B.exe 
# Getting that weird stack smashing on exit here:
	(./build_benchmark.sh $<; echo "IGNORING stack smashing... weird")


.SUFFIXES: .o .c .exe .testrun .out
.PRECIOUS: $(EXES) $(OUTS) $(TESTS)

.c.exe:
	$(CC) $(FLAGS) -c -I $(CHEZP) $<
	$(CC) $(FLAGS) $(LIBS) $(<:.c=.o) ../libwsq_runtime.a -o $@

clean: 
	rm -rf *.out *~ *.exe *.o *.dSYM *query.c *.testrun b*.run *_*.err generated_query*.c *.log


