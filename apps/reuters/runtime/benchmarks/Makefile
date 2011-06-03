
include ../makefile.common

SOURCES = 1_TAQ_reader.c

EXES  = $(SOURCES:.c=.exe)

all: $(EXES)

test: run
run:
	./run_benchmark.sh 1_TAQ_reader.exe generated_query_1.exe

.SUFFIXES: .o .c .exe .testrun .out
.PRECIOUS: $(EXES) $(OUTS) $(TESTS)

.c.exe:
	$(CC) $(FLAGS) -c -I $(CHEZP) $<
	$(CC) $(FLAGS) $(LIBS) $(<:.c=.o) ../libwsq_runtime.a -o $@

clean: 
	rm -rf *.out *~ *.exe *.o *.dSYM *query.c *.testrun *.run *_*.err generated_query*.c *.log


