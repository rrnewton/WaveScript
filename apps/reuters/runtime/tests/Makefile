
# This makefile both builds and runs the tests.

include ../makefile.common


SOURCES = 1_ReuterSource.c 2_Filter.c 3_Project.c \
 4_MatchRecognize_always_succeed.c 5_MatchRecognize_rare_hits.c \
 6_UDF.c 7_MergeMonotonic.c 7B_MergeFiles.c \
 8_TAQ_reader.c 10_MergeTest.c 11_Avg_UDF.c

# 6_Orfilter.c

EXES  = $(SOURCES:.c=.exe)
TESTS = $(SOURCES:.c=.testrun)  9_example_distributed.testrun
OUTS  = $(SOURCES:.c=.out)      9_example_distributed.out

################################################################################
## MAIN ENTRYPOINTS:

all: $(EXES) distmake sockmake

test: fulltests socktest
	@echo 
	@echo " === DONE RUNNING TESTS, APPARENTLY SUCCESSFUL ==="
	@echo "Double check that there are no \*.err files:"
	find -name "*.err"

# These represent certain tests that can be run with more input and
# thereby become benchmarks.
bench: benchmark
benchmark:  8_TAQ_reader.bench

# Here we require the .out file to force the generated_query_ file:
8_TAQ_reader.bench: taq_500mb.log 8_TAQ_reader.out
	INPUT_FILE=taq_500mb.log ./generated_query_8.exe

taq_500mb.log:
	wget http://cs.indiana.edu/~rrnewton/temp/taq_500mb.log

################################################################################

distmake:
	(cd example_distributed; $(MAKE))

sockmake:
	(cd socket_example;      $(MAKE))



socktest:
	(cd socket_example;      $(MAKE) test)

# Do a full build, run, and check output results:
fulltests: $(TESTS)

# Run but do not COMPARE the expected outputs:
runonly: $(OUTS)

include make.common

.c.exe:
	$(CC) $(FLAGS) -c -I $(CHEZP) $<
	$(CC) $(FLAGS) $(LIBS) $(<:.c=.o) ../libwsq_runtime.a -o $@


9_example_distributed.out:
	(cd example_distributed; $(MAKE) run)

clean: 
	rm -rf *.out *~ *.exe *.o *.dSYM *query.c *.testrun *.run *_*.err generated_query*
	(cd example_distributed; $(MAKE) clean)
	(cd socket_example;      $(MAKE) clean)
