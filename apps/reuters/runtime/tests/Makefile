
# This makefile both builds and runs the tests.

include ../makefile.common


SOURCES = 1_ReuterSource.c 2_Filter.c 3_Project.c \
 4_MatchRecognize_always_succeed.c 5_MatchRecognize_rare_hits.c \
 6_UDF.c 7_MergeMonotonic.c 7B_MergeFiles.c \
 8_TAQ_reader.c

# 6_Orfilter.c

EXES  = $(SOURCES:.c=.exe)
TESTS = $(SOURCES:.c=.testrun)  9_example_distributed.testrun
OUTS  = $(SOURCES:.c=.out)      9_example_distributed.out

all: $(EXES) distmake sockmake

distmake:
	(cd example_distributed; $(MAKE))

sockmake:
	(cd socket_example;      $(MAKE))

test: fulltests socktest
	@echo 
	@echo " === DONE RUNNING TESTS, APPARENTLY SUCCESSFUL ==="
	@echo "Double check that there are no \*.err files:"
	ls "*.err"


socktest:
	(cd socket_example;      $(MAKE) test)

# Do a full build, run, and check output results:
fulltests: $(TESTS)

# Run but do not COMPARE the expected outputs:
runonly: $(OUTS)


.SUFFIXES: .o .c .exe .testrun .out
.PRECIOUS: $(EXES) $(OUTS) $(TESTS)

.c.exe:
	$(CC) $(FLAGS) -c -I $(CHEZP) $<
	$(CC) $(FLAGS) $(LIBS) $(<:.c=.o) ../libwsq_runtime.a -o $@


9_example_distributed.out:
	(cd example_distributed; $(MAKE) run)

.exe.out:
	@echo 
	@echo ====================================================================================================
	@echo RUNNING TEST $< 
	@echo -------------------------------
        # Store the raw terminal output in the .run file (different from the query output).
        # We don't DIRECTLY write the .out file here, by convention the test itself writes it:
	@echo "(Counting on exe $< to write file $@ ...)"
	@echo 
	WSQ_VERBOSE=1 ./$<  "&>" /dev/stdout | tee $(<:.exe=.run)
	@echo 
	@echo Done Running test.
	@echo -------------------------------

# Note -- we only take as many lines of output as are in the compare file:
.out.testrun:
	@echo 
	@echo ----------------------------------------
	@echo Comparing output, $<, to expected, check .err file for results in case of failure:
        # Here we count the number of lines in the compare file and truncate that much of the .out:
	head -n $(shell ./wc_lines_only.sh $(<:.out=.compare)) $< > $(<:.out=.trunc)
	diff $(<:.out=.compare) $(<:.out=.trunc) > $(<:.out=.err)
#	python compare.py -p $(<:.out=.compare) -c $<  > $(<:.out=.err)

        # IF we were successful, move the result into place:
	mv $(<:.out=.err) $@ 
        # And remove the temporary:
	rm -f $(<:.out=.trunc)
	@echo 
	@echo --------------------
	@echo COMPARISON SUCCEEDED
	@echo ====================================================================================================
	@echo 

clean: 
	rm -rf *.out *~ *.exe *.o *.dSYM *query.c *.testrun *.run *_*.err generated_query*
	(cd example_distributed; $(MAKE) clean)
	(cd socket_example;      $(MAKE) clean)
