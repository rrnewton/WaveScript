
# This makefile both builds and runs the tests.

include ../makefile.common


SOURCES = 1_ReuterSource.c 2_Filter.c 3_Project.c \
 4_MatchRecognize_always_succeed.c 5_MatchRecognize_rare_hits.c 6_Orfilter.c

EXES  = $(SOURCES:.c=.exe)
TESTS = $(SOURCES:.c=.testrun)
OUTS  = $(SOURCES:.c=.out)

all: $(EXES)

test: $(TESTS)


.SUFFIXES: .o .c .exe .testrun .out
.PRECIOUS: $(EXES) $(OUTS) $(TESTS)

.c.exe:
	$(CC) $(FLAGS) -c -I $(CHEZP) $<
	$(CC) $(FLAGS) $(LIBS) $(<:.c=.o) ../libwsq_runtime.a -o $@


.exe.out:
	@echo 
	@echo ====================================================================================================
	@echo RUNNING TEST $< 
	@echo -------------------------------
	@echo 
	WSQ_VERBOSE=1 ./$<  &> /dev/stdout | tee $(<:.exe=.run)

.out.testrun:
	@echo 
	@echo Comparing output to expected, check .err file for results if an error occurs:
	python compare.py -p $(<:.out=.compare) -c $<  > $(<:.out=.err)
# if we were successful, move the result into place:
	mv $(<:.out=.err) $@ 
	@echo 
	@echo --------------------
	@echo COMPARISON SUCCEEDED
	@echo ====================================================================================================
	@echo 

clean: 
	rm -rf *.out *~ *.exe *.o *.dSYM *query.c *.testrun *.run

