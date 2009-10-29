

all:
	echo "Actually there's not really anything to compile in this directory..."

test:
	@echo
	@echo "  Running all tests with output to test_output.txt ..."
	./test_all.sh > test_output.txt
	@echo
	@echo "  Diff against with correct outpkt:"
	diff correct_test_outputs.txt test_output.txt

#	@echo $(?)
# if $?; then echo TEST PASSED; else echo TEST FAILED; fi

clean:
