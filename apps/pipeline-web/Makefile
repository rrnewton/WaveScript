
all: data.txt
	wsc 1_simple_moving_average.ws

data.txt: data.txt.gz
	gunzip -c data.txt.gz > data.txt

test: data.txt
	rm -rf test_out.1 test_out.2 test_out.3
	echo dump test_out.1 | ws.debug 1_simple_moving_average.ws -exit-error
	echo dump test_out.2 | ws.debug 2_with_arrays.ws -exit-error
	echo dump test_out.3 | ws.debug 3_manually_printed_output.ws -exit-error

#	wsc 1_simple_moving_average.ws
#	wsc 2_with_arrays.ws
#	wsc 3_manually_printed_output.ws

clean:
	rm -f .__* run_query query.cpp *~ test_out.*

