
all: data.txt
	wsc 1_simple_moving_average.ws

data.txt: 
	gunzip data.txt.gz

test:
	echo dump test_out.1 | ws 1_simple_moving_average.ws
	echo dump test_out.2 | ws 2_with_arrays.ws
	echo dump test_out.3 | ws 3_manually_printed_output.ws

#	wsc 1_simple_moving_average.ws
#	wsc 2_with_arrays.ws
#	wsc 3_manually_printed_output.ws

clean:
	rm -f .__* run_query query.cpp *~ test_out.*

