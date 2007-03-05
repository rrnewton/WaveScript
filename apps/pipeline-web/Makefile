
all: data.txt
	wsc 1_simple_moving_average.ws

data.txt: data.txt.gz
	gunzip -c data.txt.gz > data.txt

test: data.txt
	echo dump test_out.1 | ws 1_simple_moving_average.ws -exit-error
	echo dump test_out.2 | ws 2_with_arrays.ws -exit-error
	echo dump test_out.3 | ws 3_manually_printed_output.ws -exit-error

#	wsc 1_simple_moving_average.ws
#	wsc 2_with_arrays.ws
#	wsc 3_manually_printed_output.ws

clean:
	rm -f .__* run_query query.cpp *~ test_out.*

