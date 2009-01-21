


ENCHS=" just_timer.ws read_filedata_bigwins.ws read_filedata_smallwins.ws"

all: /tmp/dummyfile.bin
	@echo "   Running alloc/datapass benchmarks, building pdf summary."
#	./run_misc.sh
	./run_datapass.sh
	$(MAKE) graph

graph:
	cp RESULTS_misc.txt RESULTS.txt
	gnuplot plot.gp > microbench1.eps
	ps2pdf microbench1.eps
#	cp RESULTS_datapass.txt RESULTS.txt
#	gnuplot plot.gp > microbench2.eps
#	ps2pdf microbench2.eps

# This makse a ~40MB file.
/tmp/dummyfile.bin:
#	dd if=/dev/zero of=/tmp/dummyfile.bin bs=1M count=40
	dd if=/dev/urandom of=/tmp/dummyfile.bin bs=1M count=40

clean:
	rm -f *~ __* .__* query.* /tmp/dummyfile.bin microbench*.eps microbench*.pdf 6sec_marmot_sample.raw

