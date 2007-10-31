


BENCHS=" just_timer.ws read_filedata_bigwins.ws read_filedata_smallwins.ws"

all: /tmp/dummyfile.bin
	@echo "   Running all MICRObenchmarks, building pdf summary."
	./runall.sh
	$(MAKE) graph

graph:
	gnuplot plot.gp > microbench.eps
	ps2pdf microbench.eps


# This makse a ~40MB file.
# 
# I'm sure there's a better way to make a file full of nonsense.
# Could use /dev/something or other...
/tmp/dummyfile.bin:
	rm -f /tmp/one /tmp/two
	cp just_timer.ws /tmp/one
	for i in 1 2 3 4 5 6 7 8 9 10 11 12 13; do cat /tmp/one >> /tmp/two; cat /tmp/two >> /tmp/one; done
	mv /tmp/one /tmp/dummyfile.bin
	rm -f /tmp/two

clean:
	rm -f *~ __* .__* query.* /tmp/dummyfile.bin microbench.eps microbench.pdf

