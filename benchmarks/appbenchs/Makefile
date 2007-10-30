

#BENCHS=" just_timer.ws read_filedata_bigwins.ws read_filedata_smallwins.ws"

all: 
	@echo "   Running application benchmarks."
	./runall.sh
	$(MAKE) graph

graph:
	gnuplot plot.gp > marmot.eps


clean:
	rm -f *~ __* .__* query.* marmot.eps marmot.pdf

