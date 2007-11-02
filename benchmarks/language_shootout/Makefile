

all:
	./runall.sh
	$(MAKE) graph


graph:
	gnuplot plot.gp > shootout.eps
	ps2pdf shootout.eps

gather:



clean:
	rm -f *~ __* .__* query.* shootout.eps shootout.pdf

