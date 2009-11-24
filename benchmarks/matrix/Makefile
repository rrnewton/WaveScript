



all: a.out 
	@echo "   Running all matrix benchmarks, building pdf summary."
	./runall.sh
	$(MAKE) graph

graph:
	gnuplot plot.gp > matrixbench.eps
	ps2pdf matrixbench.eps

a.out:	matrix_ctest.c
	gcc -Wall -O3 matrix_ctest.c

clean:
	rm -f *~ __* .__* query.* a.out matrixbench.eps matrixbench.pdf

