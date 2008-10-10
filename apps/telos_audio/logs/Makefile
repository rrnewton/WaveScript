
all:
	./extract_info.sh
	./summarize.ss > summary.dat
	gnuplot plot.gp > plot.eps
	ps2pdf plot.eps

clean:
	rm -f cut_*.ss cut_*.dat

distclean: clean
	rm -f cut_* compile_*
