

all:
	$(MAKE) profile
	$(MAKE) tabulate
	$(MAKE) graph

profile:
	./profile_data_reps.ss

tabulate:
	./tabulate_results.ss

graph:
	./plotall.sh
#	gnuplot plot_fold.gp > fold.eps

clean:
	rm -f query.* query

distclean: clean
	rm -f *.result *.eps *.pdf
	(cd results; make clean)
