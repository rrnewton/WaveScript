


ENCHS=" just_timer.ws read_filedata_bigwins.ws read_filedata_smallwins.ws"

all: 
	rm -f /tmp/dummyfile.bin
	$(MAKE) /tmp/dummyfile.bin
	@echo "   Running all MICRObenchmarks, building pdf summary."
	./run_misc.sh
	./run_datapass.sh
	$(MAKE) graph

#	cp RESULTS_misc.txt RESULTS.txt
#	cp RESULTS_datapass.txt RESULTS.txt

graph:
	gnuplot plot_misc.gp > microbench1.eps
	gnuplot plot_datapass10.gp   > datapass10.eps
	gnuplot  plot_datapass100.gp > datapass100.eps
	gnuplot plot_datapass1000.gp > datapass1000.eps
	ls *.eps | xargs -i ps2pdf {}

# 
# I'm sure there's a better way to make a file full of nonsense.
# Could use /dev/something or other...
# Alas, no 'seq' under mac os x:

# This makse a 500MB file.
SEQ=1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 
# Scratch that... now it makes 1.5 gb.
#SEQ=1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18

#SEQ=1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 21 22 23 24 25 26 27 28 29 30
/tmp/dummyfile.bin:
	rm -f /tmp/one /tmp/two
	echo "nonsenseseednonsenseseednonsenseseednonsenseseednonsenseseed" > /tmp/one
	for i in $(SEQ); do cat /tmp/one >> /tmp/two; cat /tmp/two >> /tmp/one; done
	mv /tmp/one /tmp/dummyfile.bin
	rm -f /tmp/two

#	cp just_timer.ws /tmp/one

clean:
	rm -f *~ __* .__* query.* /tmp/dummyfile.bin microbench*.eps microbench*.pdf 6sec_marmot_sample.raw

