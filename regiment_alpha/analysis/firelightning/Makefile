

# [2006.02.20] Seems like with a 30 node network, 5 second sample rate
# (60 second global tree update).  That we can simulate a day's
# running in (hopefully) between 15 and 30 minutes real time.
# Eh, that was a bit off.  It took 45 minutes.  The log is 430 mb's gzipped.
# Fasl-reencoding the log-file took another 40 minutes (reduced 430mb to 330mb).
#
# It took 50 minutes to run 250 nodes for a virtual "hour".

NOISE = 6 # stddev six degrees celcius

test:
	for i in `seq 1 5`; do echo "Woot $$i"; done

all: world.simworld allruns #allanalyze


prog1:
	for i in `seq 0 50`; do \
	./run_querysim.ss -param $$i -noise $(NOISE) \
	    -f "prog1.rs" \
	    -l "./logs/prog1/deadsimple_thresh"$$i"_noise"$NOISE".log.gz" \
	    -w "world.simworld"; \
	done

prog2:
	for i in `seq 0 50`; do \
	./run_querysim.ss -param $$i -noise $(NOISE) \
	    -f "prog2.rs" \
	    -l "./logs/prog1/deadsimple_thresh"$$i"_noise"$NOISE".log.gz" \
	    -w "world.simworld"; \
	done

world.simworld:
	./create_topology.ss world.simworld

allruns:
	(cd ./paper_figures; nice -n 10 nohup ./runall &)

allanalyze:
	(cd ./paper_figures; ./analyze_all)

# This runs the simulation while also running the analysis.
# NOTE: Have to make sure the logger is flushing each of its writes.
# Not working correctly right now because the analyzer 
pipelined:
	./analyze_deadsimple_vs_groundtruth.ss run &
	sleep 5
	./analyze_deadsimple_vs_groundtruth.ss analyze

clean:
	$(RM) _genned_node_code.ss world.simworld
