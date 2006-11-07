

# 140.247.90.172 - blacksmoker.

# [2006.02.20] Seems like with a 30 node network, 5 second sample rate
# (60 second global tree update).  That we can simulate a day's
# running in (hopefully) between 15 and 30 minutes real time.
# Eh, that was a bit off.  It took 45 minutes.  The log is 430 mb's gzipped.
# Fasl-reencoding the log-file took another 40 minutes (reduced 430mb to 330mb).
#
# It took 50 minutes to run 250 nodes for a virtual "hour".

NOISE = 6 # stddev six degrees celcius

# ENTRYPOINTS: 
# Examples:
#   make quick runall
#   make full run0 analyze0

all: quick runall analyzeall

# This does all the runs and analysis at a very low resolution with a small network.
# This should allow us to always regenerate results right after
# checking out code, thus making sure that everything works.
quick: world.simworld
	export NUMNODES=30
	export BASENAME=smallrun_
#	LOWER = 0
#	STEP = 10
#	UPPER = 40
#	$(MAKE) allruns

full: 
	NUMNODES=200
	BASENAME=fullrun_
	LOWER=0
	STEP=5
	UPPER=40
#	$(MAKE) allruns

################################################################################


world.simworld:
	./create_topology.ss world.simworld

runall: world.simworld run0 run1 # run2 run3

run0:
	LOGFILENAME=prog0/$(BASENAME)
	INPUTFILE=prog0.rs
	(export LOGFILENAME=$(LOGFILENAME); cd ./paper_figures; nice -n 10 ./runall )

run1:
	LOGFILENAME=prog1/$BASENAME
	INPUTFILE=prog1.rs
	(cd ./paper_figures; nice -n 10 ./runall )

run2:
	LOGFILENAME=prog2/$BASENAME
	INPUTFILE=p2/prog2_manual.tm
	(cd ./paper_figures; nice -n 10 ./runall )

run3:
	LOGFILENAME=prog3/$BASENAME
	INPUTFILE=p3/p3_hand.tm
	(cd ./paper_figures; nice -n 10 ./runall )

analyzeall:
analyze0:
analyze1:
analyze2:
analyze3:


################################################################################
## OLD:

all: world.simworld allruns #allanalyze


### Not using these individual entrypoints atm.  See allruns
prog1:
	for i in `seq 0 50`; do \
	if  -d "./logs/prog1/deadsimple_thresh"$$i"_noise"$(NOISE)".log.gz" ]; then \
	./analyze_vs_groundtruth.ss -param $$i -noise $(NOISE) \
	    -f "prog1.rs" \
	    -l "./logs/prog1/deadsimple_thresh"$$i"_noise"$(NOISE)".log.gz" \
	    -w "world.simworld"; \
	fi; done

prog2:
	for i in `seq 0 50`; do \
	if  -d "./logs/prog2/deadsimple_thresh"$$i"_noise"$(NOISE)".log.gz" ]; then \
	./analyze_vs_groundtruth.ss -param $$i -noise $(NOISE) \
	    -f "prog2.rs" \
	    -l "./logs/prog2/deadsimple_thresh"$$i"_noise"$(NOISE)".log.gz" \
	    -w "world.simworld"; \
	fi; done

prog2_manual:
	for i in `seq 0 50`; do \
	if test -d foo \ #"./logs/prog2/deadsimple_thresh"$$i"_noise"$(NOISE)".log.gz" ; \
	then @echo "Log already exists! Noise:"$(NOISE)" and Thresh:"$$i; \
	else echo "RUNNING IT -- Noise:"$(NOISE)" and Thresh:"$$i; \
	fi; done

#	./analyze_vs_groundtruth.ss -param $$i -noise $(NOISE) \
#	    -f "prog2_manual.tm" \
#	    -l "./logs/prog2/deadsimple_thresh"$$i"_noise"$(NOISE)".log.gz" \
#	    -w "world.simworld"; \

prog3:



#	(cd ./paper_figures; nice -n 10 nohup ./runall &)

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
