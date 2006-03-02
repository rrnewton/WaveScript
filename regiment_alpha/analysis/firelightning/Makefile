

# [2006.02.20] Seems like with a 30 node network, 5 second sample rate
# (60 second global tree update).  That we can simulate a day's
# running in (hopefully) between 15 and 30 minutes real time.
# Eh, that was a bit off.  It took 45 minutes.  The log is 430 mb's gzipped.
# Fasl-reencoding the log-file took another 40 minutes (reduced 430mb to 330mb).
#
# It took 50 minutes to run 250 nodes for a virtual "hour".

all: 




# This runs the simulation while also running the analysis.
# NOTE: Have to make sure the logger is flushing each of its writes.
# Not working correctly right now because the analyzer 
pipelined:
	./analyze_deadsimple_vs_groundtruth.ss run &
	sleep 5
	./analyze_deadsimple_vs_groundtruth.ss analyze

