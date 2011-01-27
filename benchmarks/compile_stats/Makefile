
all: alloc_sites.log refcounts.log

graph: refcounts.eps alloc_sites.eps
	epstopdf refcounts.eps
	epstopdf alloc_sites.eps

refcounts.eps: refcounts.log
	gnuplot refcounts.gp > refcounts.eps

refcounts.log:
	echo " Name Total Local Heap PerCodesize" > refcounts.log
	./refcount_opportunities.ss | grep "Total refcounts:" | sed 's/Total refcounts:/ /' >> refcounts.log
#	./refcount_opportunities.ss | grep "Total refcounts:" | sed 's/[^[:digit:]]/ /g' >> refcounts.log

alloc_sites.eps: alloc_sites.log
	gnuplot alloc_sites.gp > alloc_sites.eps

alloc_sites.log:
	echo "# OnTrunkStatic OnTrunkDynamic OffTrunk OnTrunkDontEscape CodeSize" > alloc_sites.log
	./measure_alloc_sites.ss | grep "ALLOC:" | sed 's/ALLOC://' | grep -v "0 0 0 0" >> alloc_sites.log
#	./measure_alloc_sites.ss | grep "ALLOC-COUNT" | awk '{ print $3" "$4" "$5" "$6" "$7" "$8 }'
#	./measure_alloc_sites.ss | grep "ALLOC COUNT" | sed 's/[^[:digit:]]/ /g' >> alloc_sites.log

clean:
	rm -f *.log *.eps

