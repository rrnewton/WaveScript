
BENCHMARK=17500

cat BUILDSPLIT_FINAL_600_600.txt | dbcolcreate speedup 0 | dbroweval "_speedup = $BENCHMARK/_realtime;" | \
    dbmultistats numthreads speedup | dbsort -n numthreads > buildsplit_600_600_stats.txt

cat BUILDSPLIT_FINAL_600_30.txt | dbcolcreate speedup 0 | dbroweval "_speedup = $BENCHMARK/_realtime;" | \
    dbmultistats numthreads speedup | dbsort -n numthreads > buildsplit_600_30_stats.txt

cat BUILDSPLIT_FINAL_600_1.txt | dbcolcreate latency 0 | dbroweval "_latency = _realtime/600.0;" | \
    dbmultistats numthreads latency | dbsort -n numthreads > buildsplit_600_1_stats.txt



cat MAPSPLIT_FINAL_600_600.txt | dbcolcreate speedup 0 | dbroweval "_speedup = $BENCHMARK/_realtime;" | \
    dbmultistats numthreads speedup | dbsort -n numthreads > mapsplit_600_600_stats.txt

cat MAPSPLIT_FINAL_600_30.txt | dbcolcreate speedup 0 | dbroweval "_speedup = $BENCHMARK/_realtime;" | \
    dbmultistats numthreads speedup | dbsort -n numthreads > mapsplit_600_30_stats.txt

cat MAPSPLIT_FINAL_600_1.txt | dbcolcreate latency 0 | dbroweval "_latency = _realtime/600.0;" | \
    dbmultistats numthreads latency | dbsort -n numthreads > mapsplit_600_1_stats.txt




cat FULLPAR_FINAL_600_600.txt | dbcolcreate speedup 0 | dbroweval "_speedup = $BENCHMARK/_realtime;" | \
    dbmultistats numthreads speedup | dbsort -n numthreads > fullpar_600_600_stats.txt

cat FULLPAR_FINAL_600_30.txt | dbcolcreate speedup 0 | dbroweval "_speedup = $BENCHMARK/_realtime;" | \
    dbmultistats numthreads speedup | dbsort -n numthreads > fullpar_600_30_stats.txt

cat FULLPAR_FINAL_600_1.txt | dbcolcreate latency 0 | dbroweval "_latency = _realtime/600.0;" | \
    dbmultistats numthreads latency | dbsort -n numthreads > fullpar_600_1_stats.txt


