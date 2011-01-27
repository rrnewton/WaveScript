

cat FULLPAR_LONGTEST.txt | dbcolcreate speedup 0 | dbroweval '_speedup = 76776.0/_realtime;' | \
    dbmultistats numthreads speedup | dbsort -n numthreads > /tmp/stats

cat MAPSPLIT_GRIDS_LONG.txt | dbcolmerge numthreads gridsize |  dbmultistats numthreads_gridsize realtime | dbcolsplittocols numthreads_gridsize | dbsort -n numthreads > /tmp/stats2

for grids in 8 16 32 64 128 256 512 1024 ; do
  cat /tmp/stats2 | dbrow "_gridsize == $grids" > /tmp/t
  baseline=`cat /tmp/t | dbrow "_numthreads == 0" | grep -v "#" | cut -f 2`
  echo "baseline for $grids is $baseline"
  cat MAPSPLIT_GRIDS_LONG.txt | dbrow "_gridsize == $grids" | \
    dbcolcreate speedup 0 | dbroweval "_speedup = $baseline / _realtime" | \
    dbmultistats numthreads speedup | dbsort -n numthreads > /tmp/grids.$grids
done
