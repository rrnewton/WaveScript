

// This stresses the region simulation within the regionsim.ws library.

// Primarily, it stresses hash tables.

include "regionsim.ws"


/* This file also contains multiple examples of how to use Regions,
 * specifically, how to use a purely synchronous region interface
 * (with well defined epochs for both streams and regions).  You may
 * select which example to run by changing the "main =" line at the
 * bottom of the file.
 */ 
/*============================================================================*/

// Example 1 : Basic region primitives.

// First we build a list of filenames, these are our data sources.
dataset1 = List:build(5,fun(i) ("dat/"++ i+1 ++".dat"));

// readDataSet will construct a region by reading all of these files.
// Each file represents the sensor stream for each node.
reg1 = (readDataSet(dataset1) :: Region Int);

// Now let's add one to each reading:
reg2 = rmap(fun(x) x+1, reg1);

// Next we aggregrate all the readings across the region:
sums = rfold((+), 0, reg2);

// Then we broadcast the stream of sums back across the region,
// matching up each sum with the original sensor readings:
reg3 = disseminate (sums, reg1);

// Finally, we dump the resulting region out of the network as a stream:
example1 = rdump(reg3);

/* Supposing that the dataset consisted of identical files with
 *  ascending integral sensor readings: 1,2,3... We'll see an output
 *  of the form:

(<nodeid>, (<summed-readings-for-epoch>, <orig-reading-for-epoch>))

For example:

(1004, (10, 1))
(1003, (10, 1))
(1002, (10, 1))
(1001, (10, 1))
(1000, (10, 1))
(1004, (15, 2))
(1003, (15, 2))
(1002, (15, 2))
...
*/

/*============================================================================*/

// Example 2: Filter out the output of just nodes with odd IDs.

fun is_odd(n) { (n/2) * 2 != n }

filt = rfilter(fun((id,x)) is_odd(id), rids(reg1));

// Strip those extra IDs off and dump it:
example2 = rdump(rmap(fun((_,x)) x, filt));

/*============================================================================*/

// Example 3: Average the readings in a region.

fun avg(reg) {
  smap(fun((sum,n)) sum/n,
       rfold(fun((sum,n), x) (sum+x,n+1), (0,0), reg))
}

// In this case we'll average the sum of node ID and sensor reading:
ids = rmap(fun((id,x)) id+x, rids(reg1));

// The result will depend on how many nodes we have.
example3 = avg(ids);


/*============================================================================*/

/*
// This buffers until the signal reaches a peak and starts coming 
// down again. It's very inefficient.

fun delay_until_peak(strm) 
 iterate x in strm {
   state { buf = [] }
   ;
 }
*/

/*============================================================================*/


// Here you can set the return value to the stream you would like to output:
main = example3

