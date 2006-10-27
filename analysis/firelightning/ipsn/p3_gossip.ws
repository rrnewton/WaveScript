
// include "stdlib.ws"
// include "lists.ws"

//update = alist_update

//----------------------------------------------------------------------

//type Time = Int
//type Id = Int
//type Table =  (List (Id, (Time, Float)))

thresh = 20;

// Return a tuple of relevent information:
fun read(n) (nodeid(n), sense("time",n), sense("temp", n));
fun abovethresh((id,t,temp)) temp > thresh;

temps = rmap(read, world)

// All entries over a local temperature threshold.
heat_events = rfilter(abovethresh, temps);

// Now share those tuples with our neighbors.
strms :: Area (Int, Int, Int);
strms = gossip(heat_events);

// We accumulate the messages receieved at each node.
/* fun form_table(strm) { */
/*   iterate((this, (id,tm,temp)) in strm) { */
/*     state { table = [] } */
/*     //    table[id] := (tm, temp); */
/*     table := alist_update(table, id, (tm,temp)); */
/*     emit (this, table); */
/*   } */
/* } */

// Pure version:
fun form_table(strm) {
  integrate(fun ((this, (id,tm,temp)), table)
	        ([(this, table)],                     // Return value
		 alist_update(table, id, (tm,temp))), // New table
	    [],    // Starting state
	    strm); // Input stream
}

// This allows us to map over the local *streams*.
// The result is a region of changing tables.
tables :: Area (Node, (List (Int, (Int, Int))));
tables = rmap_localstreams( form_table, strms);

// Now we determine which results need to be sent back to the base station.
table_filt :: (Node, (List (Int, (a, Int)))) -> Bool;
fun table_filt((this, table)) {
  temps = map(fun((_,(_,z))) z, table);
  ids = map(fun((id,_)) id, table);

  // Confirm that there are enough nodes.
  table.listLength >= 2 &&
  // Confirm that the summed temp is large enough
  fold( (+), 0, temps) > 18 &&
  // Confirm that we are the leader and hence can return the result:
  this.nodeid == fold( max, 0, ids)
}

BASE <- rmap(fun((_,tbl)) tbl, 
	     rfilter(table_filt, tables))


// Here we gossip our temp values:
// This returns a region of streams

/* fun local_results(n) { */
/*   hood = khood(node_to_anchor(n), 1); */
/*   //smap(fun(n) n, count_nbrs(hood)); */
/*   count_nbrs(hood) */
/* } */


/* fun count_nbrs(r) { */
/*   rfold( (+), 0,  */
/* 	 rmap(fun (_) 1,  */
/* 	      rfilter(fun (n) temp(n)>thresh, r))) */
/* } */
