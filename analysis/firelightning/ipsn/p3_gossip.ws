

thresh = 20;

fun temp(n) sense("temp", n);
fun abovethresh(n) temp(n) > thresh;
fun count_nbrs(r) {
  rfold( (+), 0, 
	 rmap(fun (_) 1, 
	      rfilter(fun (n) temp(n)>thresh, r)))
}

// All nodes over a local temperature threshold.
heat_events = rfilter(abovethresh, world);

// Here we gossip our temp values:
// This returns a region of streams



fun local_results(n) {
  hood = khood(node_to_anchor(n), 1);
  //smap(fun(n) n, count_nbrs(hood));
  count_nbrs(hood)
}
