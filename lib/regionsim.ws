
/* 
 * This library enables simulated region operations in WaveScript.
 * 
 * This is a place to experiment before developing an actual regions
 * implementation in WS.
 *
 */

include "stdlib.ws";

/* We read in data as a set of files: */
type DataSet = List String;

/* We use the INDEX in that filename list to generate node IDs. 
 * We add 1000 to distinguish node IDs easily.  
 */
type NodeID = Int;

/* In the simulator, a Region is just a list: */
//type Region t = List (Stream t);

/*  Further, we make the synchronization implicit, so that regions are
 *  pre-synchronized "snapshots":
 */ 
//type Region t = Stream (Array t);
type Region t = Stream (HashTable (NodeID, t));

//foo :: Array (List Int) = Array:make(1,[3]);
//bar :: Array Float = Array:make(1,3.3);
//baz = Array:make(1, wserror(""));

/* ============================================================ */

readDataSet :: DataSet -> Region anytype;

/* Apply a function to each sample in a region: */
rmap :: ((a -> b), Region a) -> Region b;

rfilter :: ((a -> Bool), Region a) -> Region a;

/* Extract the node ID of each member of the region. */
rids :: (Region a) -> Region (NodeID * a);

rfold :: ((acc, a) -> b, acc, Region a) -> Stream b;

//leader_elect :: Region a -> Stream NodeID;

// ====================
// Operations on synchronized streams/regions:

rintersect :: (Region a, Region b) -> Region (a * b);

//runion :: (Region a, Region b) -> Region (a, Option b);
runion :: (Region a, Region b) -> Region (a * ack_cant_write_sum);

disseminate :: (Stream a, Region b) -> Region (a * b);

// ====================
// Asynchronous opoerations:

/* This simply returns all data in a region in a stream */
rdump :: Region a -> Stream (NodeID * a);

/* Broadcast a stream over the entire network */
//disseminate :: Stream t -> Region t;

/* Merge the event streams from two different regions. */
//rmerge :: (Region a, Region a) -> Region a;


//ravg 

/* ============================================================ */

//fun worldtimer(freq) {}


/* Scrapping Stream-of-Array version: */
/*
fun rmap(fn, reg) {
  using Curry;
  List:map(smap(fn) ,reg);
}

fun rmap(fn, reg) {
  smap(fun(arr) Array:map(fn,arr), reg);
}

// Calls to this function should always be type-annotated!
fun readDataSet(files) {
  fun foo(name) 
     // This is a naughty abuse of the type system:
    (readFile(name, "", timer(10)) :: Stream unknowntype);
  zipN(DEFAULT_ZIP_BUFSIZE, List:map(foo, files));
}

*/

fun rmap(fn, reg) {
  smap(fun(table) HashTable:map(fun(_,x) fn(x),table), reg);
}

fun rfilter(pred, reg) {
  using HashTable;
  smap(fun(tbl) {
    new = make(size(tbl));
    foreach(fun(key,x) {
      if pred(x)//pred(key,x)
      then set(new, key, x)
    }, tbl);
    new;
  },reg);
}

fun rids(reg) {
  smap(fun(table) HashTable:map(fun(id,x) (id,x),table), reg);
}

fun rfold(fn, zer, reg) {
  smap(fun(tbl) {
    acc = Mutable:ref$ zer;
    HashTable:foreach(fun(_,x) acc := fn(acc,x), tbl);
    acc
  }, reg);
}


fun rdump(reg) {
  iterate tbl in reg {
    HashTable:foreach(fun(key,x) emit(key,x), tbl);
  }
}

fun disseminate(stm, reg) {
  smap(fun((elem,tbl)) {
    HashTable:map(fun(id,x) (elem,x), tbl);
  }, zip2(stm,reg));
}

fun runion(r1, r2) {
  smap(fun((tbl1,tbl2)) {
    using HashTable;
    new = make(size(tbl1));
    foreach(fun(key,x) {
      if contains(tbl2,key)
      then set(new, key, (x, Some(get(tbl2,key))))
      else set(new, key, (x, None(())))
    }, tbl1);
    new;
  }, zip2(r1,r2));
}


fun rintersect(r1, r2) {
  smap(fun((tbl1,tbl2)) {
    using HashTable;
    new = make(size(tbl1));
    foreach(fun(key,x) {
      if contains(tbl2,key)
      then set(new, key, (x,get(tbl2,key)))
    }, tbl1);
    new;
  }, zip2(r1,r2));
}


// Calls to this function should always be type-annotated!
fun readDataSet(files) {
  fun foo(name) 
     // This is a naughty abuse of the type system:
    (readFile(name, "", timer(10)) :: Stream unknowntype);
  //id_arr = List:toArray(List:map(NodeID, files));
  id_arr = List:toArray(files);
  arrs = zipN(DEFAULT_ZIP_BUFSIZE, List:map(foo, files));
  smap(fun(arr) {
    using HashTable;
    tbl = make(Array:length(arr));
    //Array:foreachi(fun(i,id) set(tbl, id, arr[i]), id_arr);
    Array:foreachi(fun(i,x) set(tbl, 1000 + i, x), arr);
    tbl
    //pairs = List:map2(fun(x,y) (x,y), files, Array:toList(arr));
    //HashTable:fromList(pairs);
  }, arrs);
}

/* ============================================================ */

namespace Region {
  map    = rmap;
  filter = rfilter;
  ids    = rids;
  fold   = rfold;
  dump   = rdump;
  union  = runion;
  intersect = rintersect;    
}


/* ============================================================ */
