

/* 

 [2008.11.01] This represents a good case for deferred reference
 counting.  Most allocation doesn't escape, but a little bit does.
 (Can't just region allocate.)

First run, 500K tuples, bufsize 100.  1GB of allocation, icc.
  wsc2 boehm    - 2.5s  ~260ms collect  (2.56 gcc)
  wsc2 refcount - 2.5s                  (1.96 gcc)
  wsc2 deferred - 2.0s                  (2.0 gcc)
  wsc2 no gc    - 2.1s
  wsmlton       - 4.0s  ~180ms collect maxlive 20K
  wscaml        - 0.5s

  (scheme ikarus, ~88s)
  (scheme chez O2, 225ms?? using skip... no 40s using -t)

Hmm... just throwing in random number generation adds .5 s to wsc2 but brings caml up to 2.8s?
Only brings mlton up to 4.3s.

As long as the set of live data is small, copying collection will be
very fast.




 */

bufsize = if GETENV("BUFSIZE") == "" then 100 else stringToInt(GETENV("BUFSIZE"));

_ = print("Running with bufsize "++ bufsize ++"\n")

using List;

fun last_cell(ls) {
  ptr = ls;
  while not(ptr.tail.is_null) {
    ptr := ptr.tail;
  };
  ptr
}

// fun insert_sorted(cell, ls) { }

fun worker(s) 
  iterate _ in s {
    ls = List:build(bufsize, fun(i) randomI(1000));
    emit last_cell(ls);
  }

main = iterate cell in worker(timer(1000)) {
  emit cell.head;
}
