// Optimizer microbench.
// This is a chain of redundant rewindows.
// Ideally it should be a single rewindow.

include "stdlib.ws";

reps = 100;

src = iterate _ in timer(3.0) { state{cnt=0} cnt += 1; emit cnt };
windowed = window(src, 10);

BASE <- 
 List:fold(
    fun(strm,wid) rewindow(strm, wid, 0), 
    windowed,
    List:build(reps, fun(i) i + 1 ))

