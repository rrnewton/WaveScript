
// This creates some heavyweight worker boxes that do a million sqrts.

// On my nokia Core2 duo 3ghz:
// 1000 tuples with ops = 1 million takes 61.3s real on 2 threads
// In single threaded mode it takes 100.6 seconds

// That is not quite the perfect speedup that one would expect on something ilke this.

/* 

  [2008.08.29] Testing on honor.  I have no good way to shut down
  processors, except to run infinite loops to shutdown processors.

  Disabled threads gcc -O3 -gc rc -n 500: 
             31.2s 
   1 cpu:  17.1
   2 cpus: 17.0
   3 cpus: 16.7
   4 cpus: 16.7 -- but cpu time only 31.5 -- not keeping them exercised?

  I am going to redo this with 12 boxes because it divides by 2,3, and 4.
  It is possible that 3 and four threads are being broken by the relative primality.
  Hmm, it looks like that upped CPU utilization but time was constant.

  Oh duh, the timer is set too low.  Tripling it.  Oh, grr, this
  infinite loop methodology isn't working.  I would have to change the
  nice values.  Let's try that.
              
   1 cpu:  37.4   37.5  2829 left in queues
   2 cpus: 18.6   43.1  1152 left in queues
   3 cpus: 14.1   42.3 (query.exe nice 5)
   4 cpus: 10.9 / 43.5, full cpu utilization

I also got this inconsistent data point... what when wrong?
Hmm.. it looks like some of the times one of these threads falls off the intended core...
I need to do some pinning, or more precisely, anti-pinning.
   1 cpu:  23.5   39.8  1587 left in queues

[2008.09.01] {Trouble with pin2cpuRange}
  Trying a new method of restricting the numbers of CPUs used, using
  linux thread affinity.  But oddly, this is doing very poorly.

   2 cpus: 63 seconds

  The unthreaded version still works.  And the threaded version
  without -limitcpus works.  With -limitcpus set to 4, it still
  saturates all four processors, and it takes 31s as opposed to 11
  seconds.  Oh my, this scheduling restriction must have some serious
  costs...

  Ok, going back to my approach of simply keeping other processors
  busy.  I wrote a script, run_with_n_cpus that automates the process.
  But it doesn't work very well.  When I use nice values of 0 and 5, I
  still get the WS threads leaking off onto the other CPUs (and it
  takes 39s w/ ~2 cpus).  Using nice of 0 and 19 results in what
  appears to be the correct cpu-assignment behavior... But wait...  I
  note that it took a while to get up to 300% cpu utilization at
  3-cpus.  Also, at 2 cpus, it really only used one!  These nice
  numbers aren't working right.

   nice 5:  
   nice 7:  _    27    13.95      11s
   nice 10: 37.8 28.3  20.1/15.6  10.5 -- only used 2 for 3-cpus, on a second run it used ~260
   nice 19: 

  Even if there's a nice answer for a particular program, basically
  there's no chance that this could ever work reliably across
  different conditions -- machines, numbers of threads, etc.


   

 */

fun trylookup(str,def) {
  if GETENV(str) == "" 
  then def
  else stringToInt(GETENV(str));
}


ops = 1 * 1000 * 1000
//workers = trylookup("WORKERS", 16)
workers = 16 // Stay constant.
rate = 9
amplify = 10

fun work(fl) {
  x = Mutable:ref(fl);
  for i = 0 to ops {
    x := sqrtF(x);
    x := x*x;
  }; 
  x
}

src = iterate _ in timer(rate) {
  for i = 1 to amplify {  emit 99  }  
}

main = {
  fun f(n,s) 
    if n == 0 then s else
    iterate x in f(n-1,s) { emit work(x) };
  f(workers, src)
}
