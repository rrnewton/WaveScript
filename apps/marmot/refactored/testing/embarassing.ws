
// This is embarassingly parallel.

/* 
   [2007.11.12] Running on AWD, trying to get SOMETHING to speed up 16X

I'm making a split/join with 16 boxes and then running it on up to 16 threads.

First I tried with my current version of "parmap".


1  1297.0  2020.0
2  849.0  1986.0
3  719.0  2077.0
4  776.0 
5  676.0  2562.0
6  526.0  2429.0
7  509.0  2444.0
8  498.0  3243.0
9  435.0  2407.0
10  407.0  2477.0
11  410.0  2569.0
12  410.0  2460.0
13  411.0  2431.0
14  437.0  2429.0
15  406.0  2542.0
16  396.0  3921.0


Then I tried something even SIMPLER.  just a plain old split join with workers. But this did even WORSE.  

#h numthreads realtime cputime
1  1539.0  2783.0
2  1449.0  4009.0000000000005
3  1322.0  5111.0
4  1379.0  6633.0
5  1374.0  7978.0
6  1240.0  8270.0
7  1332.0  9933.0
8  1030.0  8827.0
9  1051.0  10215.0
10  1065.0  11453.0
11  1108.0  13030.0
12  1077.0  13699.0
13  1044.0  14342.0
14  1026.0  15126.0
15  1026.0  16075.0
16  874.0  13760.0

AH, this interesting.  If I do 15 workers, then the whole thing
becomes *very* happy when it hits 16 cpus and the split/joiner is all alone.

#h numthreads realtime cputime
1  2351.0  4403.0
2  2201.0  6312.0
3  2068.0  7710.0
4  1818.0  8710.0
5  1849.0  10740.0
6  1671.0  11350.0
7  1619.0  12572.0
8  1186.0  10396.0
9  1252.0  12214.0
10  1316.0  13969.0
11  1211.0  14272.0
12  1188.0  15134.0
13  1303.0  17618.0
14  1212.0  17775.0
15  1186.0  18520.0
16  192.0  2839.0

Now this is more like it.  Made the program always reserve a cPU for the splitter joiner.
Then, if we run with 1-15 worker threads:

#h numthreads realtime cputime
2  23981.0  43996.0
3  12528.0  35438.0
4  8077.0  30802.0
5  5958.0  28988.0
6  4767.0  27700.0
7  3950.0  26910.0
8  3320.0  25999.0
9  2920.0  25819.0
10  2657.0  25967.0
11  2362.0  25379.0
12  2161.0  25416.0
13  1971.0  25161.0
14  1915.0  26154.0
15  1730.0  25353.0
16  1591.0  24477.0

That's almost a perfect speedup:

  0: 1.0
  1: 1.9141922094508301
  2: 2.969047913829392
  3: 4.025008392077878
  4: 5.030627228865114
  5: 6.071139240506329
  6: 7.223192771084338
  7: 8.212671232876712
  8: 9.025592773805043
  9: 10.152836579170195
  10: 11.09717723276261
  11: 12.166920345002536
  12: 12.522715404699738
  13: 13.861849710982659
  14: 15.072910119421747

Stan tweaked the DF scheduler so that it never does a DF call before an enqueue.

Speed up a 2-way split:  That works as far as it goes.

#h numthreads realtime cputime
1  25740.0  41064.0
2  11566.0  29869.0
3  13633.0  35296.0
4  12431.0  32436.0
5  13528.0  34537.0
6  13547.0  35133.0
7  12274.0  31941.0
8  12506.0  32247.0
9  13360.0  34412.0
10  13270.0  34372.0
11  13295.0  34173.0
12  13435.0  34594.0
13  13215.0  34201.0
14  12766.0  33055.0
15  13279.0  34389.0
16  13304.0  34699.0

Speed up a 16-way split.  That fails much like before:

#h numthreads realtime cputime
1  24563.0  45151.0
2  21697.0  61499.0
3  19742.0  75451.0
4  19143.0  92499.0
5  19081.0  111088.0
6  17932.0  122315.0
7  17320.0  136004.0
8  15466.0  136451.0
9  15428.0  151876.0
10  16645.0  178596.0
11  17248.0  202709.0
12  15359.0  196704.0
13  15323.0  211827.0
14  15248.0  226074.0
15  15312.0  241440.0
16  13176.0  208940.0

It also looks like the WSSoure thread is using 100% cpu.  Because when it runs -j 5 it uses 600%, and so on.

OOPS. DUH, we overwrote the macro.  Ok, copied it but still no good.

#h numthreads realtime cputime
1  22922.0  43465.0
2  22063.0  60799.0
3  20374.0  77861.0
4  19081.0  92673.0
5  19545.0  112926.99999999999
6  18450.0  125662.0
7  17786.0  138890.0
8  15571.0  137443.0
9  15391.0  151310.0
10  15427.0  167016.0
11  15473.0  182841.0
12  15527.0  197966.99999999997
13  15386.0  212918.0
14  15368.0  227666.0
15  15314.0  241604.0
16  11412.0  181091.0


Ok, trying with PARMAP now. (and with the lonely-splitter hack).
First trying BEFORE updating to stan's recent push/pull fix.  This
looks right.  It hits synergy only when there are 15 workers.  It's
stuck in a rigid round robin regime rather than work-stealing.

OOPS, this was with 16 boxes... INTERESTING...  How does that work.
It should only be nice for 15 boxes.

Ack crap, what revision was this???  It was before 1737.  I believe
1736.. I had already updated to try stans fix to the emit DF/queue
issue.

#h numthreads realtime cputime
1  25062.0  38276.0
2  14629.0  36190.0
3  9223.0  32204.0
4  7750.0  34837.0
5  6348.0  34067.0
6  6148.0  39667.0
7  6525.0  48001.0
8  4378.0  37067.0
9  4347.0  41126.0
10  4614.0  47904.0
11  4455.0  50710.0
12  4329.0  53480.0
13  4306.0  57432.0
14  4319.0  61725.0
15  1536.0  22954.0
16  2876.0  44755.0


When I updated to 1738... if I don't put a repeater in I get serialized behavior:

#h numthreads realtime cputime
1  22905.0  22877.0
2  41098.0  41056.0
3  44989.0  44934.0
4  44997.0  44914.0
5  44997.0  44901.0
6  44995.0  44920.0
7  43018.0  42928.0
8  45006.0  44885.0
...

Damn.  Turning on a repeater that spits out 15 at a time doesn't solve the problem at all.

#h numthreads realtime cputime
1  22847.0  22815.0
2  41845.0  41803.0
3  45313.0  45241.0

Even if I pump the repeater up to 100, it doesn't work at all.  I
don't think the fix to DF emits is working...  

Went back to 1736 and things work fine.  Here's 15 workers on 1-16 threads.

#h numthreads realtime cputime
1  24264.0  36945.0
2  13598.0  33660.0
3  8183.0  28288.0
4  6736.0  30640.0
5  4800.0  26147.0
6  4770.0  30833.0
7  4619.0  34555.0
8  3117.0  26532.0
9  3190.0  30229.0
10  3480.0  36322.0
11  3136.0  35309.0
12  3131.0  38639.0
13  3191.0  42822.0
14  3059.0  43606.0
15  1601.0  24325.0
16  1541.0  23461.0

When I switch to 1737, it becomes totally serialized again.  Even if I use a repeater.

Ok, let me try 1738 with the simpler form of joining (not parmap).
ok, that works... EVEN WITHOUT A REPEATER... 

#h numthreads realtime cputime
1  22928.0  22927.0
2  12067.0  22875.0
3  7701.0  22780.0
4  6106.0  22978.0
5  4731.0  23064.0
6  4585.0  23623.0
7  4566.0  24450.0
8  3099.0  23516.0
9  3225.0  24898.0
10  3086.0  26084.0
11  3091.0  27436.0
12  3076.0  28754.0
13  3054.0  29946.0
14  3117.0  31040.0
15  1646.0  23023.0
16  1608.0  22892.0

I wasn't REALLY using a repeater before.... here's 1738 with parmap with repeater 15.

#h numthreads realtime cputime
1  22879.0  22867.0
2  12666.0  23263.0
3  7715.0  22851.0
4  6432.0  23115.0
5  4653.0  22832.0
6  5092.0  24272.0
7  5122.0  26001.0
8  4535.0  25990.0
9  4573.0  28593.0
10  4470.0  30191.0
11  4517.0  32773.0
12  4392.0  34289.0
13  4478.0  36945.0
14  4287.0  37772.0
15  1572.0  22861.0
16  1585.0  22931.0


NOW I'm testing the "lonely splitter" hack.... it doesn't make a
difference for simplejoin, but for parmap....  Here's parmaps crappy
speedup with split-join.

#h numthreads realtime cputime
1  22896.0  22889.0
2  12547.0  23212.0
3  9803.0  23746.0
4  9642.0  27755.0
5  7205.0  25701.0
6  7317.0  29612.0
7  5797.0  28086.0
8  6766.0  33560.0
9  6762.0  34883.0
10  6585.0  35672.0
11  6715.0  37662.0
12  6583.0  40028.0
13  6629.0  40465.0
14  4458.0  37281.0
15  6474.0  43758.0
16  5934.0  43482.0


*/

include "stdlib.ws";

fun myjoiner(bufsize, slist) {
  using List; 
  len = slist`List:length;
  iterate (ind, elem) in unionList(slist) {
    state { bufs = Array:build(len, fun(_) FIFO:make(bufsize));
            pos = 0; // The next guy in line
           }
    using FIFO;
    //println("joiner received: "++(ind,elem));
    if ind == pos then {
      emit elem;
      pos += 1;
      if pos == len then pos := 0;
      // Relieve any pent-up data
      while not(empty(bufs[pos])) {
        emit dequeue(bufs[pos]);
        pos += 1;	
	if pos == len then pos := 0;
      }
    } else enqueue(bufs[ind], elem);
  }
}

threads = stringToInt(GETENV("NUMTHREADS"))
boxes   = stringToInt(GETENV("NUMBOXES"))

fun assigncpu(i) {
  // HACK
  //moduloI(i,threads-1)+1
  // NORMAL:
  i
}

// Map a function over a stream with N separate instances for pipelining.
//parmap  :: (Int, (a -> b), Stream a) -> Stream b;
fun myparmap(n, fn, src) {
    split = iterate x in src {
      state { cnt = 0 }
      // NEED MODULO:
      //println("Farming out to cpu "++cnt);
      emit (cnt, cnt);
      cnt += 1;
      if cnt == n then cnt := 0;
    };
    routed = List:build(n, 
      fun(i) iterate (ind,x) in split {
        if ind == i then emit x;
      });
    // route before processing so spurious tuples don't go across CPU boundaries.
    processed = List:mapi(fun(i,filtered) 
         SETCPU(assigncpu(i),smap(fn, filtered)), routed);
    // Use zipN for simplicity.
    // Could optimize this a little bit.
    //joined = zipN(10,routed); 
    // Finally, spool them out.
    //iterate arr in joined { Array:foreach(fun(x) emit x, arr)  }
 
    myjoiner(10, processed);
    //joiner(10, routed);
  };



fun work(cpu) {  
  //println("  Doing work on CPU "++cpu);
  var = Mutable:ref(100.0);
  for i = 0 to 1 * 1000 * 1000 {
    var := sqrtF(var) + 2.0;
  }; 
  var
}

plaintimer = timer(10.0)
simplejoin =
smap(fun(_)(),
 unionList(List:build(boxes, 
  fun(i) {
    // Keep em in a sandbox.
    SETCPU(assigncpu(i), smap(work,plaintimer))
  })));


//BASE <- simplejoin;
BASE <- myparmap(boxes, work, repeater(boxes, timer(10.0)))

//BASE <- myparmap(boxes, work, timer(10.0))

