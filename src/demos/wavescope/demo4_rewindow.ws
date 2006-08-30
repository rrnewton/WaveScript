
// This does a rewindow manually.  That is, without defining a
// separate function and using the inliner.

s1 = audioFile("./countup.raw", 4096, 0);

newwidth = 1024;
step = 512;

s2 = iterate (w in s1) {
   state { acc = nullseg; }

   print("\nCurrent ACC, width ");
   print(acc.width);
   print(": ");
   print(acc);
   print("\n");

   acc := joinsegs(acc, w);
   // We do this entirely with index numbers, no abstract Time objects are used.
   // w.width is an upper bound on how many windows we can produce.

/*    for i = 1 to w.width { */
/*      if acc.width > newwidth */
/*      then {emit subseg(acc, 0, newwidth); */
/* 	   acc := subseg(acc, step, acc.width - step)} */
/*      else break; */
/*    } */
  
   if acc.width > newwidth
   then {emit subseg(acc, 0, newwidth);
         acc := subseg(acc, step, acc.width - step) }
   else {};
};

BASE <- s2;
