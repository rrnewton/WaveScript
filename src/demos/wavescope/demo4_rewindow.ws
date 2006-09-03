
// This does a rewindow manually.  That is, without defining a
// separate function and using the inliner.

s1 = audioFile("./countup.raw", 4096, 0);

newwidth = 1024;
step = 512;

s2 = iterate (w in s1) {
   state { acc = nullseg; 
         }

   print("\nCurrent ACC, width ");
   print(acc.width);
   print(": ");
   print(acc);
   print("\n");

   acc := joinsegs(acc, w);
   // We do this entirely with index numbers, no abstract Time objects are used.
   // w.width is an upper bound on how many windows we can produce.

   for i = 1 to w.width {
     print("Iterating, acc.width ");
     print(acc.width);
     print("\n ");

     if acc.width > newwidth
     then {emit subseg(acc, acc.start, newwidth);
	   acc := subseg(acc, acc.start + step, acc.width - step)}
     else { break; }
     //{for i = 1 to 1 {};}
   }
  
/*    if acc.width > newwidth */
/*    then {emit subseg(acc, acc.start, newwidth); */
/*          acc := subseg(acc, acc.start + step, acc.width - step) */
/* 	} */
/*    else {}; */

};

BASE <- s2;
