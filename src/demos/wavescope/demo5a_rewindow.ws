
// This does a rewindow manually.  That is, without defining a
// separate function and using the inliner.

// This serves as a test of sigseg operations.

fun assert_eq(s,a,b) if not(a==b) then wserror("Assert failed in '"++s++"' : "++ a ++" not equal "++ b);

s1 = (readFile("./countup.raw", "mode: binary  window: 4096") :: Stream (Sigseg Int16));

newwidth = 1024;
step = 512;

s2 = iterate (win in s1) {
   state { acc = nullseg; 
         }

   v :: Sigseg () = nullseg;
   assert_eq("nullseg eq1", v,v);
   assert_eq("nullseg eq2", v, (nullseg :: Sigseg ()));
   assert_eq("nullseg eq3", (nullseg :: Sigseg ()), (nullseg :: Sigseg ()));

   print("\nIncoming width ");
   print(win`width);
   print(" Current ACC/width ");
   print(acc`width);
   print(": ");
   print(acc);
   print("\n");


   newacc = joinsegs(acc, win);
   assert_eq("join", newacc`width, win`width + acc`width);
   acc := newacc;

   // We do this entirely with index numbers, no abstract Time objects are used.
   // win.width is an upper bound on how many windows we can produce.

   print("JOINED Current ACC/width ");
   print(acc.width);
   print(": ");
   print(acc);
   print("\n");

   while acc.width > newwidth {
     print("Iterating, acc.width " ++ acc.width ++ "\n");
     chop = subseg(acc, acc.start, newwidth);
     emit chop;

     leftover = subseg(acc, acc.start + step, acc.width - step);
     assert_eq("split", acc`width, chop`width + leftover`width - step);
     acc := leftover;
   }
};

BASE <- s2;
