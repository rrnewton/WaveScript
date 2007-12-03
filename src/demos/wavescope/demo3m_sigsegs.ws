
// Several demos up till now have read in a stream of Sigsegs.
// But this demo stresses the various sigseg operators.


include "common.ws";

s1 = (readFile("countup.raw", "mode: binary window: 100", timer(10.0)) :: Stream (Sigseg Int16))

main = iterate w in s1 {
   state { pos = gint(0) }

   //========================================
   // Test nullsegs

   v :: Sigseg Int16 = nullseg;
   assert_eq_prnt("nullseg eq1", v,v);
   assert_eq_prnt("nullseg eq2", v, (nullseg :: Sigseg Int16));
   assert_eq_prnt("nullseg eq3", (nullseg :: Sigseg Int16), (nullseg :: Sigseg Int16));


   //========================================
   // API basics

   assert_eq_prnt("self eq", w,w);
   assert_prnt("not eq",  not(w==v));   

   assert_eq_prnt("width",  w`width, 100);     

   print(w`start ++"\n");
   assert_eq_prnt("start",  w`start, pos);
   assert_eq_prnt("end",    w`end, pos+gint(99));
   pos := pos + gint(100);     

   //========================================
   // Split into two pieces

   fst = subseg(w, w`start, 50`gint);
   snd = subseg(w,w`start + 50`gint,50`gint);

   assert_eq_prnt("fst width",  fst`width, 50);
   assert_eq_prnt("snd width",  snd`width, 50);

   assert_prnt("fst snd inequal", not(fst==snd));
   assert_prnt("fst w inequal", not(fst==w));
   assert_prnt("snd w inequal", not(snd==w));

   assert_eq_prnt("fst start",  fst`start, w`start);
   assert_eq_prnt("snd start",  snd`start, w`start + 50`gint);

   assert_eq_prnt("fst lookup", fst[[5]], w[[5]]);
   assert_eq_prnt("snd lookup", snd[[5]], w[[55]]);

   joined = joinsegs(fst,snd);

   assert_eq_prnt("joined width",  joined`width, 100);
   assert_eq_prnt("joined end",    joined`end, w`end);

   for i = 0 to 99 {
     assert_eq_prnt("joined same "++i, joined[[i]], w[[i]])
   };
   assert_eq_prnt("joined same total", w, joined);   

   //========================================
   // Take a chunk in the middle 

   mid = subseg(w, w`start + 25`gint, 50`gint);
   assert_eq_prnt("mid lookup",  mid[[30]], snd[[5]]);
   assert_eq_prnt("mid lookup2", mid[[29]],   w[[54]]);
   
   //========================================
   // Converting to arrays.

   arr  = toArray(w);
   back = toSigseg(arr, w`start, w`timebase);

   for i = 0 to 99 {
     assert_eq_prnt("toarray",  w[[i]], arr[i]);
   }
   
   assert_eq_prnt("roundtrip",  back, w);

   //arr2 = Array:fromList([1,2,3]);
   //tosig = toSigseg(arr2, intToInt64(99), 318);

   //========================================
   // Split into several small, overlapping chunks.

   chunks = Array:make(10,nullseg);

   for i = 0 to 8 { 
     // These chunks ovelap by one
     chunks[i] := subseg(w, w`start + (i`intToInt64 * 10`gint), 11`gint);
   };
   chunks[9] := subseg(w, w`start + 90`gint, 10`gint);
   //   inspect$ chunks;
   for i = 0 to 8 {
     assert_eq_prnt("chunks width", chunks[i]`width, 11);
     assert_prnt("chunks start", chunks[i]`start >= w`start);     
     ch = chunks[i];
     ch2 = chunks[i+1];
     assert_eq_prnt("chunks overlap", ch[[10]], ch2[[0]]);
     //     ch3 = joinsegs(ch,ch2);
   };
   
   emit "succeeded";
}

