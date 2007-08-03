
// Several demos up till now have read in a stream of Sigsegs.
// But this demo stresses the various sigseg operators.

fun assert_eq(s,a,b) if not(a==b) then wserror("Assert failed in '"++s++"' : "++ a ++" not equal "++ b);
fun assert(s,bool)   if not(bool) then wserror("Assert failed in '"++s++"' ");


s1 = (readFile("countup.raw", "mode: binary window: 100", timer(10.0)) :: Stream (Sigseg Int16))

BASE <- iterate w in s1 {
   state { pos = 0 }

   //========================================
   // Test nullsegs

   v :: Sigseg Int16 = nullseg;
   assert_eq("nullseg eq1", v,v);
   assert_eq("nullseg eq2", v, (nullseg :: Sigseg Int16));
   assert_eq("nullseg eq3", (nullseg :: Sigseg Int16), (nullseg :: Sigseg Int16));


   //========================================
   // API basics

   assert_eq("self eq", w,w);
   assert("not eq",  not(w==v));   

   assert_eq("width",  w`width, 100);     

   print(w`start ++"\n");
   assert_eq("start",  w`start, pos);
   assert_eq("end",    w`end, pos+99);
   pos := pos + 100;     

   //========================================
   // Split into two pieces

   fst = subseg(w,w`start,50);
   snd = subseg(w,w`start + 50,50);

   assert_eq("fst width",  fst`width, 50);
   assert_eq("snd width",  snd`width, 50);

   assert("fst snd inequal", not(fst==snd));
   assert("fst w inequal", not(fst==w));
   assert("snd w inequal", not(snd==w));

   assert_eq("fst start",  fst`start, w`start);
   assert_eq("snd start",  snd`start, w`start+50);

   assert_eq("fst lookup", fst[[5]], w[[5]]);
   assert_eq("snd lookup", snd[[5]], w[[55]]);

   joined = joinsegs(fst,snd);
   
   assert_eq("joined width",  joined`width, 100);
   assert_eq("joined end",    joined`end, w`end);

   for i = 0 to 99 {
     assert_eq("joined same "++i, joined[[i]], w[[i]])
   };
   assert_eq("joined same total", w, joined);   

   //========================================
   // Take a chunk in the middle 

   mid = subseg(w, w`start + 25, 50);
   assert_eq("mid lookup",  mid[[30]], snd[[5]]);
   assert_eq("mid lookup2", mid[[29]],   w[[54]]);

   arr  = toArray(w);
   back = toSigseg(arr, w`start, w`timebase);

   for i = 0 to 99 {
     assert_eq("toarray",  w[[i]], arr[i]);
   }
   
   assert_eq("roundtrip",  back, w);

   //========================================
   // Split into several small, overlapping chunks.
   
   chunks = Array:make(10,nullseg);

   for i = 0 to 8 { 
     // These chunks ovelap by one
     chunks[i] := subseg(w, w`start + (i*10), 11);
   };
   chunks[9] := subseg(w, w`start + 90, 10);
   //   inspect$ chunks;
   for i = 0 to 8 {
     assert_eq("chunks width", chunks[i]`width, 11);
     assert("chunks start", chunks[i]`start >= w`start);     
     ch = chunks[i];
     ch2 = chunks[i+1];
     assert_eq("chunks overlap", ch[[10]], ch2[[0]]);
     //     ch3 = joinsegs(ch,ch2);
   };
   
   emit "succeeded";
}

