
fun smap_(f,s) {
  iterate (x in s) {
    emit f(x);
  }
}


flag = GETENV("WSARCH") == "ENSBox";
chans = (dataFile("foo", "binary", 44000, 0) :: Stream (Int * Int * Int * Int));
ch1 = if flag then audio(0,4096,0,44000) else window(smap_(fun((a,_,_,_)) intToFloat(a), chans), 4096);
ch2 = if flag then audio(1,4096,0,44000) else window(smap_(fun((_,b,_,_)) intToFloat(b), chans), 4096);
ch3 = if flag then audio(2,4096,0,44000) else window(smap_(fun((_,_,c,_)) intToFloat(c), chans), 4096);
ch4 = if flag then audio(3,4096,0,44000) else window(smap_(fun((_,_,_,d)) intToFloat(d), chans), 4096);


/* let (ch1,ch2,ch3,ch4) =  */
/*  if GETENV("WSARCH") == "ENSBox" */

/*  // Then we're live: */
/*  then (audio(0, 4096, 0, 44000), */
/*        audio(1, 4096, 0, 44000), */
/*        audio(2, 4096, 0, 44000), */
/*        audio(3, 4096, 0, 44000)) */

/*  // Otherwise we play back from sample data: */
/*  else {chans = (dataFile("foo", "binary", 44000, 0) :: Stream (Int * Int * Int * Int)); */
/*        //chans as (a,b,c,d) = ... */
/*        (window(smap_(fun((a,_,_,_)) intToFloat(a), chans), 4096), */
/* 	window(smap_(fun((_,b,_,_)) intToFloat(b), chans), 4096), */
/* 	window(smap_(fun((_,_,c,_)) intToFloat(c), chans), 4096), */
/* 	window(smap_(fun((_,_,_,d)) intToFloat(d), chans), 4096)) */
/*  }; */



BASE <- unionList([ch1,ch2,ch3,ch4]);




