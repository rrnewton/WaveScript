
//======================================================================



















// RRN: btw, these long argument lists might be better to use tuples-of-tuples
// to organize the arguments (i.e. a hi/low/alpha tuple and a pre/post tuple).

// Even better, we can use type shorthands to make things more readable:
type HiLowAlpha = <Int, Int, Float>

threshold : Stream <Int, Sigseg a>, HiLowAlpha, <Time, Time>
            -> Stream <Time, Time, Bool>
// This classifies a stream into time segments that are over threshold
// and time segments that are under threshold.  Adjacent time segments are contiguous.
fun threshold(s, hi_thresh, lo_thresh, alpha, pre_pad, post_pad) {
 // Note/TODO: I still use +/- over Time values here, that needs to be fixed:
 return stream_integrate(<v,w> in s) {
   state { thresh_value = 0;
           trigger = False;
           smoothed_mean = 0;
           smoothed_var = 0;
           saved_w = Nullref; }
   if (trigger) {
     if (v < thresh_value) {
       trigger := False;
       emit <saved_w.start - pre_pad, w.end + post_pad, True>;
       saved_w := Nullref;
     }
   } else {
     // ewma : Int, Int, Float -> Int
     mean = ewma(v, smoothed_mean, alpha);
     var = ewma(sqr(v - mean), smoothed_var, alpha);
     std_dev = sqrt(var);
     if (v - mean > hi_thresh * std_dev) {
       trigger := True;
       thresh_value := mean + lo_thresh * std_dev;
       saved_w := w;
     } else {
       smoothed_mean := mean;
       smoothed_var := var;
       // RRN: 0 shouldn't have any meaning as a Time!!
       emit <0, w.start - pre_pad, False>;
     }
   }
 }
}

// RRN: Lewis, accumulate is just sum?
accumulate : Sigseg Int -> Int
fun accumulate(w) {
  sum = 0;
  for i = 0 to w.width - 1 {
    sum := sum + w.[i];
  }
  return sum;
}


//======================================================================
// Now for some bird-detection functions:

// CURRENTLY UNDEFINED VALUES:
//   arrayPhase, sum, fdPhaseShift, classify, antbirdProfile, my_location
// UNDEFINED TYPES:
//   BirdDatabase

profileDetector : Stream (Sigseg Float), Int, Sigseg Float, HiLowAlpha, <Time, Time>
                  -> Stream <Time, Time, Bool>
// This function searches a stream for a given bird profile. 
// It outputs a stream of time-segments annotated with a bool indicating profile-match.
fun profileDetector(s, points, profile, hilowalpha, <pre_pad, post_pad>) {
  <hi_thresh, lo_thresh, alpha> = hilowalpha;
  s2 = rewindow(s, points, points/2);
  s3 = map(specgram,s2); // Let's make this map!
  s4 = stream_integrate(<psd, w> in s3) {
    v = accumulate( mult(psd, profile));
    emit <v,w>;
  }
  return threshold(s4, hilowalpha, <pre_pad, post_pad>);
}

// This is for the four audio windows from a given time interval:
type Fourwins = <Sigseg Int, Sigseg Int, Sigseg Int, Sigseg Int>
// A detection contains: profile, recombine, max_i, and the windows.
type Detection =  <Sigseg Int, Sigseg Int, Int, Fourwins>

amlRecombine : Stream Fourwins, Sigseg ?? -> Stream Detection
fun amlRecombine(s, geometry) {
 return stream_integrate(<w0,w1,w2,w3> in s) {
   profile = new_sigref(360, 0);
   recombine = Nullref;
   max = 0;
   max_i = 0;
   
   F.[0] := FFT(w0);
   F.[1] := FFT(w1);
   F.[2] := FFT(w2);
   F.[3] := FFT(w3);

   for i = 0 to 359 {
     acc = Nullref;
     for j = 0 to 3 {
       acc := sum(acc, ifft(fdPhaseShift(F[j], arrayPhase(j, geometry, D2R(i)))));
       // D2R = Degrees to Radians
       // Arrayphase is a function that computes the phase given an array 
       // geometry, channel number, and incoming angle. 
     }
     profile.[i] = accumulate(acc);

     if (profile.[i] > max) {
       recombine := acc;
       max := profile.[i];
       max_i := i;
     }
   }
   emit <profile, recombine, max_i, <w0,w1,w2,w3>>;
 }
}

// Tagged unions subsume enumerations!!
type Bird = Antbird | Robin | Canary | Dodo
// Or something...

matchBirds : Stream Detection, BirdDatabase -> Stream <Bird, Detection>
fun matchBirds(s, birdDatabase) {
 return stream_integrate(detect in s) {
   <profile, recombine, max_i, fourwins> = detect;
   // HMM based recognition...
   whichbird = classify(birdDatabase, profile);
   emit <whichbird, detect>;
 }
}

//======================================================================
// The rest of the file is a "script" that uses the above function declarations.

// Let's use a list for this (it's at the meta-level).
channels : [Stream (Sigseg Int)]
channels = 
  list_integrate (n in iota(4)) {
    emit sample_audio(n,1024);  // sample channel # n
  }
// iota(n) = [0,1,2 ... n-1]
// In normal functional style that would be:
// map (fun (n) -> sample_audio(n,1024)) iota(4);

possibleBirds : Stream <Sigseg , Sigseg, Sigseg, Sigseg>
possibleBirds = sync4(channels.(0), channels.(1), channels.(2), channels.(3),
		      // rrn: TODO! These pre/post paddings are integers, they need to be 
		      // converted to Time's in the appropriate TimeBase...
		      profileDetect(channels.(0), 512, antbirdProfile, <6, 2, 0.99>, <4800, 4800>),
		      // rrn: Lewis, what is this seconds stream for?
		      seconds(1));


// This doesn't seem to type-check:
_DOAProb = amlRecombine( zip2(possibleBirds, retryBirds), arrayGeometry);

birds = matchBirds(_DOAProb, birdDatabase); 

// Do we want to send the whole chunk of data with the initial hint?
ToNet("BirdHint", AllNodes, 
      stream_integrate(<bird, detection> in birds) {
        // By convention "_" is used as "throw away" in pattern matching.
        <_, _, _, fourwins> = detection;
        <w1, _, _, _> = fourwins;   
        // This 
        emit <my_location, bird, w1.start, w1.end>;
      })

hints = FromNet("BirdHint");
retryBirds = sync4(channels.(0), channels.(1), channels.(2), channels.(3),
                   stream_integrate(<loc, bird, start, end> = hints) {
                     // we'd like to avoid re-checking areas we've checked here...
                     emit(<start,end,true>);
                   }, seconds(8));

OnlyOn(Node1) {
  // Localize based on stream from Hints
  // wait for most hints to arrive before processing them ...
}


//======================================================================
// Scratchpad Area:

// RRN: By the way, for zip2 - zip5 it might make sense to have them
// return these types instead of maybe types:
type Choice a b = Left a | Right b
type ThreeChoice a b c = First3 a | Second3 b | Third3 c
type FourChoice a b c d = First4 a | Second4 b | Third4 c | Fourth4 d

// This might look screwy, but it reflects the fact that only sample
// may occur at a time in the output of zip.  (Which in turn makes the
// pattern matching nicer.)

// Thus, instead of: 
case zip2(s1,s2) of
 | <None, Some x> -> ...
 | <Some y, None> -> ...

// You can write
case zip2(s1,s2) of
 | Left x  -> ...
 | Right y -> ...
// And so on...


===========================================================================
===========================================================================

// Rewritten syntax (LDG)

System Setup:
=============

// Audio(channel, rate, batching) instantiates an audio port
// NOTE: these audio sensors are on each node, so if this query plan 
// covers the whole system, there really are N of them.  We dont have
// to go into this for the paper though.
Audio0 = Audio(0, 48000, 1024);
Audio1 = Audio(1, 48000, 1024);  
Audio2 = Audio(2, 48000, 1024);
Audio3 = Audio(3, 48000, 1024);

// ScopedFloodProtocol();
// (if we talk about network stuff at all)
NearbyNet = ScopedFloodProtocol();

// PersistentLog(), records stream of data persistently
Record = PersistentLog("logfile");

// NOTE: 
//  * making Audio() part of the system setup (i.e. not part of query)
//    makes sense because two queries cannot both specify different rates.
//    although specifying different rates could convert into the max rate
//    plus resampling filters.
//  * things like scopedfloodprotocol() and persistentlog.  clearly these
//    are services that need coordination among different queries.  although
//    it's not clear to me that these can't be specified as part of the
//    query plan and automatically aggregated.
//  * perhaps i am still missing something here...


Query Plan
==========

possibleBirds = sync4(Audio0, Audio1, Audio2, Audio3,
		      profileDetect(channels.(0), 512, antbirdProfile, <6, 2, 0.99>, 
	              <time(seconds, 0.1), time(seconds, 0.1)>,
		      time(seconds, 1)));


// This doesn't seem to type-check:
_DOAProb = amlRecombine( zip2(possibleBirds, retryBirds), arrayGeometry);

birds = matchBirds(_DOAProb, birdDatabase); 

// Do we want to send the whole chunk of data with the initial hint?
ToNet("BirdHint", AllNodes, 
      stream_integrate(<bird, detection> in birds) {
        // By convention "_" is used as "throw away" in pattern matching.
        <_, _, _, fourwins> = detection;
        <w1, _, _, _> = fourwins;   
        // This 
        emit <my_location, bird, w1.start, w1.end>;
      })

hints = FromNet("BirdHint");
retryBirds = sync4(channels.(0), channels.(1), channels.(2), channels.(3),
                   stream_integrate(<loc, bird, start, end> = hints) {
                     // we'd like to avoid re-checking areas we've checked here...
                     emit(<start,end,true>);
                   }, seconds(8));

OnlyOn(Node1) {
  // Localize based on stream from Hints
  // wait for most hints to arrive before processing them ...
}

