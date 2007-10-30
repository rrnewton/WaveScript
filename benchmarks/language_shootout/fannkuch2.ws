
// With gcc, -O0, this takes 13.16s user on justice. (But the other processor is being used up.)
//   With -O2 it takes 3.6s.
//   With -O3 it takes 3.63s.

// ============================================================
// User time for WS on justice: (still one processor hosed):
// ws:      204s
// ws.opt:  179.5s

// ============================================================
// wsmlton: 10.8s (faith) 12.0 when the system is under other load...

// FIXME:
// wsmlton, hacked to use gcc rather than native code? - 17.689 (under other load, -O1)
//   with -O3 ...~16.6 cpu (under other load)

// ============================================================
// [2007.10.16] WSC is currently insanely inefficient... it produces
// aliases for ARRAY valued variables.  This results in a bunch of
// unnecessary reference counting inside the inner loops.

// wsc:     161s

// Well, this means we need to fix ws-remove-complex-opera*.  But in
// the meantime, one can simply lift those array bindings out to top
// level. That brings us to:

// wsc:     48.1s

// Ok, fixing ws-remove-complex-opera*... that should be about the same:

// wsc:     47.3
// wsc -O2:  5.92s 
// wsc -O3:  4.92s 


// NICE: good to see it beat MLton.


include "stdlib.ws";
using Array;
using Mutable;


//arg = 7;
arg = 11; // The input used on the language shootout website.


//r        = ref(0);didpr    = ref(0);flipsMax = ref(0);

//#define XCH(x,y)	{ Aint t_mp; t_mp=(x); (x)=(y); (y)=t_mp; }
BASE <- iterate _ in timer(100.0) {
  state {

perm  = make(arg,0);
perm1 = build(arg, fun(x) x);
count = make(arg,0);

    rr        = 0;
    k        = 0;
    didpr    = 0;
    flipsMax = 0;
    flips    = 0;

    magic1 = true;    
    magic2 = true;    
    first = true;
  }
  
  fun fannkuch( n ) {
    n1 = n - 1;    
    fun showstate() {
      "r:"++rr++" didpr:"++didpr++" flipsmax:"++flipsMax++ " perm:"++perm++" perm1:"++perm1++" count:"++count
    };

    if n<1 then 0 else {
     magic1 := true;
     rr := n; didpr := 0; flipsMax := 0;
     while magic1 {
       //println(" Looping magic1 "++showstate());
	if didpr < 30 then {
	    for i = 0 to n1 { print(show(1+perm1[i])) };  print("\n");
	    //println("CURRENT PERM: "++perm1);
	    didpr += 1;
	};
	for i = 2 to rr { count[i-1] := i }; 
	rr := 1;
	//println("Initialized Count: "++count);

        // Do the flips and count them up.
	if not(perm1[0]==0 || perm1[n1]==n1) then {
	    flips := 0;
	    for i = 1 to n1 { perm[i] := perm1[i] };

	    k := perm1[0];		/* cache perm[0] in k */

	    fun once() {
 	        i = ref(1);
	        j = ref(k-1);
		while i<j {
		  { swp = perm[i]; perm[i] := perm[j]; perm[j] := swp }; // SWAP
		  i += 1;
		  j -= 1;
		};
		flips += 1;

		/*
		 * Now exchange k (caching perm[0]) and perm[k]... with care!
		 * XCH(k, perm[k]) does NOT work!
		 */
		j := perm[k]; perm[k] := k ; k := j;
	    };

   	    /* k!=0 ==> k>0 */	    
	    once(); while k != 0 { once() }; // LAME version of do/while.

	    if( flipsMax < flips ) then { flipsMax := flips; };
	};
	
	magic2 := true;
        // Generate the next permutation for the next time around.
	while magic2 {
	  //println("  Looping magic2 r:"++r++" didpr:"++didpr++" flipsmax:"++flipsMax++" perm:"++perm++" perm1:"++perm1);
	    if( rr == n ) then {
  	        // break magic1, return flipsMax.
	      magic2 := false;
      	      magic1 := false;
	      () //return flipsMax;
   	    } else {
	    /* rotate down perm[0..r] by one */
	    {
		perm0 = perm1[0];
		i = ref(0);
		while( i < rr ) {
		  //println("Doing rotation... i:"++i++" r:"++rr);
		    k := i+1;
		    perm1[i] := perm1[k];
		    i := k;
		};
		perm1[rr] := perm0;
	    };
	    count[rr] := count[rr] - 1;
	    //println("  Count[r] = "++count[r]);
	    if( count[rr] > 0 ) then {
	      //print("\nGetting out of loop2 but not loop1!!!\n");
	      magic2 := false;
	      () //break; // break magic2
	    } else { rr += 1 }
	    }
	};
	()
        //println("    Next PERM r:"++r++" didpr:"++didpr++" flipsmax:"++flipsMax++" perm:"++perm++" perm1:"++perm1);
    };
    flipsMax
    //max(flipsMax,max(didpr,r));
  }
  };

  if first then {
    print("\nRunning!\n");
    strt = clock();
    println("Ran for "++arg++": "++fannkuch(arg));
    ellapsed = clock() - strt;
    println("CPU ticks elapsed: "++ellapsed);
    first := false;
    emit ();
  } //else emit ();  
}


    
 /*

  */


/*

BASE <- ONCE(fun() {
  print("\nRunning!\n");

  //  println("r: "++r);  println("p: "++p);  println("s: "++s);

  println("Ran for 7: "++fannkuch(7));  
})


*/
