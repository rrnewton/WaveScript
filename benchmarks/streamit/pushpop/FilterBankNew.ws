
// This is a WS port of FilterBankNew.str in the StreamIT distribution.

/*
  [2008.02.24] This demo works and produces the same output as the streamit version.

  Using wsmlton, it is currently over 10X slower than the strc
  "cluster" backend (single threaded).  (100ms for 20K outputs vs 1.3s)

  There are many layers of inefficiency currently.  FIFOs, no
  execution scaling, various inefficient uses of sigsegs in
  interleaveSS and elsewhere.  I could start chipping away at this in
  various ways.

  Hack#1: Adjust streamit.ws filters to use up all their input before
          producing output.

   Wow, that made a big difference.  With starting window size 128,
   we're down to 332ms.  This method dynamicyally sets the execution
   scalefactor based on the number of times the pop-rate divides the
   input window size.  It has one major problem: decimating filters,
   which have high pop rate and low push rate.  The better goal is to
   keep the output granularity the same as the input granularity,
   irrespective of whether the filter explodes/implodes the sequence.

   (note: using 1280 winsize doesn't make it any faster)
   
  Hack#2: maintaining constant granularity

   This performs about the same as the previous hack.  It might
   *sometimes* do slightly better.  There's some variance.
   Note: I still haven't fixed splitjoins.
   Note: GC overhead is down to ~2% from >16% before these hacks.

  Hack#3: fixing splitjoin also.

   Hmm, hard to fix it correctly.  Experimented with just hacking in a
   scaling factor.  Didn't help.

 Well, doing some profiling on the MLton code indicates that,
 unsurprisingly, we're calling the sigseg methods heavily.  However,
 the number of entriest into ss_get.loop is only slightly higher than
 ss_get itself, from which I conclude ss_get doesn't have to past the
 first seg very often.  

   

 */

include "streamit.ws";
include "matrix.ws";

using Streamit;
using Mutable;

// Granularity for this demo.
WINSIZE = 128 ;

fun source() {
  raw = iterate _ in timer$ 1 {
    state {
      max = 1000.0;
      current = 0.0;
    }
    emit current;
    if current > max 
    then current := 0.0
    else current += 1.0;
  };
  window(raw, WINSIZE);
}

fun FirFilter(N, COEFF)
  filter(N, 1, 1, fun(peek,pop,push) {
    sum = ref(0);
    for i = 0 to N-1 {
      sum += peek(i) * COEFF[N-1-i];
    }
    pop(1);
    push(sum);
  })

fun Delay_N(N) {
  arr = Array:make(N, 0.0);
  place_holder = ref(0);
  filter(1,1,1, fun(peek,pop,push) {
    push(arr[place_holder]);
    arr[place_holder] := pop(1);
    place_holder += 1;
    if (place_holder == N) then place_holder := 0;
  })
}

fun DownSamp(N)
  filter(N,N,1, fun(peek,pop,push) {
    push(peek(0));
    pop(N);
  })

fun UpSamp(N) 
  filter(1,1,N, fun(peek,pop,push) {
    push(pop(1));
    for i = 1 to N-1 { push(0) }
  })

fun Bank(N,L,H,F) 
 pipeline$ [
   Delay_N(L-1),
   FirFilter(L, H),
   DownSamp(N),
   UpSamp(N),
   Delay_N(L-1),
   FirFilter(L, F)
 ]

fun Branches(N_samp, N_rows, N_col, H, F)
   splitjoin(Duplicate(()),
	     List:build(N_rows, fun(i) {	      
   	        H_ch = Array:build(N_col, fun(j) Matrix:get(H, i,j));
	        F_ch = Array:build(N_col, fun(j) Matrix:get(F, i,j));
	        Bank(N_samp, N_col, H_ch, F_ch);
             }),
             RoundRobin(1))

fun Combine(N) 
  filter(N,N,1, fun(peek,pop,push) {
    sum = ref(0);
    for i = 0 to N-1 { sum += pop(1); }
    push(sum);
  })

fun FilterBank(N_samp, N_ch, N_col, H, F) 
  pipeline $ [
    Branches(N_samp, N_ch, N_col, H, F),
    Combine(N_samp)
  ]

fun FilterBankNew() {
   N_sim  = 1024 * 2;
   N_samp = 8;
   N_ch   = N_samp;
   N_col  = 32;
   r = Array:make(N_sim, 0.0);
   using Matrix;
   H = create(N_ch, N_col, 0.0);
   F = create(N_ch, N_col, 0.0);
   for i = 0 to N_col - 1 {
     for j = 0 to N_ch - 1 {
       // Using gint rather than intToFloat causes a problem:
       set(H, j,i, intToFloat$ i*N_col + j*N_ch + j + i + j + 1);
       set(F, j,i, intToFloat$ i*j + j*j + j + i);
     }
   }
 FilterBank(N_samp, N_ch, N_col, H, F) 
 $ source();
}

main = Streamit:run $ FilterBankNew();
