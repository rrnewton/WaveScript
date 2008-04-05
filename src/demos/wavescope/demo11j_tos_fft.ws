

include "stdlib.ws"



type FftInt = Int32;
//type FftInt = Int16;

// This version doesn't have proper overflow protection:
fftimpl = "// From TI http://focus.ti.com/lit/an/spra654/spra654.pdf
//#define bigty int32_t
//#define lilty int16_t

#define bigty int64_t
#define lilty int32_t

// x : input (and output)
// w : twiddle factors
int myradix4(int n, lilty x[], lilty w[])
{ 
   // These were int/short originally:
   bigty n1, n2, ie, ia1, ia2, ia3, i0, i1, i2, i3, j, k; 
   lilty t, r1, r2, s1, s2, co1, co2, co3, si1, si2, si3; 
   int32_t iters = 0;

   n2 = n; 
   ie = 1; 

   //printf(\"Well...\\n\");
   for (k = n; k > 1; k >>= 2) {           // number of stages 
       n1 = n2;
       n2 >>= 2; 
       ia1 = 0; 

       for (j = 0; j < n2; j++) {          // number of butterflies 
           ia2 = ia1 + ia1;                // per stage 
           ia3 = ia2 + ia1; 
           co1 = w[ia1 * 2 + 1]; 
           si1 = w[ia1 * 2]; 
           co2 = w[ia2 * 2 + 1]; 
           si2 = w[ia2 * 2]; 
           co3 = w[ia3 * 2 + 1]; 
           si3 = w[ia3 * 2]; 
           ia1 = ia1 + ie; 
          //printf(\".\");                 
          for (i0 = j; i0 < n; i0 += n1) {  // loop for butterfly 

	       //printf(\"Iter  %d  %d  %d\\n\", k,j,i0);

	       iters++;

               i1 = i0 + n2;                // calculations 
               i2 = i1 + n2; 
               i3 = i2 + n2; 
               r1 = x[2 * i0] + x[2 * i2]; 
               r2 = x[2 * i0] - x[2 * i2]; 
               t = x[2 * i1] + x[2 * i3]; 
               x[2 * i0] = r1 + t; 

               r1 = r1 - t; 
               s1 = x[2 * i0 + 1] + x[2 * i2 + 1]; 
               s2 = x[2 * i0 + 1] - x[2 * i2 + 1]; 
               t = x[2 * i1 + 1] + x[2 * i3 + 1]; 

               x[2 * i0 + 1] = s1 + t; 
               s1 = s1 - t; 
               x[2 * i2] = (r1 * co2 + s1 * si2)    >> 15; 
               x[2 * i2 + 1] = (s1 * co2 - r1 * si2)>> 15; 
               t = x[2 * i1 + 1] - x[2 * i3 + 1]; 

               r1 = r2 + t; 
               r2 = r2 - t; 
               t = x[2 * i1] - x[2 * i3]; 
               s1 = s2 - t; 
               s2 = s2 + t; 

               x[2 * i1] = (r1 * co1 + s1 * si1)    >> 15; 
               x[2 * i1 + 1] = (s1 * co1 - r1 * si1)>> 15; 
               x[2 * i3] = (r2 * co3 + s2 * si3)    >> 15; 
               x[2 * i3 + 1] = (s2 * co3 - r2 * si3)>> 15; 
          }

       }
      ie <<= 2; 
   }
   return iters;
}"

// stupid emacs mode: " 

using Array;

// Hmm... what is the meaning of the first argument?
arrsize = 16;
nsize = arrsize/2;

floats  = make(arrsize,0)
inp     = make(arrsize,11)
imag    = make(arrsize,0)
twiddle = make(arrsize,0)
snip    = make(15,0);

//theCcode = inline_C(fftimpl,"");
//thefn = (foreign("myradix4", []) :: (Int, Array FftInt, Array FftInt) -> Int);


// Trying this version instead:
theCcode = inline_C("#include \"fix_fft.c\"","");
newfn = (foreign("fix_fft", ["fix_fft.c"]) :: (Array FftInt, Array FftInt, Int16, Int16) -> Int);


Node:s1 = iterate _ in timer(0.2) 
                       .merge(theCcode)
{
  // This gives us a constant input:
  //for i = 0 to arrsize-1 { floats[i] := 0.0; };
  // This gives us a simple ascending sequence::
  //for i = 0 to arrsize-1 { floats[i] := (cast_num(i) :: Float) / arrsize.gint; };
  // This gives us a sin wave:
  for i = 0 to arrsize-1 { floats[i] := sin$ (cast_num(i) :: Float) * 2 * 3.14159 / arrsize.gint; };

  for i = 0 to arrsize-1 { 
    inp[i] := (cast_num(floats[i] * 32768) :: FftInt); 
    // Clear imaginary:
    imag[i] := 0;
  };

  println("Int input: "++inp);
  //println("Float input: "++floats);
    
  //reference = fftR2C(map(int16ToFloat, inp));
  reference = fftR2C(floats);
  println("Reference: "++reference);

  //iters = thefn(nsize, inp, twiddle);
  //print("iters "++iters++"\n");

  newfn(inp, twiddle, nsize, 0);
  
  println("Output (real): "++inp);

  println("Output (imag): "++imag);

    
  //Array:blit(snip, 0, out, 0, Array:length(snip));
  //print("length: "++snip.length++"\n");
  //blit(snip, 0, out, 0, snip.length);
  emit ();
}

main = Node:s1;
