

include "stdlib.ws"



//type FftInt = Int32;
type FftInt = Int16;

// This version doesn't have proper overflow protection:
fftimpl = "// From TI http://focus.ti.com/lit/an/spra654/spra654.pdf
#define bigty int32_t
#define lilty int16_t

//#define bigty int64_t
//#define lilty int32_t

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
levels = 8;
arrsize = 2^levels;
nsize = arrsize/2;

twiddle = make(arrsize,0)
snip    = make(15,0);

//theCcode = inline_C(fftimpl,"");
//thefn = (foreign("myradix4", []) :: (Int, Array FftInt, Array FftInt) -> Int);


// Trying this version instead:
fixlib = GETENV("WAVESCRIPTD") ++ "/lib/fix_fft.c";
theCcode = inline_C("#include \""++ fixlib ++"\"","");
newfn = (foreign("fix_fft", []) :: (Array FftInt, Array FftInt, Int, Bool) -> Int);

// Third, ported fix_fft directly to WS.  This is in lib/
include "fix_fft.ws"

// Set up the problem in global state:
fun setup(floats, inp, imag){
  // This gives us a constant input:
  //for i = 0 to arrsize-1 { floats[i] := 0.5; };

  // This gives us a simple ascending sequence::
  //for i = 0 to arrsize-1 { floats[i] := (cast_num(i) :: Float) / arrsize.gint; };
  //for i = 0 to arrsize-1 { floats[i] := (cast_num(i) :: Float); };

  // This gives us a sin wave:
  for i = 0 to arrsize-1 { floats[i] := sin$ (cast_num(i) :: Float) * 2 * 3.14159 / arrsize.gint; };
  //println("Float input: "++floats);

  for i = 0 to arrsize-1 { 
    inp[i] := (cast_num(floats[i] * 32767) :: FftInt); 
    // Clear imaginary:
    imag[i] := 0;
  };
  //print("  Int input: "++inp++"\n");
}

floats  = make(arrsize,0)
inp     = make(arrsize,11)
imag    = make(arrsize,0)

floats2  = make(arrsize,0)
inp2     = make(arrsize,11)
imag2    = make(arrsize,0)

table_len = (Array:length(Sinewave));

// This is just an example, it builds the sine table at metoprogram eval:
newsinetable :: Array Int16 =
Array:build(768, fun(i) {
  fl :: Float = (cast_num(i)::Float) * 1.0;
  fl2 :: Float = sin(const_PIO2 * fl / 256);
  //result :: Int16 = cast_num(fl2 * 65536.0); // Let it overflow
  result :: Int16 = cast_num(fl2 * 32767.0); 
  //result :: Int16 = floatToInt16(fl * 32767.0);
  //println("Converting float: orig: "++fl++"  frac: "++fl/256++"  \tsin: "++fl2++" int: "++result);
  result
})

namespace Node {
 
cver = iterate _ in timer(0.2) 
               .merge(theCcode)
{
  //print("Size of Sinewave table: "++table_len++"\n");  
  //print("New Sinewave table: "++ (newsinetable::Array Int16) ++"\n");  
  //print("C Version, fix_fft:\n");
  setup(floats,inp,imag);   

  // Try 1
  //iters = thefn(nsize, inp, twiddle);
  //print("iters "++iters++"\n");

  // Try 2
  newfn(inp, imag, levels, false);
  
  print(" C (real): "++inp++"\n");
  ////print("  C (imag): "++imag++"\n");
    
  //Array:blit(snip, 0, out, 0, Array:length(snip));
  //print("length: "++snip.length++"\n");
  //blit(snip, 0, out, 0, snip.length);
  emit ();
}

 wsver = iterate _ in cver {
   //println("WS Version, fix_fft:\n");
   setup(floats2, inp2,imag2);
 
   // Try 3
   fix_fft(inp2, imag2, levels, false);
   //newfn(inp2, imag2, levels, false);

   print(" WS (real): "++inp2++"\n");
  ////print(" WS (imag): "++imag2++"\n");

  emit ();
 }

} // End Node  



/*
corroborate = iterate _ in timer$1{
  //reference = fftR2C(map(int16ToFloat, inp));
  reference = fftR2C(floats);
  using Array;
  println("Reference: "++ reference);
  println("Log Real Reference: "++ map(logF, map(realpart, reference)));

}*/

main = Node:wsver;
