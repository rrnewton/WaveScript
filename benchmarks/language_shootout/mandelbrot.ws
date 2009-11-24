
/* The Computer Language Shootout
   http://shootout.alioth.debian.org/

   contributed by Ryan Newton
   based on C version by Greg Buchholz

   for the debian (AMD) machine...
   compile flags:  -O3 -ffast-math -march=athlon-xp -funroll-loops

   for the gp4 (Intel) machine...
   compile flags:  -O3 -ffast-math -march=pentium4 -funroll-loops

On honor I get 1.0s for the C version and 1.5s for the WS version.
They should be identical;wonder what the problem is...  Well, first,
it's down to 1.3 with gcc.  That's a first.  Also, the C version gets
slower under gcc, also at 1.3.  The obfuscation is hurting icc in this
instance.  I could probably help it out some.

I haven't tried the SSE version yet.

*/

include "stdlib.ws"

//N = 200
N = 3000

fun run ()
{
    w = N; h = w;
    _w = Double! w; 
    _h = Double! h;
    bit_num = 0; 
    byte_acc = intToChar(0);
    iter = 50;
    limit2 = 2.0l * 2.0l;

    print("P4\n"++w++" "++h++"\n");

    for y = 0 to h-1 
    {
        for x = 0 to w-1
        {
	    Zr = 0.0l;  Zi = 0.0l;
	    Tr = 0.0l;  Ti = 0.0l;
            Cr = (2.0l * Double!x / _w - 1.5l); 
	    Ci = (2.0l * Double!y / _h - 1.0l);
	    
	    i=0; 
	    while i<iter && (Tr+Ti <= limit2)
            {
                Zi := 2.0l * Zr * Zi + Ci;
                Zr := Tr - Ti + Cr;
                Tr := Zr * Zr;
                Ti := Zi * Zi;
 	        i += 1;
            }

  	    byte_acc := lshiftC(byte_acc, 1);
	    
            if Tr+Ti <= limit2 then byte_acc := lorC(byte_acc, 1`intToChar);

            bit_num += 1;

            if bit_num == 8 then
            {
                print(byte_acc);
   	        byte_acc := 0`intToChar;
	        bit_num  := 0;
            }
            else if x == w-1 then
            {
	        byte_acc := lshiftC(byte_acc, 8 - moduloI(w,8));
                print(byte_acc);
	        byte_acc := 0`intToChar;
	        bit_num := 0;
            }
        }
    }
}

main = ONCE(run)
