
#define ARRLEN(ptr)        (ptr ? ((uint16_t*)ptr)[-1] : 0)
#define SETARRLEN(ptr,len) ((uint16_t*)ptr)[-1]=len 

// Get a pointer to the *start* of the thing (the pointer to free)
#define ARRPTR(ptr)        (((uint16_t*)ptr)-1)

#define wserror(str) call Leds.led0On()

// This seems insane to me, but the memcpy implementation on Telos
// doesn't work for unaligned addresses!  Here's a hack:
void my_memcpy(void* dst, const void* src, size_t num) {
  int s = (int)src;
  int d = (int)dst;
  //ASSERT num > 0

  // Alligned version:  
  if (!(d & 1) && !(s & 1)) 
    memcpy(dst,src,num);
  // Both unaligned:
  else if ((d & 1) && (s & 1)) {
    ((char*)dst)[0] = ((char*)src)[0];
    memcpy(dst+1,src+1,num-1);
    // Do we need to handle the tail specially also?
  } else {
    int i;  
    for (i=0; i<num; i++) ((char*)dst)[i] = ((char*)src)[i];
  }
}


// From TI http://focus.ti.com/lit/an/spra654/spra654.pdf
void radix4(int n, short x[], short w[]) 
{ 
   int   n1, n2, ie, ia1, ia2, ia3, i0, i1, i2, i3, j, k; 
   short t, r1, r2, s1, s2, co1, co2, co3, si1, si2, si3; 
   n2 = n; 
   ie = 1; 
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
          for (i0 = j; i0 < n; i0 += n1) {  // loop for butterfly 
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
}


/*
#define Q12_SCALE 8 
extern int r4_fft(short, short*, short*); 
short x[32]={   0,                0,      // input samples 
                4617/Q12_SCALE,   0,      // Scale the data from Q15 to Q12 
                9118/Q12_SCALE,   0, 
                13389/Q12_SCALE,  0, 
                17324/Q12_SCALE,  0, 
                20825/Q12_SCALE,  0, 
                23804/Q12_SCALE,  0, 
                26187/Q12_SCALE,  0, 
                27914/Q12_SCALE,  0, 
                28941/Q12_SCALE,  0, 
                29242/Q12_SCALE,  0, 
                28811/Q12_SCALE,  0, 
                27658/Q12_SCALE,  0, 
                25811/Q12_SCALE,  0, 
                23318/Q12_SCALE,  0, 
                20241/Q12_SCALE,  0   }; 
short w[32]={   0,      32767,  // Twiddle Factors 
                12540,  30274,  // 32768*sin(2PI*n/N), 32768*cos(2PI*n/N) 
                23170,  23170, 
                30274,  12540, 
                32767,  0, 
                30274,  -12540, 
                23170,  -23170, 
                12540,  -30274, 
                0,      -32767, 
                -12540, -30274, 
                -23170, -23170, 
                -30274, -12540, 
                -32767, 0, 
                -30274, 12540, 
                -23170, 23170, 
                -12540, 30274   }; 
short index[16]={   0,  4,  8,  12,   // index for 16-points digit reverse 
                    1,  5,  9,  13, 
                    2,  6,  10, 14, 
                    3,  7,  11, 15  }; 

short y[32];   // outputs 
main()
{ 
    int n=16; 
    int i; 
    int scale; 
    scale = r4_fft(n,x,w); 
    for(i=0; i<n; i++) { 
        y[2*i] = x[index[i]*2]; 
        y[2*i+1] = x[index[i]*2+1]; 
    } 
} 
*/




/* // [2007.12.06] This is the header that goes with my new C backend. -Ryan */

/* // Headers that we need for the generated code: */
/* #include<stdio.h> */
/* #include<stdlib.h> */
/* #include<string.h> */
/* #include<complex.h> */
/* #include<math.h> */
/* #include <time.h> */
/* #include <sys/time.h> */
/* #include <sys/resource.h> */


/* //int* arrayMake(size_t size, int len, ) { } */

/* #define TRUE  1 */
/* #define FALSE 0 */

/* // Handle Cons Cell memory layout: */
/* #define CONSCELL(ty)   ((int*)malloc(2 * sizeof(void*) + sizeof(ty)) + 2); */
/* #define CAR(ptr)       (*ptr) */
/* #define CDR(ptr)       (((void**)ptr)[-2]) */
/* #define SETCDR(ptr,tl) (((void**)ptr)[-2])=tl */
/* #define SETCAR(ptr,hd) ptr[0]=hd */

/* // Handle Array memory layout: */
/* #define ARRLEN(ptr)        (ptr ? ((int*)ptr)[-2] : 0) */
/* //#define ARRLEN(ptr)        ((int*)ptr)[-2] */
/* // This should not be used on a null pointer: */

/* // Handle RCs on Cons Cells and Arrays: */
/* #define CLEAR_RC(ptr)                ((int*)ptr)[-1] = 0 */
/* #define INCR_RC(ptr)        if (ptr) ((int*)ptr)[-1]++ */
/* #define DECR_RC_PRED(ptr) (ptr && --(((int*)ptr)[-1]) == 0) */

/* int outputcount = 0; */
/* int tuplimit = 10; */

/* void BASE(char x) {  */
/*   outputcount++; */
/*   if (outputcount >= tuplimit) exit(0); */
/* } */

/* void wserror(char* msg) { */
/*   //error(msg); */
/*   printf("Failed with error: %s\n", msg); */
/*   exit(-1); */
/* } */


/* /\* */
/* // TODO: */
/* int Listlength(void* list) { */
/*   int acc = 0; */
/*   printf("List len... %p\n", list); */
/*   while (list != 0) { */
/*     list = CDR(list); */
/*     acc++; */
/*   } */
/*   return acc;  */
/* } */
/* *\/ */

/* /\* */
/* // TODO: */
/* void* Listappend(void* ls1, void* ls2) { */
/*   printf("List append... %p and %p\n", ls1, ls2); */
/*   return ls1; */
/* } */

/* // TODO: */
/* void* Listreverse(void* ls) { */
/*   printf("List reverse... %p\n", ls); */
/*   return ls; */
/* } */
/* *\/ */


/* // This won't work: */
/* /\* */
/* int Listref(void* list, int n) { */
/*   return 0;  */
/* } */
/* *\/ */


/* inline static float cNorm(complex c) { */
/*    float re =  __real__ (c); */
/*    float im =  __imag__ (c); */
/*    return sqrt ((re*re) + (im*im)); */
/* } */

