


// [2008.09.01] Would this work for 64 bits?

inline int atomic_exchange_and_add( int * pw, int dv )
{
    // int r = *pw;
    // *pw += dv;
    // return r;

    int r;

    __asm__ __volatile__
    (
        "lock\n\t"
        "xadd %1, %0":
        "=m"( *pw ), "=r"( r ): // outputs (%0, %1)
        "m"( *pw ), "1"( dv ): // inputs (%2, %3 == %1)
        "memory", "cc" // clobbers
    );

    return r;
}

inline void atomic_increment( int * pw )
{
    //printf("Atomic Incr %p\n", pw);
    //atomic_exchange_and_add( pw, 1 );
    __asm__
    (
        "lock\n\t"
        "incl %0":
        "=m"( *pw ): // output (%0)
        "m"( *pw ): // input (%1)
        "cc" // clobbers
    );
}

/*

// From midishare, gpl:


#ifdef __SMP__
#	define LOCK "lock ; "
#else
#	define LOCK ""
#endif

//----------------------------------------------------------------
// CAS functions
//----------------------------------------------------------------
static inline char CAS (volatile void * addr, volatile void * value, void * newvalue) 
{
	register char ret;
	__asm__ __volatile__ (
		"# CAS \n\t"
		LOCK "cmpxchg %2, (%1) \n\t"
		"sete %0               \n\t"
		:"=a" (ret)
		:"c" (addr), "d" (newvalue), "a" (value)
	);
	return ret;
}

static inline char CAS2 (volatile void * addr, volatile void * v1, volatile long v2, void * n1, long n2) 
{
	register char ret;
	__asm__ __volatile__ (
		"# CAS2 \n\t"
		LOCK "cmpxchg8b (%1) \n\t"
		"sete %0               \n\t"
		:"=a" (ret)
		:"D" (addr), "d" (v2), "a" (v1), "b" (n1), "c" (n2)
	);
	return ret;
}

*/


/*
MORE: scraped from the web:



void atomic_add(int * operand, int incr)
{
    asm volatile (
        "lock xaddl %1, %0\n" // add incr to operand
        : // no output
        : "m" (*operand), "r" (incr)
    );
}

//It uses xaddl so that I don\u2019t have to worry about output, and can do the memory operation. The following also works, but may not be as fast (or it may be equivalent):

void atomic_add(int * operand, int incr)
{
    asm volatile (
        "lock xaddl %1, (%0)\n" // add incr to operand
        : // no output
        : "r" (operand), "r" (incr)
    );
}

//Here\u2019s a version that works on 386\u2019s. I don\u2019t know which is \u201cfaster\u201d on all architectures:

void atomic_add(int * operand, int incr)
{
    asm volatile (
        "lock addl %1, %0\n" // add incr to operand
        : "=m" (*operand)
        : "r" (incr), "m" (*operand)
    );
}


#define atomic_add(i,v) \
({ \
    int __ia64_aar_i = (i); \
    (__builtin_constant_p(i) \
     && (   (__ia64_aar_i   1) || (__ia64_aar_i    4) \
         || (__ia64_aar_i   8) || (__ia64_aar_i   16) \
         || (__ia64_aar_i  -1) || (__ia64_aar_i   -4) \
         || (__ia64_aar_i  -8) || (__ia64_aar_i  -16) \
            ? ia64_fetch_and_add(__ia64_aar_i, &(v)->counter) \
            : ia64_atomic_add(__ia64_aar_i, v); \
})

Yeah, pretty sweet, no? Separate functions for doing simple math versus doing other increments. Here\u2019s a simple increment:

void atomic_oneup(int * operand)
{
    uint64_t res; // this MUST be 64 bits
    asm volatile (
        "fetchadd4.rel %0=%1,%2"
        : "=r" (res)
        : "m" (*operand), "i" (1)
        : "memory"
    );
}

Note that it HAS to return something, because it\u2019s a fetch instruction. But it\u2019s also conveniently atomic. And that increment doesn\u2019t have to be 1, obviously, but could be any of the ones listed in that ugly #define. But what about adding arbitrary numbers? For this we must use cmpxchg. Here\u2019s how Linux uses it (tidied up a LOT):

static __inline int ia64_atomic_add (int i, atomic_t *v)
{
    __s32 old, new;
    do {
        old = atomic_read(v);
        new = old + i;
    } while (ia64_cmpxchg(acq, v, old, new, sizeof(atomic_t)) != old);
    return new;
}

How can you not love something like that? Boiling it down, that call to ia64_cmpxchg turns into: ia64_cmpxchg4_acq((__u16*)v, new, o); (that @o@ variable is a place-holder). THAT, in turn, is this:

#define ia64_cmpxchg4_acq(ptr, new, old) \
({
    __u64 ia64_intri_res;
    asm volatile ("mov ar.ccv=%0;;" :: "rO" (old));
    asm volatile ("cmpxchg4.acq %0=[%1],%2,ar.ccv":
    "=r" (ia64_intri_res) : "r"(ptr), "r"(new) : "memory");
    ia64_intri_res;
})

Just so we don\u2019t go insane here, here\u2019s a condensed loop:

void atomic_incr(int * operand, int incr)
{
    int64_t res, old, new;
    do {
        old = *operand; // atomic if operand is 32-bit aligned
        new = old + incr;
        asm volatile ("mov ar.ccv=%0;;" :: "rO" (old));
        // these are separate because the compiler inserts stuff in between
        asm volatile ("cmpxchg4.acq %0=[%1],%2,ar.ccv":
                    "=r" (res) : "r"(operand), "r"(new) : "memory");
    } while (res != old); // if res==old, our computation is out of date
}

Ugly much? Just a bit.



*/
