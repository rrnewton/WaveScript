/* sample.c
 * Copyright (c) 1997 Cadence Research Systems
 */

/* test with:
 *   make m=<machine type> Custom=sample.o
 *   ./scheme
 */

static void sample_custom_init(void);

/* define CUSTOM_INIT so that custom.c won't define its own */
#define CUSTOM_INIT sample_custom_init

/* we want to be everything the base custom.c is, and more */
#include "custom.c"

/****
   This is a sample C customization file.  It defines the C function
   id() and make it available to Scheme via the call to foreign_symbol()
   in custom_init().  The function custom_init() is invoked at system
   boot time expressly to allow foreign entry points to be defined.
****/

static int id(x) int x; { return x; }

/* the following are extracted from the Chez Scheme System Manual */
/* a particularly silly way to multiply two floating-point numbers */
static double mul(double x, double y) {
    ptr times = Stop_level_value(Sstring_to_symbol("*"));
 
    return Sflonum_value(Scall2(times, Sflonum(x), Sflonum(y)));
}
 
/* an equally silly way to call printf with five arguments */
 
/* it is best to define interfaces such as the one below to handle
 * calls into Scheme to prevent accidental attempts to nest frame
 * creation and to help ensure that initiated calls are completed
 * as discussed above.  Specialized versions tailored to particular
 * C argument types may be defined as well, with embedded conversions
 * to Scheme objects. */
static ptr Scall5(ptr p, ptr x1, ptr x2, ptr x3, ptr x4, ptr x5) {
    Sinitframe(5);
    Sput_arg(1, x1);
    Sput_arg(2, x2);
    Sput_arg(3, x3);
    Sput_arg(4, x4);
    Sput_arg(5, x5);
    Scall(p, 5);
}

static void dumpem(char *s, int a, double b, ptr c, char *d) {
    printf(s, a, b, c, d);
}

static void foo(int x, double y, ptr z, char *s) {
    ptr ois, sip, read, expr, eval, c_dumpem;
    char *sexpr = "(foreign-procedure \"dumpem\" (string integer-32\
 double-float scheme-object string) void)";

  /* this series of statements is carefully crafted to avoid referencing
     variables holding Scheme objects after calls into Scheme */
    ois = Stop_level_value(Sstring_to_symbol("open-input-string"));
    sip = Scall1(ois, Sstring(sexpr));
    read = Stop_level_value(Sstring_to_symbol("read"));
    expr = Scall1(read, sip);
    eval = Stop_level_value(Sstring_to_symbol("eval"));
    Sforeign_symbol("dumpem", (void *)dumpem);
    c_dumpem = Scall1(eval, expr);
    Scall5(c_dumpem,
           Sstring("x = %d, y = %g, z = %x, s = %s\n"),
           Sinteger(x),
           Sflonum(y),
           z,
           Sstring(s));
}

static void sample_custom_init(void) {
   Sforeign_symbol("id", (void *)id);
   Sforeign_symbol("foo", (void *)foo);
   Sforeign_symbol("mul", (void *)mul);
}


/* sample Scheme session using id, foo, and mul:

> (define int->bool (foreign-procedure "id" (integer-32) boolean))
> (int->bool 0)
#f
> (int->bool 1)
#t
> (define p
    (foreign-procedure "foo"
      (integer-32 double-float scheme-object string)
      void))
> (p 3 2.3 '() "hi mom")
x = 3, y = 2.3, z = 26, s = hi mom
> (define mul
    (foreign-procedure "mul" (double-float double-float)
      double-float))
> (mul 3.4 2.0)
6.8
> (define fact
    (lambda (n)
      (if (= n 0.0)
          1.0
          (mul n (fact (- n 1.0))))))
> (fact 20.0)
2.43290200817664e18
*/
