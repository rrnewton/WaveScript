
#include <stdio.h>

// This is the entry point into WS:
void wsentry(int);

void wserror(const char* p) {
  printf("GOT WSERROR: %s\n", p);
}

// In a multithreaded environment we would need a lock protecting
// "val" because the callback "wssink" could be called asynchronously
// from another thread.
int val = 0;

// A callback for WS to return values back to C.  In this simple
// example we store them in a global variable.
void wssink(int n) {
  val = n;
}

void wsmain(int argc, char* argv[]) {
  int i, counter = 5;

  printf("Starting up in C main function, val = %d\n", val);
  wsentry(val);
  printf("After one call to WS has completed, val = %d\n", val);
  wsentry(val);
  printf("After two calls to WS have completed, val = %d\n", val);
}

void wsinit(int argc, char* argv[]) {
  val = 99;
}
