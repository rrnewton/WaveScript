


#include <stdio.h>

void wsinit(int argc, char** argv) {}
void wserror(const char* msg) { wserror_fun(msg); exit(1); }

extern void NODE_ENTRY(char);

void wsmain(int argc, char** argv) {
  // This does no timing at all.  It simply feeds events as fast as possible.
  while(1) {
    NODE_ENTRY(0);
  }
}
