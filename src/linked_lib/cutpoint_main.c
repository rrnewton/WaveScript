


#include <stdio.h>

void wsinit(int argc, char** argv) {}
void wserror(const char* msg) { wserror_fun(msg); }

extern void READ_CUTPOINT_0(uint8_t*);

void wsmain(int argc, char** argv) {
  int count;  

  while(1) {
    if (1 != fread((void*)&count, 4, 1, stdin)) 
      wserror("Error reading message length word.");
    uint8_t* buf = WSARRAYALLOC_CHAR(count); // With refcount=0 initially.
    if (count != fread(buf, 1, count, stdin))
      wserror("Error reading message from stdin.");
    READ_CUTPOINT_0(buf);
  }
}

