
/*
 [2008.10.17]

 This file is a prototype.  It's the code that feeds byte arrays into
 the cutpoint of the query_server partition.  It's incredibly
 inflexible, assuming only one cutpoint (READ_CUTPOINT_0).  In the
 future, this stub will have to be generated for each of the cutpoints
 in the program.  But that will also entail a more complex
 communication mechanism.  This method simply reads from stdin --
 something that won't or shouldn't be scale to multiple cutpoints.
 The intention is to use TCP sockets instead.

 */


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
    SET_ARR_RC(buf, 1);
    if (count != fread(buf, 1, count, stdin))
      wserror("Error reading message from stdin.");
    READ_CUTPOINT_0(buf);
    // We free it ourselves:
    FREEARR(buf);
  }
}

