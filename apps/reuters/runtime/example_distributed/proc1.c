
#include <stdlib.h>
#include <stdio.h>
#include "wsq_runtime.h"

// An example demonstrating how to link and use the WSQ runtime system.

#include "port.h"

int main(int argc, char* argv[]) {
    char addr[128];
    //sprintf(addr, "fort2.csail.mit.edu | %d", PORT);
    sprintf(addr, "localhost | %d", PORT);

  WSQ_Init("");
  WSQ_SetQueryName("proc1query");

  WSQ_BeginTransaction(1001);
    WSQ_BeginSubgraph(101);

      // Random tuples, 100Hz
      //WSQ_AddOp(2, "RandomSource", "","2", "100 |foobar.schema");
      WSQ_AddOp(1, "ASCIIFileSource", "", "2", "100 |foobar.schema|longer.dat"); 
      WSQ_AddOp(3, "ConnectRemoteOut", "2", "", addr); 

    WSQ_EndSubgraph();
  WSQ_EndTransaction();

  sleep(1000);

  WSQ_Shutdown();
  return 0;
}
