
#include <stdlib.h>
#include <stdio.h>
#include "wsq_runtime.h"

// A small benchmark that simply pumps tuples through a socket.
// (CLIENT SIDE)

#include "port.h"

int main(int argc, char* argv[]) {
    char addr[128];
    sprintf(addr, "UNSET__THIS_ADDR_SHOULD_NOT_BE_USED_IN_THE_CURRENT_IMPLEMENTATION | %d", PORT);
    // sprintf(addr, "localhost | %d", PORT);

  WSQ_Init("");
  WSQ_SetQueryName("generated_query_2A");

  WSQ_Pause();
  printf("PAUSED WSQ engine, run compiled query manually...\n");

  WSQ_BeginTransaction(1001);
    WSQ_BeginSubgraph(101);
    // Load tuples MAX speed:
    WSQ_AddOp(1, "ASCIIFileSource", "", "100", "-1 || TAQ.1000000");
    // A whole gigabyte:
    // WSQ_AddOp(1, "ASCIIFileSource", "", "100", "-1 || TAQ.5206325");
      // Send them right out the door:
      WSQ_AddOp(2, "ConnectRemoteOut", "100", "", addr); 
    WSQ_EndSubgraph();
  WSQ_EndTransaction();

  // WSQ_Shutdown();
  return 0;
}
