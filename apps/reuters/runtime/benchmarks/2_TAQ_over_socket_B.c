#include <stdlib.h>
#include <stdio.h>
#include "wsq_runtime.h"

// An example demonstrating how to link and use the WSQ runtime system.

#include "port.h"

int main(int argc, char* argv[]) { 
    char strargs[128];
    char* machine = "localhost";
    // char* machine = "marble";
    // This is the schema for tuples produced directly from ASCIIFileSource:
    sprintf(strargs, 
	    "%s | %d | bigint TIMESTAMP, string SYMBOL, bigint EXCHTIMESTAMP, bigint RECEIVEDTIME, double BID, double BIDSIZE, double ASK, double ASKSIZE "
	    , machine, PORT);

  WSQ_Init("");
  WSQ_SetQueryName("generated_query_2B");

  WSQ_Pause();
  printf("PAUSING WSQ engine, run compiled query manually...\n");
  WSQ_BeginTransaction(1001);
    WSQ_BeginSubgraph(101);

      WSQ_AddOp(20, "ConnectRemoteIn", "", "100", strargs);
      // Sample the throughput every second (1.0 hz):
      WSQ_AddOp(2, "UDF", "100", "200", "output_timer.ws | output_timer | 1.0 ");
      // LIMITATION: Need to have a printer to make sure the output gets compiled properly.
      WSQ_AddOp(3, "Printer", "200", "", " Should never see this.. ");

    WSQ_EndSubgraph();
  WSQ_EndTransaction();
  // WSQ_Shutdown();
  return 0;
}
