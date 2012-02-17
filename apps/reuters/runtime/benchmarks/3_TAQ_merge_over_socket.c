
#include <stdlib.h>
#include <stdio.h>
#include "wsq_runtime.h"

// A small benchmark that simply pumps tuples through a socket.
// (CLIENT SIDE)

#include "port.h"


void query_A() 
{ 
   printf("query a.\n");
   char addr[128];
   sprintf(addr, "UNSET__THIS_ADDR_SHOULD_NOT_BE_USED_IN_THE_CURRENT_IMPLEMENTATION | %d", PORT);

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
}


void query_B() 
{
    char strargs[2048];
    char* machine = "localhost";
    //char* machine = "marble";

    // This is the schema for tuples produced directly from ASCIIFileSource:
    sprintf(strargs, 
	    "%s | %d | bigint TIMESTAMP, string SYMBOL, bigint EXCHTIMESTAMP, bigint RECEIVEDTIME, double BID, double BIDSIZE, double ASK, double ASKSIZE "
	    , machine, PORT);

    WSQ_BeginTransaction(1001);
      WSQ_BeginSubgraph(101);

	WSQ_AddOp(20, "ConnectRemoteIn", "", "100", strargs);
	// Sample the throughput every second (1.0 hz):
	WSQ_AddOp(2, "UDF", "100", "200", "output_timer.ws | output_timer | 1.0 ");
	// LIMITATION: Need to have a printer to make sure the output gets compiled properly.
	WSQ_AddOp(3, "Printer", "200", "", " Should never see this.. ");

      WSQ_EndSubgraph();
    WSQ_EndTransaction();
}

void query_C() 
{
}

int main(int argc, char* argv[]) 
{
   if (argc < 2) {
     printf("ERROR: Usage: 3_TAQ_merge_over_socket [1,2,3]");
     printf("Runs component 1, 2, or 3 of the three-part query.");
     abort();    
   }
   int mode = atoi(argv[1]);
   printf("Building query fragment, mode %d\n", mode);

   char name[256];
   WSQ_Init("");
   sprintf(name,"generated_query_3%d", mode);
   WSQ_SetQueryName(name);
   WSQ_Pause();
   printf("PAUSED WSQ engine, run compiled query manually...\n");

   switch (mode) {
   case 1: query_A();
     break;
   case 2: query_B();
     break;
   case 3: query_C();
     break;
   }

   // WSQ_Shutdown();
   return 0;
}
