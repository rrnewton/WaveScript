
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

  WSQ_Pause();
  printf("PAUSING WSQ engine, run compiled query manually...\n");

  WSQ_BeginTransaction(1001);
    WSQ_BeginSubgraph(101);

      // Random tuples, 100Hz
      //WSQ_AddOp(2, "RandomSource", "","2", "1000 |foobar.schema");
      // WSQ_AddOp(1, "ASCIIFileSource", "", "2", "100 |foobar.schema|longer.dat"); 
      //WSQ_AddOp(1, "ASCIIFileSource", "", "2", "10000 |foobar.schema|taq_500mb.log"); 
      WSQ_AddOp(1, "ASCIIFileSource", "", "2", "-1 |foobar.schema|TAQ.1000000"); 
      //      WSQ_AddOp(1, "ASCIIFileSource", "", "2", "10000 |foobar.schema|../taq_500mb.log"); 
      // WSQ_AddOp(1, "ASCIIFileSource", "", "2", "10000 |foobar.schema|taq_500lines.dat"); 

      //      WSQ_AddOp(4, "Printer", "2", "", "From file source:");Thank

      WSQ_AddOp(3, "ConnectRemoteOut", "2", "", addr); 

    WSQ_EndSubgraph();
  int pid = WSQ_EndTransaction();

  if(pid ==0) { //pause version

    // run the executable directly
    printf("pid is 000000000000\n");
    char cmd[128] = "WSQ_OPTLVL=3 WSQ_MAXSPEED=1 ./proc1query.exe";
    system(cmd);
  }

  // sleep(1000);
  //sleep(1);

  //printf("Done Sleeping.  Shutting down WSQ Engine...\n");
  WSQ_Shutdown();
  return 0;
}
