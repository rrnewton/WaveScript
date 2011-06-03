
#include <stdlib.h>
#include <stdio.h>
#include "wsq_runtime.h"

int main(int argc, char* argv[]) {
  // WSQ_Init("1_TAQ_reader.out");
  WSQ_Init("");
  WSQ_SetQueryName("generated_query_1");

  WSQ_Pause();
  printf("PAUSED WSQ engine, run compiled query manually...\n");

  WSQ_BeginTransaction(1001);
    WSQ_BeginSubgraph(101);
      // Drive it by a max-rate timer (negative frequency convention):
      WSQ_AddOp(1, "ASCIIFileSource", "", "100", "-1 || TAQ.1000000");
      WSQ_AddOp(2, "UDF", "100", "200", "output_timer.ws | output_timer | 1.0 ");
      WSQ_AddOp(3, "Printer", "200", "", " Should never see this.. ");

    WSQ_EndSubgraph();
  int pid = WSQ_EndTransaction();
  
  // printf("Spawned pid %d, waiting on child process.\n");  
  // WSQ_Shutdown();
  return 0;
}
