
// Benchmark:
// How fast can we read tuples from a file 

#include <stdlib.h>
#include <stdio.h>
#include "wsq_runtime.h"

int main(int argc, char* argv[]) {
  char* filename = "TAQ.1000000";
  char opargs[1000];
  if (argc <= 1) { } else 
  if (argc == 2) {
    filename = argv[1];
    printf("Reading input tuples from file %s\n", filename);
  } else {
    printf("ERROR: wrong number of arguments!\n"); abort();
  }

  WSQ_Init("");
  WSQ_SetQueryName("generated_query_1");

  WSQ_Pause();
  printf("PAUSED WSQ engine, run compiled query manually...\n");

  WSQ_BeginTransaction(1001);
    WSQ_BeginSubgraph(101);
      // Drive it by a max-rate timer (negative frequency convention):
      sprintf(opargs, "-1 || %s", filename);
      WSQ_AddOp(1, "ASCIIFileSource", "", "100", opargs);
      WSQ_AddOp(2, "UDF", "100", "200", "output_timer.ws | output_timer | 1.0 ");

    WSQ_EndSubgraph();
  int pid = WSQ_EndTransaction();
  
  // printf("Spawned pid %d, waiting on child process.\n");  
  // WSQ_Shutdown();
  return 0;
}
