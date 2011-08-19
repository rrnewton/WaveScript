
#include <stdlib.h>
#include <stdio.h>
#include "wsq_runtime.h"


int main(int argc, char* argv[]) {
  WSQ_Init("8_TAQ_reader.out");
  WSQ_SetQueryName("generated_query_8");

  printf("PAUSING WSQ engine, run compiled query manually...\n");

  WSQ_BeginTransaction(1001);
    WSQ_BeginSubgraph(101);

      // Tuples, 100Hz
      WSQ_AddOp(1, "ASCIIFileSource", "", "2", "-1 |foobar.schema|./example_distributed/taq_500lines.dat"); 

      WSQ_AddOp(2, "Printer", "2", "", "From file source:");

    WSQ_EndSubgraph();
  WSQ_EndTransaction();

  sleep(2);
  printf("Done Sleeping.  Shutting down WSQ Engine...\n");
  WSQ_Shutdown();
  return 0;
}
