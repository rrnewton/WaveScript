
#include <stdio.h>
#include "wsq_runtime.h"

// This test demonstrates the new UDF capability of the WSQ query system.

int main(int argc, char* argv[]) 
{
  // Set output file:
  WSQ_Init("11_Avg_UDF.out");
  WSQ_SetQueryName("generated_query_11");

    WSQ_BeginTransaction(99);
       WSQ_BeginSubgraph(11);
        WSQ_AddOp(1, "RandomSource", "", "100", "10000 |foobar.schema");

        WSQ_AddOp(2, "UDF", "100", "200", "11_Avg_UDF.ws | avg | 10");

        WSQ_AddOp(3, "Printer", "200", "", " [11_Avg_UDF] got output: ");
       WSQ_EndSubgraph();
    WSQ_EndTransaction();

    sleep(2);
    printf("\n [11_Avg_UDF] Query successfully ran for 3 seconds, shutting down...\n");
    WSQ_Shutdown();
    
    printf(" [11_Avg_UDF] Shutdown apparently successful.\n");
    return 0;
}
