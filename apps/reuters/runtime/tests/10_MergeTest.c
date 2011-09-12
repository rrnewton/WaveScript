

#include <stdio.h>
#include "wsq_runtime.h"

// Merge monotonic matches fields that montonically increase in both of two input streams.

int main(int argc, char* argv[]) {
  WSQ_Init("7_MergeMonotonic.out");
  WSQ_SetQueryName("generated_query_7");

    WSQ_BeginTransaction(99);
       WSQ_BeginSubgraph(11);
       WSQ_AddOp(1, "ASCIIFileSource", "", "100", "50 |foobar.schema|TAQ.10000");
       WSQ_AddOp(2, "ASCIIFileSource", "", "200", "50 |foobar.schema|TAQ.10000");

       // Now merge them together:
       // ============================================================
       WSQ_AddOp(3, "MergeMonotonic", "100 200", "300", "TIMESTAMP");
       // ============================================================

       WSQ_AddOp(6, "Printer", "300", "", "Merged");

       WSQ_EndSubgraph();
       WSQ_EndTransaction();
       sleep(2);
       
       printf("\n ******* Query successfully ran for 3 seconds, shutting down...\n");
       WSQ_Shutdown();
       
       printf("Shutdown apparently successful.\n");
       return 0;
}
