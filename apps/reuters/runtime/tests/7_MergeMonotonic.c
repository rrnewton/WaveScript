

#include <stdio.h>
#include "wsq_runtime.h"

// Merge monotonic matches fields that montonically increase in both of two input streams.

int main(int argc, char* argv[]) {
  WSQ_Init("7_MergeMonotonic.out");
  WSQ_SetQueryName("generated_query_7");

    WSQ_BeginTransaction(99);
       WSQ_BeginSubgraph(11);
        WSQ_AddOp(1, "NonRandomSource", "", "100", "50 |foobar.schema");

        // Even and Odd Volume fields:
        WSQ_AddOp(2, "Filter", "100", "200", "((VOLUME % 2) == 0)");
        WSQ_AddOp(3, "Filter", "100", "300", "((VOLUME % 2) == 1)");

        // Increment by one:
        WSQ_AddOp(4, "Project", "300", "400", "SYM, TIME, PRICE, (VOLUME + 1) AS VOLUME");

        // Now merge them together:
        // ============================================================
        WSQ_AddOp(5, "MergeMonotonic", "200 400", "500", "VOLUME");
        // ============================================================

        WSQ_AddOp(6, "Printer", "500", "", "Merged");

       WSQ_EndSubgraph();
    WSQ_EndTransaction();
    sleep(2);

    printf("\n ******* Query successfully ran for 3 seconds, shutting down...\n");
    WSQ_Shutdown();
    
    printf("Shutdown apparently successful.\n");
    return 0;
}


