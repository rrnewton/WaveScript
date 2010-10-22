
#include <stdio.h>
#include "wsq_runtime.h"

// The idea here is to run a high-rate stream through a very selective match-recognize filter.
// The frequence is calibrated to create actual output at a reasonable rate.

int main(int argc, char* argv[]) {
  WSQ_Init("5_MatchRecognize_rare_hits.out");
  // WSQ_Init("");
    WSQ_BeginTransaction(99);
       WSQ_BeginSubgraph(11);
        WSQ_AddOp(1, "RandomSource", "", "100", "100000 |foobar.schema");
        WSQ_AddOp(2, "MatchRecognize", "100", "200", "ALL | A B A | A AS (PRICE > 55), B AS (SYM = \"IBM\")");
        WSQ_AddOp(3, "Printer", "200", "", "");
       WSQ_EndSubgraph();
    WSQ_EndTransaction();
    sleep(2);

    printf("\n ******* Query successfully ran for 3 seconds, shutting down...\n");
    WSQ_Shutdown();
    
    printf("Shutdown apparently successful.\n");
    return 0;
}
