
#include <stdio.h>
#include "wsq_runtime.h"


int main(int argc, char* argv[]) {
  WSQ_Init("4_MatchRecognize_always_succeed.out");

    WSQ_BeginTransaction(99);
       WSQ_BeginSubgraph(11);
        WSQ_AddOp(1, "ReutersSource", "", "100", "10 |foobar.schema");
        //WSQ_AddOp(3, "MatchRecognize", "100", "200", "ALL | A B | A AS (PRICE > 50), B AS (SYM = \"IBM\")");
        WSQ_AddOp(2, "MatchRecognize", "100", "200", "ALL | A B | A AS (PRICE > 1), B AS (VOLUME > 0)");
        WSQ_AddOp(3, "Printer", "200", "", "");
       WSQ_EndSubgraph();
    WSQ_EndTransaction();
    sleep(2);

    printf("\n ******* Query successfully ran for 3 seconds, shutting down...\n");
    WSQ_Shutdown();
    
    printf("Shutdown apparently successful.\n");
    return 0;
}
