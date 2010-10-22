#include <stdio.h>
#include "wsq_runtime.h"

// This test creates just a source of data and prints tuples to a file.

int main(int argc, char* argv[]) {
  WSQ_Init("1_ReuterSource.out");

    WSQ_BeginTransaction(99);
       WSQ_BeginSubgraph(11);
        WSQ_AddOp(1, "RandomSource", "", "100", "10 |foobar.schema");
        WSQ_AddOp(4, "Printer", "100", "", "");
       WSQ_EndSubgraph();
    WSQ_EndTransaction();
    sleep(2);

    printf("\n ******* Query successfully ran for 3 seconds, shutting down...\n");
    WSQ_Shutdown();
    
    printf("Shutdown apparently successful.\n");
    return 0;
}
