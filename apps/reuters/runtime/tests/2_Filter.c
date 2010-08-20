#include <stdio.h>
#include "wsq_runtime.h"

int main(int argc, char* argv[]) {
  WSQ_Init("2_Filter.out");

    WSQ_BeginTransaction(99);
       WSQ_BeginSubgraph(11);
        WSQ_AddOp(1, "ReutersSource", "", "100", "10 |foobar.schema");
        WSQ_AddOp(2, "Filter", "100", "200", "(VOLUME >= 1) AND (PRICE <= 50)");
        WSQ_AddOp(4, "Printer", "200", "", "");
       WSQ_EndSubgraph();
    WSQ_EndTransaction();
    sleep(2);

    printf("\n ******* Query successfully ran for 3 seconds, shutting down...\n");
    WSQ_Shutdown();
    
    printf("Shutdown apparently successful.\n");
    return 0;
}
