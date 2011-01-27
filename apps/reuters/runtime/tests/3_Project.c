
#include <stdio.h>
#include "wsq_runtime.h"


int main(int argc, char* argv[]) {
  WSQ_Init("3_Project.out");

    WSQ_BeginTransaction(99);
       WSQ_BeginSubgraph(11);
        WSQ_AddOp(1, "RandomSource", "", "100", "10 |foobar.schema");
        WSQ_AddOp(2, "Project", "100", "200", "SYM, TIME, (PRICE + 1000) AS PRICE");
        WSQ_AddOp(3, "Printer", "200", "", "");
       WSQ_EndSubgraph();
    WSQ_EndTransaction();
    sleep(2);

    printf("\n ******* Query successfully ran for 3 seconds, shutting down...\n");
    WSQ_Shutdown();
    
    printf("Shutdown apparently successful.\n");
    return 0;
}
