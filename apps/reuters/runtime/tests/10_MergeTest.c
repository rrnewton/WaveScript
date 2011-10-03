

// This test merges two copies of the same file.

#include <stdio.h>
#include "wsq_runtime.h"
#include <time.h>

// Merge monotonic matches fields that montonically increase in both of two input streams.

int main(int argc, char* argv[]) {
  WSQ_Init("10_MergeTest.out");
  WSQ_SetQueryName("generated_query_10");

    WSQ_BeginTransaction(99);
     WSQ_BeginSubgraph(11);
       WSQ_AddOp(1, "ASCIIFileSource", "", "100", "-1 |foobar.schema|TAQ.10000");
       WSQ_AddOp(2, "ASCIIFileSource", "", "200", "-1 |foobar.schema|TAQ.10000");

       // Now merge them together:
       // ============================================================
       WSQ_AddOp(3, "MergeMonotonic", "100 200", "300", "TIMESTAMP");
       // ============================================================

       WSQ_AddOp(6, "Printer", "300", "", "Merged");

     WSQ_EndSubgraph();
    int pid = WSQ_EndTransaction();
    int status = 0;

    printf("\n ******* Waiting on PID %d...\n", pid);

    time_t start = time(NULL);
    waitpid(pid, &status, 0);
    time_t end   = time(NULL);

    printf("\n ******* PID %d terminated (after %ld seconds), now shutting down.\n", pid, 
           (end - start)
           );
    WSQ_Shutdown();

    printf("Shutdown apparently successful.\n");
    return 0;
}
