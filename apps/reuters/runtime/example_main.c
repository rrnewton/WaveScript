
#include <stdlib.h>
#include <stdio.h>
#include "wsq_runtime.h"

// An example demonstrating how to link and use the WSQ runtime system.

int main(int argc, char* argv[]) { 

  WSQ_Init();

  WSQ_BeginTransaction(1001);

    WSQ_BeginSubgraph(101);
      WSQ_AddReutersSource(2, "foobar.schema");
      WSQ_AddFilter(100, 2,3, "SYM == \"IBM\", PRICE >= (40 + 40)");
       WSQ_AddProject(3,4, "SYM, TIME, PRICE");
      WSQ_AddPrinter("STOCKSTRM: ", 4);
      WSQ_ConnectRemoteOut(4, "honor.csail.mit.edu", 9898); 
    WSQ_EndSubgraph();

    // This simple subgraph just routes a stream through to another machine.
    WSQ_BeginSubgraph(102);
      WSQ_ConnectRemoteIn(20,"honor.csail.mit.edu", 9897, "string BAZ, float BAR");
      WSQ_AddPrinter("NETSTRM: ", 20);
      WSQ_ConnectRemoteOut(20, "chastity.csail.mit.edu", 9896); 
    WSQ_EndSubgraph();

  WSQ_EndTransaction();

  WSQ_Shutdown();
}

