
#include <stdlib.h>
#include <stdio.h>
#include "wsq_runtime.h"

// An example demonstrating how to link and use the WSQ runtime system.

#include "port.h"

int main(int argc, char* argv[]) { 

  WSQ_Init();

  WSQ_BeginTransaction(1001);
    WSQ_BeginSubgraph(101);
      WSQ_AddReutersSource(2, "foobar.schema");
      WSQ_ConnectRemoteOut(2, "fort2.csail.mit.edu", PORT); 
    WSQ_EndSubgraph();
  WSQ_EndTransaction();

  WSQ_Shutdown();
  return 0;
}

