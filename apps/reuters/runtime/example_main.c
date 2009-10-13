
#include "wsq_runtime.h"

// An example demonstrating how to link and use the WSQ runtime system.

int main(int argc, char* argv[]) { 
  //   do_scheme(argc,
  WSQ_Init();

  WSQ_BeginTransaction(1001);

  WSQ_BeginSubgraph(101);

  WSQ_AddReutersSource(2, "foobar.schema");
  
  WSQ_AddFilter(2,3, "FOO == 333, BAR == 444");
  WSQ_AddProject(3,4, "FOO, BAR");

  WSQ_ConnectRemoteOut(4, "honor.csail.mit.edu", 9898); 

  WSQ_EndSubgraph();
  WSQ_EndTransaction();

  WSQ_Shutdown();
}

