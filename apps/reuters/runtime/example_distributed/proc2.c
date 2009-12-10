
#include <stdlib.h>
#include <stdio.h>
#include "wsq_runtime.h"

// An example demonstrating how to link and use the WSQ runtime system.

#include "port.h"

int main(int argc, char* argv[]) { 

  WSQ_Init();

  WSQ_BeginTransaction(1001);
    WSQ_BeginSubgraph(101);
    //type DummySchema99 = (| SYM:String, TIME:Float, PRICE:Float, VOLUME:Int);
    //      WSQ_ConnectRemoteIn(20,"localhost", PORT, "string BAZ, float BAR");
      WSQ_ConnectRemoteIn(20,"128.30.79.5", PORT, "string SYM, float TIME, float PRICE, int VOLUME");
      WSQ_AddPrinter("NETSTRM: ", 20);
    WSQ_EndSubgraph();
  WSQ_EndTransaction();

  WSQ_Shutdown();
  return 0;
}

