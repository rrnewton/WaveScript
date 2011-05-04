
#include <stdlib.h>
#include <stdio.h>
#include "wsq_runtime.h"

// An example demonstrating how to link and use the WSQ runtime system.

#include "port.h"

int main(int argc, char* argv[]) { 
    char strargs[128];
    //sprintf(strargs, "128.30.79.5 | %d | string SYM, float TIME, float PRICE, int VOLUME", PORT);
    //sprintf(strargs, "localhost | %d | \"string SYM, float TIME, float PRICE, int VOLUME\"", PORT);

    // This is the schema for RandomSource:
    // sprintf(strargs, "localhost | %d | string SYM, float TIME, float PRICE, int VOLUME", PORT);

    // This is the schema for ASCIIFileSource:
    // CHANGE TO BIGINT!!! TODO FIXME
    sprintf(strargs, "localhost | %d | bigint TIMESTAMP, string SYMBOL, bigint EXCHTIMESTAMP, bigint RECEIVEDTIME, double BID, double BIDSIZE, double ASK, double ASKSIZE ", PORT);


  WSQ_Init("");
  WSQ_SetQueryName("proc2query");

  WSQ_Pause();
  printf("PAUSING WSQ engine, run compiled query manually...\n");

  WSQ_BeginTransaction(1001);
    WSQ_BeginSubgraph(101);
    //type DummySchema99 = (| SYM:String, TIME:Float, PRICE:Float, VOLUME:Int);
    //      WSQ_ConnectRemoteIn(20,"localhost", PORT, "string BAZ, float BAR");

      WSQ_AddOp(20, "ConnectRemoteIn", "", "20", strargs);
          WSQ_AddOp(21, "Printer", "20", "", "NETSTRM: ");

    WSQ_EndSubgraph();
  WSQ_EndTransaction();

  //sleep(1000);
  sleep(1);

  printf("Done Sleeping.  Shutting down WSQ Engine...\n");
  WSQ_Shutdown();
  return 0;
}

