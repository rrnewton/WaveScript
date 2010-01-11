
#include <stdlib.h>
#include <stdio.h>
#include "wsq_runtime.h"

// An example demonstrating how to link and use the WSQ runtime system.

int offset = 0;

void transaction1() {
    WSQ_BeginTransaction(1001 + offset);

     WSQ_BeginSubgraph(101 + offset);

      WSQ_AddReutersSource(2 + offset, "foobar.schema");
      WSQ_AddPrinter("STOCKSTRM: ", 2 + offset);

    /*
      WSQ_AddFilter(2,3, "SYM == \"IBM\", PRICE >= (40 + 40)");
      WSQ_AddProject(3,4, "SYM, TIME, PRICE");
      WSQ_AddPrinter("STOCKSTRM: ", 4);

      // Add a second stream and do a join:
      WSQ_AddReutersSource(5, "foobar.schema");

      // WSQ_AddProject(3,6,"SYM, VOLUME");
      WSQ_AddWindowJoin(5,4,7, 30, "SYM == SYM"); // TUPLES, 
      WSQ_AddPrinter("Joined: ", 7);
    */

    //WSQ_ConnectRemoteOut(4, "honor.csail.mit.edu", 9898); 
      WSQ_EndSubgraph();

    // This simple subgraph just routes a stream through to another machine.
    WSQ_BeginSubgraph(102 + offset);
    //WSQ_ConnectRemoteIn(20,"honor.csail.mit.edu", 9897, "string BAZ, float BAR");
    //WSQ_AddPrinter("NETSTRM: ", 20);
    //WSQ_ConnectRemoteOut(20, "chastity.csail.mit.edu", 9896); 
    WSQ_EndSubgraph();

    WSQ_EndTransaction();
}

int main(int argc, char* argv[]) {

  WSQ_Init();

  transaction1();
  //printf(" ******* ENDTRANSACTION finished... sleeping now \n");
  sleep(3);
  printf("\n ****** Query run for a 3 seconds, doing another transaction.\n\n");

  offset += 1000;
  transaction1();
  sleep(3);
  printf("\n ******* Ok, then one more time...\n\n");

  offset += 1000;
  transaction1();
  sleep(3);
  printf("\n ******* Query successfully ran for another 3 seconds, shutting down...\n");

  WSQ_Shutdown();
}

