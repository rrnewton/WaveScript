
#include <stdlib.h>
#include <stdio.h>
#include "wsq_runtime.h"

// An example demonstrating how to link and use the WSQ runtime system.

void transaction1() {
    WSQ_BeginTransaction(1001);

    WSQ_BeginSubgraph(101);

    WSQ_AddReutersSource(2, "foobar.schema");

    WSQ_AddPrinter("STOCKSTRM: ", 2);

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
    WSQ_BeginSubgraph(102);
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
  printf("Query run for a 3 seconds, doing another transaction.\n");

  transaction1();
  sleep(3);
  printf("Ok, then one more time...\n");

  transaction1();
  sleep(3);
  printf("Query run for another 3 seconds shutting down...\n");

  WSQ_Shutdown();
}

