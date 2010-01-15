
#include <stdlib.h>
#include <stdio.h>
#include "wsq_runtime.h"

// An example demonstrating how to link and use the WSQ runtime system.

int offset = 0;

void transaction1();

int main(int argc, char* argv[]) {

  WSQ_Init();

  transaction1();
  sleep(3);
  printf("\n ****** Query run for a 3 seconds, doing another transaction.\n\n");

  offset += 1000;
  transaction1();
  sleep(3);

  /* printf("\n ******* Ok, then one more time...\n\n"); */
  /* offset += 1000; */
  /* transaction1(); */

  printf("\n ******* REMOVING one of those subgraphs...\n\n"); 
  WSQ_BeginTransaction(9999); 
    WSQ_RemSubgraph(101);
  WSQ_EndTransaction(); 

  sleep(3);

  printf("\n ******* Query successfully ran for another 3 seconds, shutting down...\n");

  WSQ_Shutdown();
}

void transaction1() {
    char buf1[128];
    char buf2[128];
    WSQ_BeginTransaction(1001 + offset);
     WSQ_BeginSubgraph(101 + offset);

        sprintf(buf2,"%d", 2 + offset);
      WSQ_AddOp(2 + offset, "ReutersSource", "", buf2, "foobar.schema");

        sprintf(buf1,"%d", 3 + offset);
      WSQ_AddOp(4+offset, "Filter", buf2,buf1, "SYM == \"IBM\", PRICE >= (2 + 2)");

        sprintf(buf2,"%d", 4 + offset);
      WSQ_AddOp(5+offset, "Project", buf1,buf2, "SYM, TIME, PRICE");


        char msg[128];
        sprintf(msg, "STOCKSTRM(id %d): ", 6+offset);
      WSQ_AddOp(6+offset, "Printer", buf2, "", msg);

      // Add a second stream and do a join:
        sprintf(buf1,"%d", 5 + offset);
      WSQ_AddOp(7+offset, "ReutersSource", "", buf1, "foobar.schema");

        sprintf(buf1,"%d %d", 4+offset, 5+offset); // Two inputs
        sprintf(buf2,"%d", 6 + offset);
      WSQ_AddOp(8+offset, "WindowJoin", buf1, buf2, "30 | SYM == SYM");

      WSQ_AddOp(9+offset, "Printer", buf2, "", "STOCKSTRM: ");
             
     WSQ_EndSubgraph();

     /* WSQ_BeginSubgraph(102 + offset); */
     /*   // Empty subgraph. */
     /* WSQ_EndSubgraph(); */
    WSQ_EndTransaction();
}

