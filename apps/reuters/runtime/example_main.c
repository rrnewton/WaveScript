
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include "wsq_runtime.h"

// An example demonstrating how to link and use the WSQ runtime system.

int offset = 0;

void transaction1();

int main(int argc, char* argv[]) {

//  WSQ_Init("query_output.log");
  WSQ_Init(NULL); // Null argument means do not specify an output file.
  WSQ_SetQueryName("foobar"); // Optional, affects output file name.

//  WSQ_SetBackend(SCHEME_BACKEND); // Optional

  // Simple Example:
  //================================================================================

  if (1) {
    WSQ_BeginTransaction(99);
       WSQ_BeginSubgraph(11);
        WSQ_AddOp(1, "RandomSource", "", "100", "10 |foobar.schema");
        //WSQ_AddOp(1, "ASCIIFileSource", "", "100", "100 |foobar.schema|short.log");

        //WSQ_AddOp(2, "Filter", "100", "200", "(PRICE >= 50) AND (PRICE <= (50 * 10))");

        //WSQ_AddOp(3, "MatchRecognize", "100", "200", "ALL | A B | A AS (BID > 50), B AS (SYMBOL = \"IBM\")");

        // Here's an example which will happen quite rarely:
        //WSQ_AddOp(2, "MatchRecognize", "100", "200", "ONE | B B B B B B | B AS (SYM = \"IBM\")");

       // WSQ_AddOp(3, "Filter", "300", "400", "(PRICE >= 50) AND (PRICE <= (50 * 10))");

        WSQ_AddOp(4, "Printer", "100", "", "YAY:");
       WSQ_EndSubgraph();
    int pid = WSQ_EndTransaction();
    printf("EndTransaction returned to main C program, reports PID %d\n", pid);
    fflush(stdout);
    sleep(3);
  }


  // More complex example:
  //================================================================================

  if (0) {
      transaction1();
      sleep(3);

      if (0) {
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
      } 

      sleep(3);
  }

  printf("\n ******* Query successfully ran for 3 seconds, shutting down...\n");
  WSQ_Shutdown();
}

void transaction1() {
    char buf1[128];
    char buf2[128];
    WSQ_BeginTransaction(10001 + offset);
     WSQ_BeginSubgraph(1001 + offset);

        sprintf(buf2,"%d", 200 + offset);
      WSQ_AddOp(2 + offset, "ReutersSource", "", buf2, "15 |foobar.schema");

        sprintf(buf1,"%d", 300 + offset);
      WSQ_AddOp(4+offset, "Filter", buf2,buf1, "(SYM = \"IBM\") AND (PRICE >= (2 + 2))");

        sprintf(buf2,"%d", 400 + offset);
      //WSQ_AddOp(5+offset, "Project", buf1,buf2, "SYM, TIME, PRICE");
      WSQ_AddOp(5+offset, "Project", buf1,buf2, "SYM, TIME, (PRICE + 1000) AS PRICE");

      if (1) 
      {
          char msg[128];
          sprintf(msg, "STOCKSTRM(id %d): ", 6+offset);
          WSQ_AddOp(6+offset, "Printer", buf2, "", msg);
      } 
      else 
      {
          // Add a second stream and do a join:
          sprintf(buf1,"%d", 500 + offset);
          WSQ_AddOp(7+offset, "ReutersSource", "", buf1, "10 |foobar.schema");

          sprintf(buf1,"%d %d", 400+offset, 500+offset); // Two inputs
          sprintf(buf2,"%d", 600 + offset);
          // WSQ_AddOp(8+offset, "WindowJoin", buf1, buf2, "30 | SYM == SYM");

          WSQ_AddOp(8+offset, "WindowJoin", buf1, buf2, " 30 | A | B | A.SYM == B.SYM");
          //WSQ_AddOp(8+offset, "WindowJoin", buf1, buf2, " TIME | 30 | A | B | A.SYM == B.SYM");
          //WSQ_AddOp(8+offset, "WindowJoin", buf1, buf2, " A | B | (abs (A.TIME - B.TIME)) <= 30 | A.SYM == B.SYM");

          WSQ_AddOp(9+offset, "Printer", buf2, "", "JOINED: ");
      }
             
     WSQ_EndSubgraph();

     /* WSQ_BeginSubgraph(102 + offset); */
     /*   // Empty subgraph. */
     /* WSQ_EndSubgraph(); */
    WSQ_EndTransaction();
}

