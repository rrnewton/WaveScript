//====================================================================================================
// DUPLICATED CODE WITH shailendra_example.c
//====================================================================================================


#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include "wsq_runtime.h"

// An example demonstrating how to link and use the WSQ runtime system.

//const int NUM_QUERIES = 1000;
const int NUM_QUERIES = 1;

typedef int stream_id;


// Some convenience functions:
void WSQ_AddOp_1_1(int node_id, const char* optype, int in, int out, const char* args) {
    // This annoying API requires string buffers... what jerk designed it?
    char buf1[128];
    char buf2[128];
    sprintf(buf1,"%d", in);  
    sprintf(buf2,"%d", out);
    WSQ_AddOp(node_id, optype, buf1, buf2, args);
}
void WSQ_AddOp_2_1(int node_id, const char* optype, int in1, int in2, int out, const char* args) {
    char buf1[128];
    char buf2[128];
    sprintf(buf1,"%d %d", in1, in2);  
    sprintf(buf2,"%d", out);
    WSQ_AddOp(node_id, optype, buf1, buf2, args);
}

// A table of symbols so that we can generate
// Curretnly 246 different stock ticks:
const char all_symbols[][10] = 
    { "IBM", "GOOG", "GM", "F", "IMGN", 
      // Supplementing this with a bunch of other symbols:
      "AAPL", "AAUKY", "AAV", "AAWW", "AB", "ABAX", "ABB", "ABC", "ABFS", "ABG", "ABM", "ABMD", "ABT", "ABV", "ABVT", "ABX",
      "ACC", "ACCL", "ACE", "ACET", "ACF", "ACGL", "ACGY", "ACH", "ACI", "ACIW", "ACL", "ACLI", "ACM", "ACN", "ACO", "ACOM",
      "ACOR", "ACTG", "ACV", "ACXM", "ADBE", "ADCT", "ADI", "ADM", "ADP", "ADRE", "ADS", "ADSK", "ADTN", "ADVS", "ADY",
      "AEE", "AEG", "AEIS", "AEL", "AEM", "AEO", "AEP", "AER", "AES", "AET", "AEZ", "AF", "AFAM", "AFFX", "AFFY", "AFG", "AFL",
      "AFSI", "AGAM", "AGCO", "AGG", "AGII", "AGL", "AGM", "AGN", "AGNC", "AGO", "AGP", "AGQ", "AGU", "AGYS", "AHGP", "AHL",
      "AHS", "AHT", "AIG", "AIMC", "AIN", "AINV", "AIPC", "AIR", "AIRM", "AIT", "AIV", "AIXG", "AIZ", "AJG", "AKAM", "AKR",
      "AKS", "ALB", "ALE", "ALEX", "ALGN", "ALGT", "ALJ", "ALK", "ALKS", "ALL", "ALNY", "ALOG", "ALSK", "ALTE", "ALTH",
      "ALTR", "ALV", "ALXN", "AM", "AMAG", "AMAT", "AMB", "AMCC", "AMD", "AME", "AMED", "AMG", "AMGN", "AMJ", "AMKR",
      "AMLN", "AMMD", "AMN", "AMP", "AMR", "AMRI", "AMSC", "AMSF", "AMSG", "AMT", "AMTD", "AMX", "AMZN", "AN", "ANDE",
      "ANF", "ANGO", "ANH", "ANN", "ANR", "ANSS", "ANV", "ANW", "AOL", "AON", "AONE", "AOS", "APA", "APAC", "APC", "APD",
      "APEI", "APH", "APKT", "APL", "APOG", "APOL", "APSG", "APU", "APWR", "ARAY", "ARB", "ARBA", "ARCC", "ARD", "ARE",
      "ARG", "ARGN", "ARI", "ARII", "ARJ", "ARLP", "ARM", "ARMH", "ARO", "ARP", "ARRS", "ARST", "ART", "ARUN", "ARW", "ASA",
      "ASBC", "ASCA", "ASEI", "ASF", "ASFI", "ASH", "ASIA", "ASMI", "ASML", "ASPS", "ASTE", "ATAC", "ATHN", "ATHR",
      "ATI", "ATK", "ATLS", "ATMI", "ATNI", "ATO", "ATPG", "ATR", "ATU", "ATVI", "ATW", "AU", "AUO", "AUXL", "AUY", "AVA",
      "AVAV", "AVB", "AVD", "AVGO", "AVID", "AVP", "AVT", "AVTR", "AVY", "AWC", "AWH", "AWI", "AWK", "AXAHY", "AXE", "AXL",
      "AXP", "AXS", "AYE", "AYI", "AYR", "AZN", "AZO", "AZSEY"
    };

int main(int argc, char* argv[]) {

  int PORT = atoi(argv[1]);
  printf("CONNECTING TO PORT: %d\n", PORT);

  //WSQ_Init("query_output.log");
  WSQ_Init("");
  WSQ_SetQueryName("wsq_server");
  WSQ_BeginTransaction(99);
  //================================================================================
  {
      stream_id source        = 99;
      stream_id filtered      = 96;
      stream_id windowed      = 98;
      stream_id vwap_filtered = 97;

      WSQ_BeginSubgraph(999999);
      {
          
          //WSQ_AddOp(49, "ReutersSource", "", "99", "100 |foobar.schema");
          //WSQ_AddOp(49, "ReutersSource", "", "99", "100073 |foobar.schema"); // Run at 100KHz
          WSQ_AddOp(49, "ReutersSource", "", "99", "5000000 |foobar.schema");

          //int PORT = 9343;
          char addr[128];
          //sprintf(addr, "fort2.csail.mit.edu | %d", PORT);
          sprintf(addr, "localhost | %d", PORT);
          WSQ_AddOp(3, "ConnectRemoteOut", "99", "", addr); 

/*

          // (A) MSFT 15 min vwap moves outside of 2%:

          // First filter the MSFT ticks:
          WSQ_AddOp_1_1(48, "Filter", source, filtered,  "(SYM = \"MSFT\")");
          
          // Next: we compute the volume-weighted average price over 15 min windwows:          
          // WSQ_AddOp_1_1(50, "Window", source, windowed, "FST SND | (absF (FST.TIME - SND.TIME)) <= (15 * 60)");
          WSQ_AddOp_1_1(50, "Window", filtered, windowed, " TIME | 15 * 60 | 1 TUPLE");
          WSQ_AddOp_1_1(51, "Filter", windowed, vwap_filtered, 
                        "((LAST(PRICE)) / ((SUM((CAST(VOLUME AS FLOAT)) * PRICE)) / (CAST((SUM(VOLUME)) AS FLOAT)))) >= 1.02");
*/
      }
      WSQ_EndSubgraph();

  }
  //================================================================================
  WSQ_EndTransaction();
  sleep(300);
  printf("\n ******* Query successfully ran for 300 seconds, shutting down...\n");

  WSQ_Shutdown();
}



