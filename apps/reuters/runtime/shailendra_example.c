// Description of example usecase:

/*
MSFT price moves outside 2% of MSFT-15 minute-VWAP (A) 
   FOLLOWED BY (   S&P moving by 0.5%     (B) 
   AND (
        IBM's price moves up by 5%   (C1)
        OR  MSFT 's price moves down by 2% (C2) )
   ALL WITHIN any 2 minute time period

THEN  BUY MSFT, SELL IBM

where VWAP stands for

 Volume Weighted Average Price and is given by
 SUM(Volume*Price)/SUM(Volume) over the specified period of time.

Interpretation
  Let’s state the specific interpretation for which we work out the solution:

Clauses B and C are allowed to appear in any order.  Further, there
may be other trades involving these or other instruments between A and
B or C1 or C2.  With regards to the base against which the relative
movement mentioned in clause B is computed, we use the following
interpretation

Let t be the time at which there is a match for condition A. Then there 
is a match for condition B if there are instants of time t1, t2, such that 

     t < t1 < t2 < t+2 mins AND
   | S&P(t1) - S&P(t2) |  <= 0.005*S&P(t1) 

Note t1 and t2 need not be successive members of the discrete ordered time domain T 
Also, for simplicity (and WLOG) we assume only one trade per instrument at any instant of time
We use a similar interpretation for clauses C1 and C2 
*/

/************************************************************************************************** 
   NOTES 

[2010.07.06] Right now the user query is causing a bus error (the
15 min wins by themselves are fine.)

[2010.07.06] Also, I'm seeing occasional oversized windows.  15 min
windows should be 900 seconds.  But some windows are coming out >
1000.  There must be a bug in wsq_window.

15 MIN WIN: (END=2167, RATIO=1.02603, WIDTH=895)
15 MIN WIN: (END=2187, RATIO=1.04566, WIDTH=894)
15 MIN WIN: (END=2189, RATIO=1.02937, WIDTH=896)
15 MIN WIN: (END=2232, RATIO=1.0281, WIDTH=898)
15 MIN WIN: (END=2247, RATIO=1.02741, WIDTH=1143)
15 MIN WIN: (END=3167, RATIO=1.04293, WIDTH=895)
15 MIN WIN: (END=3185, RATIO=1.04049, WIDTH=900)
15 MIN WIN: (END=3203, RATIO=1.03239, WIDTH=899)
15 MIN WIN: (END=3220, RATIO=1.03386, WIDTH=898)

**************************************************************************************************/


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

// Create a randomly generated user query.  Takes two arguments:
// 
// (1) The all_ticks stream is the full stock_tick source.
//     We treat the S&P just as a separate stock tick with symbol "S&P"
//
// (2) The msft_vwap_15 stream consists of 15 minute windows of data
// which have ALREADY been filtered so that they represent only
// windows where the most recent (last) sample has gone above the
// average of the whole window.
stream_id add_user_query(int offset, stream_id all_ticks, stream_id msft_selected_15) {
    
     WSQ_BeginSubgraph(99000 + offset);
     {
         // First, we pull out two minute windows, the rest of the
         // predicates will be a function of the last two minutes.
         stream_id twomins_msft = 100 + offset;
         stream_id twomins_snp  = 102 + offset;
         stream_id twomins_ibm  = 104 + offset;

         stream_id temp1        = 101 + offset;
         stream_id temp2        = 103 + offset;

         stream_id filtered_ibm  = 105 + offset;
         stream_id filtered_msft = 106 + offset;
         stream_id filtered_snp  = 107 + offset;

         // (1) This strategy involves filtering THEN windowing for all ticks of interest 
         if (0) {
             // (B) The S&P has moved by two percent:
             WSQ_AddOp_1_1(53+offset, "Filter", all_ticks, temp1, "SYM = \"S&P\"");
             WSQ_AddOp_1_1(54+offset, "Window", temp1, twomins_ibm, "TIME | 2 * 60 | 1 TUPLE");
             WSQ_AddOp_1_1(55+offset, "Filter", twomins_ibm, filtered_ibm, "(absF (((MAX(PRICE)) / (MIN(PRICE))) - 1.0)) >= 0.02");

             // There's a RISC/CISC style tradeoff here.  Without smart
             // optimizations, we can do better with much more complex operators.
             // There would even be some argument for combining Filter, Window, and Project here.
             //
             // This WindowProject would require tuples that mix sigsegs (windows) and scalars.
             // WSQ_AddOp_1_1(54+offset, "WindowProject", temp1, twomins_ibm, 
             //               "FST SND | (absF (FST.TIME - SND.TIME)) <= (2 * 60) | *, (MAX(PRICE)) AS MX, (MIN(PRICE)) AS MN");
             // WSQ_AddOp_1_1(55+offset, "Filter", twomins_ibm, filtered_ibm, "(absF ((MX / MN) - 1.0)) >= 0.02");


             // (C1) IBM Goes up by 5%
             WSQ_AddOp_1_1(54+offset, "Filter", all_ticks, temp1, "SYM = \"IBM\"");
             WSQ_AddOp_1_1(55+offset, "Window", temp1, twomins_ibm, "TIME | 2 * 60 | 1 TUPLE");
             WSQ_AddOp_1_1(56+offset, "Filter", twomins_ibm, filtered_ibm, "((MAX(PRICE)) / (MIN(PRICE)) >= 1.05) AND GOING_UP??");

             // (C2) Microsoft moves down by 2%
             // TODO: Rewindow the existing 15 min stream.  Should be able to share storage via sigsegs.
             //WSQ_AddOp_1_1(57+offset, "ReWindow", msft_selected_15, twomins_msft, "TIME | 2 * 60 | 1 TUPLE");
             WSQ_AddOp_1_1(57+offset, "Filter", all_ticks, temp2, "SYM = \"MSFT\"");
             WSQ_AddOp_1_1(58+offset, "Window", temp2, twomins_msft, "TIME | 2 * 60 | 1 TUPLE");
             WSQ_AddOp_1_1(59+offset, "Filter", twomins_msft, filtered_msft, "((MAX(PRICE)) / (MIN(PRICE)) <= 0.98) AND GOING_DOWN??");

             // The 2-min windows individually match the constraint that their
             // [start,end] intervals cover 2 min or less, but those aren't
             // necessarily THE SAME two minutes.  What we really want to do is
             // prune them all to fit in the last two minutes from NOW.
         
             // Or I guess we could have windowed BEFORE pulling out the different stock ticks.

             stream_id down_or_up = 108 + offset;
             WSQ_AddOp_2_1(58+offset, "JoinWindows", twomins_ibm, twomins_msft, down_or_up, "TIME | ?????");
             //WSQ_AddOp_2_1(58+offset, "ZIP", twomins_ibm, twomins_msft, down_or_up, "TIME | ?????");

             // Really we should probably prune them all to "NOW"


             // Finally if either C1 or C2 occurs with (B)
             stream_id final = 109 + offset;
             WSQ_AddOp_2_1(58+offset, "Join", down_or_up, twomins_snp, final, "WINDOWS ?????");
             return final;
         }

         // (2) This strategy instead creates one series of two minute
         // windows, then filters those windows for the appropriate stock ticks.
         if(1) {
             stream_id filtered_all  = 108 + offset;
             stream_id filtered_both = 109 + offset;
             stream_id twomins_all   = 110 + offset;
             stream_id twomins_all2  = 111 + offset;
             stream_id twomins_final = 112 + offset;
             stream_id final         = 113 + offset;
             stream_id bc_winners    = 114 + offset;
             stream_id parent_timepoints = 115 + offset; 

             WSQ_AddOp_1_1(54+offset, "Filter", all_ticks, filtered_all, "((SYM = \"MSFT\") OR (SYM = \"IBM\")) OR (SYM = \"S&P\")");
             // This ensures that all the per-symbol windows will fit within THE SAME two minutes.
             // (But it still says nothing about NOW.)
             WSQ_AddOp_1_1(55+offset, "Window", filtered_all, twomins_all, "TIME | 2 * 60 | 1 TUPLE");
             
             // If we're going to make this efficient at all we need
             // to chain these predicates and not execute all of them
             // if not necessary.
             
             // But we still need some kind of ZIP or way to first
             // filter a window, and then bring back the other symbol
             // data.
             
             // Either that or we need to be able to do the FIRST/LAST
             // based filter on just a SUBSET of the tuples in a
             // window (Essentially FIRST/WHERE).
              
             if (0) { // The below does 3 different filtered versions of the window, but the JOIN strategy won't work with overlapping windows...
                 // (B) The S&P has moved by two percent:
                 // This should not need to be a separate operator... a normal filter should suffice...
                 // But we don't currently "type check" the edges for windowed or not before passing to WS..
                 WSQ_AddOp_1_1(57+offset, "FilterWindows", twomins_all, twomins_snp,  "(SYM = \"S&P\")");
                 WSQ_AddOp_1_1(58+offset, "Filter", twomins_snp, filtered_snp, "(((LAST(PRICE)) / (FIRST(PRICE))) <= 0.98)");

                 // Join it back in, the LEFT ONLY join includes only columns from the left (and preserves windowing):
                 //WSQ_AddOp_2_1(58+offset, "JOIN", twomins_all, filtered_snp, joined, "MONOTONIC | A B | (FIRST(A.TIME)) == (FIRST(B.TIME))");
                 //
                 // The filtered window will fit inside the original:
             
                 // This JOIN strategy won't actually work with sliding windows:
                 char* joinstr = "MONOTONIC LEFT ONLY | A B | ((FIRST(A.TIME)) <= (FIRST(B.TIME))) AND ((LAST(A.TIME)) >= (LAST(B.TIME)))";
                 WSQ_AddOp_2_1(59+offset, "Join", twomins_all, filtered_snp, twomins_all2, joinstr);

                 // (C1) IBM Goes up by 5%
                 WSQ_AddOp_1_1(60+offset, "FilterWindows", twomins_all2, twomins_ibm,  "(SYM = \"IBM\")");
                 WSQ_AddOp_1_1(61+offset, "Filter", twomins_ibm, filtered_ibm, "(((LAST(PRICE)) / (FIRST(PRICE))) >= 1.05)");
             
                 // (C2) OR Microsoft moves down by 2%
                 WSQ_AddOp_1_1(62+offset, "FilterWindows", twomins_all2, twomins_msft,  "(SYM = \"MSFT\")");
                 WSQ_AddOp_1_1(63+offset, "Filter", twomins_msft, filtered_msft, "(((LAST(PRICE)) / (FIRST(PRICE))) <= 0.98)");

                 // Merge C1 and C2 filtered streams in TIME order (maybe duplicates in same time range).

                 // The expression here extracts the time stamp for each
                 // window, the merged output will be monotonically
                 // increasing in that quantity (and this assumes that the original inputs
                 // were as well).
                 WSQ_AddOp_2_1(64+offset, "MergeMonotonic", filtered_ibm, filtered_msft, filtered_both, " FIRST(TIME) ");

                 // Finally, join back in the full data, duplicates will be ignored:
                 WSQ_AddOp_2_1(65+offset, "Join", twomins_all2, filtered_msft, twomins_final, joinstr);

                 // What should the output of the whole query be?  We can
                 // simply output (start,end) timestamp ranges for the data windows that
                 // made it through.             

                 WSQ_AddOp_1_1(66+offset, "Project", twomins_final, final, " (FIRST(TIME)) AS START, (LAST(TIME)) AS END");
             }

             // Alright, here is something that works but requires the hackish introduction of FIRSTWHERE

             // Leave the contents of windows alone, but filter for windows that satisfy (B):
             WSQ_AddOp_1_1(58+offset, "Filter", twomins_all, filtered_snp, "(((LASTWHERE(PRICE, SYM == \"S&P\")) / (FIRSTWHERE(PRICE, SYM == \"S&P\"))) <= 0.98)");

             
             // Next (C1) OR (C2)
             WSQ_AddOp_1_1(59+offset, "Filter", filtered_snp, twomins_final, 
                           "(((LASTWHERE(PRICE, SYM == \"IBM\")) / (FIRSTWHERE(PRICE, SYM == \"IBM\"))) >= 1.05) OR (((LASTWHERE(PRICE, SYM == \"MSFT\")) / (FIRSTWHERE(PRICE, SYM == \"MSFT\"))) <= 0.98)");

             // Finally project out the start/end times of the windows that made it through...
             WSQ_AddOp_1_1(60+offset, "Project", twomins_final, bc_winners, " (FIRST(TIME)) AS START, (LAST(TIME)) AS END");
             
             // One more thing, in this inefficient version we always compute (B) & (C1/C2), and then finally we join with (A)
             // These are the endpoints of the MSFT windows:
             WSQ_AddOp_1_1(65+offset, "Project", msft_selected_15, parent_timepoints, " -1 AS START, (LAST(TIME)) AS END");

             // Now we merge that stream 

             WSQ_AddOp_2_1(64+offset, "MergeMonotonic", parent_timepoints, bc_winners, final, " END ");

             // TODO: one more thing to do, look for temporal pattern
             // of (A) followed by (B/C1/C2) within some short period of time.
             WSQ_AddOp(67, "Printer", "115", "", "fired: ");
             //WSQ_AddOp(67, "Printer", "114", "", "BUYIT!: "); // Bus error:

             return final;
         }
     }
     WSQ_EndSubgraph();
}


int main(int argc, char* argv[]) {

  //WSQ_Init("query_output.log");
  WSQ_Init("");
  WSQ_BeginTransaction(99);
  //================================================================================
  {
      stream_id source        = 99;
      stream_id filtered      = 96;
      stream_id windowed      = 98;
      stream_id vwap_filtered = 97;

      WSQ_BeginSubgraph(999999);
      {
          WSQ_AddOp(49, "ReutersSource", "", "99", "100073 |foobar.schema");

          // (A) MSFT 15 min vwap moves outside of 2%:

          // First filter the MSFT ticks:
          WSQ_AddOp_1_1(48, "Filter", source, filtered,  "(SYM = \"MSFT\")");
          
          // Next: we compute the volume-weighted average price over 15 min windwows:          
          // WSQ_AddOp_1_1(50, "Window", source, windowed, "FST SND | (absF (FST.TIME - SND.TIME)) <= (15 * 60)");
          WSQ_AddOp_1_1(50, "Window", filtered, windowed, " TIME | 15 * 60 | 1 TUPLE");
          WSQ_AddOp_1_1(51, "Filter", windowed, vwap_filtered, 
                        "((LAST(PRICE)) / ((SUM((CAST(VOLUME AS FLOAT)) * PRICE)) / (CAST((SUM(VOLUME)) AS FLOAT)))) >= 1.02");

          // Debugging: print a diagnistic at the source:
          if (0) {
              // Let us project here just to print:
              WSQ_AddOp_1_1(41, "Project", vwap_filtered, 103 , 
                 "((LAST(PRICE)) / ((SUM((CAST(VOLUME AS FLOAT)) * PRICE)) / (CAST((SUM(VOLUME)) AS FLOAT)))) AS RATIO, (LAST(TIME)) - (FIRST(TIME)) AS WIDTH, LAST(TIME) AS END");

              WSQ_AddOp(42, "Printer", "103", "", "      15 MIN WIN: ");
              //WSQ_AddOp(42, "Printer", "96", "", "      15 MIN WIN: ");
          }
      }
      WSQ_EndSubgraph();

      // Now add multiple random queries onto that:
      int i;
      for (i=0; i < NUM_QUERIES; i++) {
          add_user_query(1000 * i, source, vwap_filtered);
      }
  }
  //================================================================================
  WSQ_EndTransaction();
  sleep(3);
  printf("\n ******* Query successfully ran for 3 seconds, shutting down...\n");
  WSQ_Shutdown();
}



