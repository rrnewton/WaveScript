//WSLIBDEPS: 


#include "/home/newton/wavescript_svk/src/linked_lib/wsc2.h"



struct tuptyp_65 {
  int fld1;
  int fld2;
} 
;
struct tuptyp_67 {
  int fld1;
  struct tuptyp_65 fld2;
  float fld3;
} 
;


int stopalltimers = 0;
char* tmpconstlift_29 ;
char* tmpconstlift_28 ;
char* tmpconstlift_27 ;
char* tmpconstlift_26 ;
char* tmpconstlift_25 ;
char* tmpconstlift_24 ;
char* tmpconstlift_23 ;
char* tmpconstlift_22 ;
void tmpsmp_53(struct tuptyp_67 x_18); // Iter prototype
void s3_1(struct tuptyp_65 pattmp_5); // Iter prototype
void s2_2(char w_9); // Iter prototype
DECLARE_WORKER(0, char, BASE)
DECLARE_WORKER(1, char, s2_2)
DECLARE_WORKER(2, struct tuptyp_65, s3_1)
DECLARE_WORKER(3, struct tuptyp_67, tmpsmp_53)

void tmpsmp_53(struct tuptyp_67 x_18) {
  char ___VIRTQUEUE____19;
  GRAB_WRITEFIFO(BASE);
  printf("%s", tmpconstlift_29);
  {
    int tmpsmp_37 ;
     tmpsmp_37 = (x_18.fld1);
    printf("%d", tmpsmp_37);
  } 
  printf("%s", tmpconstlift_28);
  {
    struct tuptyp_65 tup_21 ;
     tup_21 = (x_18.fld2);
    printf("%s", tmpconstlift_27);
    {
      int tmpsmp_35 ;
       tmpsmp_35 = (tup_21.fld1);
      printf("%d", tmpsmp_35);
    } 
    printf("%s", tmpconstlift_26);
    {
      int tmpsmp_33 ;
       tmpsmp_33 = (tup_21.fld2);
      printf("%d", tmpsmp_33);
    } 
    printf("%s", tmpconstlift_25);
  } 
  printf("%s", tmpconstlift_24);
  {
    float tmpsmp_31 ;
     tmpsmp_31 = (x_18.fld3);
    printf("%g", tmpsmp_31);
  } 
  printf("%s", tmpconstlift_23);
  printf("%s", tmpconstlift_22);
  EMIT(((char)0), char, BASE);
  RELEASE_WRITEFIFO(BASE);
} 

void s3_1(struct tuptyp_65 pattmp_5) {
  char ___VIRTQUEUE____4;
  GRAB_WRITEFIFO(tmpsmp_53);
  {
    int x_6 ;
     x_6 = (pattmp_5.fld1);
    {
      int y_7 ;
       y_7 = (pattmp_5.fld2);
      {
        struct tuptyp_65 tmpsmp_45 = {x_6, x_6};
        {
          struct tuptyp_67 tmpsmp_47 = {y_7, tmpsmp_45, 3.0F};
          EMIT(tmpsmp_47, struct tuptyp_67, tmpsmp_53);
        } 
      } 
    } 
  } 
  RELEASE_WRITEFIFO(tmpsmp_53);
} 

void s2_2(char w_9) {
  char ___VIRTQUEUE____8;
  GRAB_WRITEFIFO(s3_1);
  {
    struct tuptyp_65 tmpsmp_41 = {10, 20};
    EMIT(tmpsmp_41, struct tuptyp_65, s3_1);
  } 
  RELEASE_WRITEFIFO(s3_1);
} 


void initState() {
  /* We may need to start up the Boehm GC or do other standard WS init: */ 
  wsInternalInit();
  TOTAL_WORKERS(4);
  // [2008.11.07] The static data gets allocated against a never-cleared ZCT: 
  #ifdef WS_THREADED 
  #ifdef WS_USE_ZCT 
   zct_t* zct = WSCALLOC(sizeof(zct_t), 1);
  #endif
  #endif
  REGISTER_WORKER(0, char, BASE);
  REGISTER_WORKER(1, char, s2_2);
  REGISTER_WORKER(2, struct tuptyp_65, s3_1);
  REGISTER_WORKER(3, struct tuptyp_67, tmpsmp_53);
  {
    char* tmprc_80 ;
    int* arrtmp_96 = (int*)0;
    if (2 > 0) {
      arrtmp_96 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 2) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      CLEAR_ARR_RC(arrtmp_96);
      SETARRLEN(arrtmp_96, 2);
    } 
    char* tmpchararr_95 = (char*)arrtmp_96;
    memcpy(tmpchararr_95, "(", 2);
     tmprc_80 = tmpchararr_95;
    INCR_ARR_RC(tmprc_80); /* static top-incr, type: (Array Char) */
    INCR_ARR_RC(tmprc_80); /* heap, type: (Array Char) */
     tmpconstlift_29 = tmprc_80;
  } 
  {
    char* tmprc_79 ;
    int* arrtmp_94 = (int*)0;
    if (3 > 0) {
      arrtmp_94 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 3) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      CLEAR_ARR_RC(arrtmp_94);
      SETARRLEN(arrtmp_94, 3);
    } 
    char* tmpchararr_93 = (char*)arrtmp_94;
    memcpy(tmpchararr_93, ", ", 3);
     tmprc_79 = tmpchararr_93;
    INCR_ARR_RC(tmprc_79); /* static top-incr, type: (Array Char) */
    INCR_ARR_RC(tmprc_79); /* heap, type: (Array Char) */
     tmpconstlift_28 = tmprc_79;
  } 
  {
    char* tmprc_78 ;
    int* arrtmp_92 = (int*)0;
    if (2 > 0) {
      arrtmp_92 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 2) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      CLEAR_ARR_RC(arrtmp_92);
      SETARRLEN(arrtmp_92, 2);
    } 
    char* tmpchararr_91 = (char*)arrtmp_92;
    memcpy(tmpchararr_91, "(", 2);
     tmprc_78 = tmpchararr_91;
    INCR_ARR_RC(tmprc_78); /* static top-incr, type: (Array Char) */
    INCR_ARR_RC(tmprc_78); /* heap, type: (Array Char) */
     tmpconstlift_27 = tmprc_78;
  } 
  {
    char* tmprc_77 ;
    int* arrtmp_90 = (int*)0;
    if (3 > 0) {
      arrtmp_90 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 3) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      CLEAR_ARR_RC(arrtmp_90);
      SETARRLEN(arrtmp_90, 3);
    } 
    char* tmpchararr_89 = (char*)arrtmp_90;
    memcpy(tmpchararr_89, ", ", 3);
     tmprc_77 = tmpchararr_89;
    INCR_ARR_RC(tmprc_77); /* static top-incr, type: (Array Char) */
    INCR_ARR_RC(tmprc_77); /* heap, type: (Array Char) */
     tmpconstlift_26 = tmprc_77;
  } 
  {
    char* tmprc_76 ;
    int* arrtmp_88 = (int*)0;
    if (2 > 0) {
      arrtmp_88 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 2) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      CLEAR_ARR_RC(arrtmp_88);
      SETARRLEN(arrtmp_88, 2);
    } 
    char* tmpchararr_87 = (char*)arrtmp_88;
    memcpy(tmpchararr_87, ")", 2);
     tmprc_76 = tmpchararr_87;
    INCR_ARR_RC(tmprc_76); /* static top-incr, type: (Array Char) */
    INCR_ARR_RC(tmprc_76); /* heap, type: (Array Char) */
     tmpconstlift_25 = tmprc_76;
  } 
  {
    char* tmprc_75 ;
    int* arrtmp_86 = (int*)0;
    if (3 > 0) {
      arrtmp_86 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 3) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      CLEAR_ARR_RC(arrtmp_86);
      SETARRLEN(arrtmp_86, 3);
    } 
    char* tmpchararr_85 = (char*)arrtmp_86;
    memcpy(tmpchararr_85, ", ", 3);
     tmprc_75 = tmpchararr_85;
    INCR_ARR_RC(tmprc_75); /* static top-incr, type: (Array Char) */
    INCR_ARR_RC(tmprc_75); /* heap, type: (Array Char) */
     tmpconstlift_24 = tmprc_75;
  } 
  {
    char* tmprc_74 ;
    int* arrtmp_84 = (int*)0;
    if (2 > 0) {
      arrtmp_84 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 2) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      CLEAR_ARR_RC(arrtmp_84);
      SETARRLEN(arrtmp_84, 2);
    } 
    char* tmpchararr_83 = (char*)arrtmp_84;
    memcpy(tmpchararr_83, ")", 2);
     tmprc_74 = tmpchararr_83;
    INCR_ARR_RC(tmprc_74); /* static top-incr, type: (Array Char) */
    INCR_ARR_RC(tmprc_74); /* heap, type: (Array Char) */
     tmpconstlift_23 = tmprc_74;
  } 
  {
    char* tmprc_73 ;
    int* arrtmp_82 = (int*)0;
    if (2 > 0) {
      arrtmp_82 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 2) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      CLEAR_ARR_RC(arrtmp_82);
      SETARRLEN(arrtmp_82, 2);
    } 
    char* tmpchararr_81 = (char*)arrtmp_82;
    memcpy(tmpchararr_81, "\n", 2);
     tmprc_73 = tmpchararr_81;
    INCR_ARR_RC(tmprc_73); /* static top-incr, type: (Array Char) */
    INCR_ARR_RC(tmprc_73); /* heap, type: (Array Char) */
     tmpconstlift_22 = tmprc_73;
  } 
  // We will never need to clear this ZCT, so we can throw it out:
  #ifdef WS_THREADED 
  #ifdef WS_USE_ZCT 
  WSFREE(zct);
  #endif
  #endif
  START_WORKERS();
} 

int main(int argc, char** argv) {
  int counter_s1_3 = 0;
    initState();
  ws_parse_options(argc,argv); /* [2008.08.27] Moving to after initState */ 
  ws_bool_t dummy =TRUE;
  // Insert calls to those timers executing only once (with zero rates)
  // Next, run in a timer loop indefinitely
  while(dummy && !stopalltimers) {
    counter_s1_3++;
    VIRTTICK();
    if (counter_s1_3 == 1) {
      GRAB_WRITEFIFO(s2_2);
      EMIT(((char)0), char, s2_2);
      RELEASE_WRITEFIFO(s2_2);
      counter_s1_3 = 0;
      WAIT_TICKS(1000.0);
    } 
  } 
  // We keep the main function going for other tuples to come through.
  while (print_queue_status()) { sleep(1); }
  return 0;
} 
