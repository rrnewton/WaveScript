//WSLIBDEPS:  -lc


#include "/home/newton/wavescript/src/linked_lib/wsc2.h"



struct tuptyp_55 {
  void* fld1;
  void* fld2;
} 
;


char* tmpconstlift_35 ;
char* tmpconstlift_34 ;
char* tmpconstlift_33 ;
char* tmpconstlift_32 ;
char* tmpconstlift_31 ;
char* tmpconstlift_30 ;
char* tmpconstlift_29 ;
char* tmpconstlift_28 ;
char* tmpconstlift_27 ;
char* tmpconstlift_26 ;
char* tmpconstlift_25 ;
char* tmpconstlift_24 ;
void tmpsmp_49(struct tuptyp_55 x_21); // Iter prototype
void main_2(char __5); // Iter prototype
char free_3 ;
char malloc_4 ;

void tmpsmp_49(struct tuptyp_55 x_21) {
  char ___VIRTQUEUE____22;
  printf("%s", tmpconstlift_35);
  {
    void* tmpsmp_39 ;
     tmpsmp_39 = (x_21.fld1);
    printf("%p", tmpsmp_39);
  } 
  printf("%s", tmpconstlift_34);
  {
    void* tmpsmp_37 ;
     tmpsmp_37 = (x_21.fld2);
    printf("%p", tmpsmp_37);
  } 
  printf("%s", tmpconstlift_33);
  printf("%s", tmpconstlift_32);
  BASE(((char)0)) /* emit */;
} 

void main_2(char __5) {
  char ___VIRTQUEUE____6;
  printf("%s", tmpconstlift_31);
  printf("%s", tmpconstlift_30);
  printf("%s", tmpconstlift_29);
  {
    void* p1_7 ;
     p1_7 = malloc(300);
    printf("%s", tmpconstlift_28);
    printf("%p", p1_7);
    printf("%s", tmpconstlift_27);
    {
      void* p2_9 ;
       p2_9 = malloc(300);
      printf("%s", tmpconstlift_26);
      printf("%p", p2_9);
      printf("%s", tmpconstlift_25);
      {
        char ignored_valInEffect_51 ;
        free(p1_7);
         ignored_valInEffect_51 = ((char)0);
      } 
      {
        char ignored_valInEffect_50 ;
        free(p2_9);
         ignored_valInEffect_50 = ((char)0);
      } 
      printf("%s", tmpconstlift_24);
      {
        struct tuptyp_55 tmpsmp_43 = {p1_7, p2_9};
        tmpsmp_49(tmpsmp_43) /* emit */;
      } 
    } 
  } 
} 


void initState() {
  {
    char* tmprc_58 ;
    int* arrtmp_93 = (int*)0;
    if (2 > 0) {
      arrtmp_93 = (int*)((char*)malloc((sizeof(char) * 2) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      CLEAR_RC(arrtmp_93);
      SETARRLEN(arrtmp_93, 2);
    } 
    char* tmpchararr_92 = (char*)arrtmp_93;
    memcpy(tmpchararr_92, "(", 2);
     tmprc_58 = tmpchararr_92;
    INCR_RC(tmprc_58); /* static top-incr type: (Array Char) */
    INCR_RC(tmprc_58); /* heap type: (Array Char) */
     tmpconstlift_35 = tmprc_58;
  } 
  {
    char* tmprc_59 ;
    int* arrtmp_91 = (int*)0;
    if (3 > 0) {
      arrtmp_91 = (int*)((char*)malloc((sizeof(char) * 3) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      CLEAR_RC(arrtmp_91);
      SETARRLEN(arrtmp_91, 3);
    } 
    char* tmpchararr_90 = (char*)arrtmp_91;
    memcpy(tmpchararr_90, ", ", 3);
     tmprc_59 = tmpchararr_90;
    INCR_RC(tmprc_59); /* static top-incr type: (Array Char) */
    INCR_RC(tmprc_59); /* heap type: (Array Char) */
     tmpconstlift_34 = tmprc_59;
  } 
  {
    char* tmprc_60 ;
    int* arrtmp_89 = (int*)0;
    if (2 > 0) {
      arrtmp_89 = (int*)((char*)malloc((sizeof(char) * 2) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      CLEAR_RC(arrtmp_89);
      SETARRLEN(arrtmp_89, 2);
    } 
    char* tmpchararr_88 = (char*)arrtmp_89;
    memcpy(tmpchararr_88, ")", 2);
     tmprc_60 = tmpchararr_88;
    INCR_RC(tmprc_60); /* static top-incr type: (Array Char) */
    INCR_RC(tmprc_60); /* heap type: (Array Char) */
     tmpconstlift_33 = tmprc_60;
  } 
  {
    char* tmprc_61 ;
    int* arrtmp_87 = (int*)0;
    if (2 > 0) {
      arrtmp_87 = (int*)((char*)malloc((sizeof(char) * 2) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      CLEAR_RC(arrtmp_87);
      SETARRLEN(arrtmp_87, 2);
    } 
    char* tmpchararr_86 = (char*)arrtmp_87;
    memcpy(tmpchararr_86, "\n", 2);
     tmprc_61 = tmpchararr_86;
    INCR_RC(tmprc_61); /* static top-incr type: (Array Char) */
    INCR_RC(tmprc_61); /* heap type: (Array Char) */
     tmpconstlift_32 = tmprc_61;
  } 
  {
    char* tmprc_62 ;
    int* arrtmp_85 = (int*)0;
    if (23 > 0) {
      arrtmp_85 = (int*)((char*)malloc((sizeof(char) * 23) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      CLEAR_RC(arrtmp_85);
      SETARRLEN(arrtmp_85, 23);
    } 
    char* tmpchararr_84 = (char*)arrtmp_85;
    memcpy(tmpchararr_84, "Compiled on platform: ", 23);
     tmprc_62 = tmpchararr_84;
    INCR_RC(tmprc_62); /* static top-incr type: (Array Char) */
    INCR_RC(tmprc_62); /* heap type: (Array Char) */
     tmpconstlift_31 = tmprc_62;
  } 
  {
    char* tmprc_63 ;
    int* arrtmp_83 = (int*)0;
    if (7 > 0) {
      arrtmp_83 = (int*)((char*)malloc((sizeof(char) * 7) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      CLEAR_RC(arrtmp_83);
      SETARRLEN(arrtmp_83, 7);
    } 
    char* tmpchararr_82 = (char*)arrtmp_83;
    memcpy(tmpchararr_82, "Linux\n", 7);
     tmprc_63 = tmpchararr_82;
    INCR_RC(tmprc_63); /* static top-incr type: (Array Char) */
    INCR_RC(tmprc_63); /* heap type: (Array Char) */
     tmpconstlift_30 = tmprc_63;
  } 
  {
    char* tmprc_64 ;
    int* arrtmp_81 = (int*)0;
    if (2 > 0) {
      arrtmp_81 = (int*)((char*)malloc((sizeof(char) * 2) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      CLEAR_RC(arrtmp_81);
      SETARRLEN(arrtmp_81, 2);
    } 
    char* tmpchararr_80 = (char*)arrtmp_81;
    memcpy(tmpchararr_80, "\n", 2);
     tmprc_64 = tmpchararr_80;
    INCR_RC(tmprc_64); /* static top-incr type: (Array Char) */
    INCR_RC(tmprc_64); /* heap type: (Array Char) */
     tmpconstlift_29 = tmprc_64;
  } 
  {
    char* tmprc_65 ;
    int* arrtmp_79 = (int*)0;
    if (21 > 0) {
      arrtmp_79 = (int*)((char*)malloc((sizeof(char) * 21) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      CLEAR_RC(arrtmp_79);
      SETARRLEN(arrtmp_79, 21);
    } 
    char* tmpchararr_78 = (char*)arrtmp_79;
    memcpy(tmpchararr_78, "  malloc'd pointer: ", 21);
     tmprc_65 = tmpchararr_78;
    INCR_RC(tmprc_65); /* static top-incr type: (Array Char) */
    INCR_RC(tmprc_65); /* heap type: (Array Char) */
     tmpconstlift_28 = tmprc_65;
  } 
  {
    char* tmprc_66 ;
    int* arrtmp_77 = (int*)0;
    if (2 > 0) {
      arrtmp_77 = (int*)((char*)malloc((sizeof(char) * 2) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      CLEAR_RC(arrtmp_77);
      SETARRLEN(arrtmp_77, 2);
    } 
    char* tmpchararr_76 = (char*)arrtmp_77;
    memcpy(tmpchararr_76, "\n", 2);
     tmprc_66 = tmpchararr_76;
    INCR_RC(tmprc_66); /* static top-incr type: (Array Char) */
    INCR_RC(tmprc_66); /* heap type: (Array Char) */
     tmpconstlift_27 = tmprc_66;
  } 
  {
    char* tmprc_67 ;
    int* arrtmp_75 = (int*)0;
    if (21 > 0) {
      arrtmp_75 = (int*)((char*)malloc((sizeof(char) * 21) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      CLEAR_RC(arrtmp_75);
      SETARRLEN(arrtmp_75, 21);
    } 
    char* tmpchararr_74 = (char*)arrtmp_75;
    memcpy(tmpchararr_74, "  malloc'd pointer: ", 21);
     tmprc_67 = tmpchararr_74;
    INCR_RC(tmprc_67); /* static top-incr type: (Array Char) */
    INCR_RC(tmprc_67); /* heap type: (Array Char) */
     tmpconstlift_26 = tmprc_67;
  } 
  {
    char* tmprc_68 ;
    int* arrtmp_73 = (int*)0;
    if (2 > 0) {
      arrtmp_73 = (int*)((char*)malloc((sizeof(char) * 2) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      CLEAR_RC(arrtmp_73);
      SETARRLEN(arrtmp_73, 2);
    } 
    char* tmpchararr_72 = (char*)arrtmp_73;
    memcpy(tmpchararr_72, "\n", 2);
     tmprc_68 = tmpchararr_72;
    INCR_RC(tmprc_68); /* static top-incr type: (Array Char) */
    INCR_RC(tmprc_68); /* heap type: (Array Char) */
     tmpconstlift_25 = tmprc_68;
  } 
  {
    char* tmprc_69 ;
    int* arrtmp_71 = (int*)0;
    if (22 > 0) {
      arrtmp_71 = (int*)((char*)malloc((sizeof(char) * 22) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      CLEAR_RC(arrtmp_71);
      SETARRLEN(arrtmp_71, 22);
    } 
    char* tmpchararr_70 = (char*)arrtmp_71;
    memcpy(tmpchararr_70, "  successfully freed\n", 22);
     tmprc_69 = tmpchararr_70;
    INCR_RC(tmprc_69); /* static top-incr type: (Array Char) */
    INCR_RC(tmprc_69); /* heap type: (Array Char) */
     tmpconstlift_24 = tmprc_69;
  } 
  {
    char tmprc_56 ;
     tmprc_56 = 0;
     free_3 = tmprc_56;
  } 
  {
    char tmprc_57 ;
     tmprc_57 = 0;
     malloc_4 = tmprc_57;
  } 
} 

int main(int argc, char** argv) {
  parseOptions(argc,argv);
  int counter_anonstreamop_1 = 0;
  initState();
  char dummy =TRUE;
  while(dummy) {
    counter_anonstreamop_1++;
    if (counter_anonstreamop_1 == 1) {
      main_2(((char)0)) /* emit */;
      counter_anonstreamop_1 = 0;
    } 
  } 
  return 0;
} 
