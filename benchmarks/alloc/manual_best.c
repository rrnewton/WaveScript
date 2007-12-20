#include<stdio.h>
#include<stdlib.h>
#include "/home/newton/wavescript/src/linked_lib/wsc2.h"


/*

gcc -O3 -DREPLACE_SYSTEM_ALLOCATOR nedmalloc.c manual_best.c


*/

void* ZCT[10000];
int ZCT_count;

static inline void free_Array_Array_Int(int** ptr) {
  int i_95;
  for (i_95 = 0; i_95 < ((int*)ptr)[-2]; i_95++) {
    free((int*)ptr[i_95] - 2);    
  }
  free((int*)ptr - 2);
}

int size_9;
int printevery_6;
int size_5;
int count_4;

void finalstrm_57(int x_58) {
  printf("%d", x_58);
  printf("%s", "\n");
  BASE(((char)0));
} 
void anonstreamop_1(int** arr_8) {
  int tmp_87 = (count_4 + (int)1);
  count_4 = tmp_87;
  char tmp_75 = (count_4 == printevery_6);
  char tmp;
  if (tmp_75) {
    count_4 = (int)0;
    int* tmp_79 = arr_8[2];
    int tmp_83 = tmp_79[2];
    finalstrm_57(tmp_83);
    char tmp_85 = ((char)0);
    tmp = tmp_85;
  } else {
    tmp = ((char)0);
  }
} 
void source_2(char __11) {
  int* arrtmp_97 = ((int*)calloc(sizeof(int*) * size_9 + 2*sizeof(int), 1) + 2);
  arrtmp_97[-1] = 0;
  arrtmp_97[-2] = size_9;
  int** arr_12 = (int**)arrtmp_97;

  int tmp_65 = (size_9 - (int)1);
  int i_14;
  for (i_14 = (int)0; i_14 <= tmp_65; i_14++) {
    int* arrtmp_96 = ((int*)calloc(sizeof(int) * size_9 + 2*sizeof(int), 1) + 2);
    arrtmp_96[-1] = 0;
    arrtmp_96[-2] = size_9;
    int* tmp_67 = (int*)arrtmp_96;
    arr_12[i_14] = tmp_67;
    char tmp_69 = ((char)0);
  }
  int* arrinner_13 = arr_12[2];
  arrinner_13[2] = (int)39;
  anonstreamop_1(arr_12);
  free_Array_Array_Int(arr_12);
}

void anonstreamop_3() {
  source_2(((char)0));
} 

void initState() {

  char* str = getenv("SCALEFACTOR");
  if (0 == str) {
    printf("Environment var SCALEFACTOR was not bound.\n");
    exit(-1);
  }
  int scalefactor = atoi(str);
  //printf("GOT SCALE: %d\n", scalefactor);

  size_9 = (int)1 * scalefactor;

  // 100 Million work units.
  printevery_6 = (int)100 * 1000 * 1000 / (scalefactor * scalefactor);

  size_5 = (int)size_9;
  int tmp_73 = (int)0;
  count_4 = tmp_73;

} 

int main() {
  initState();
  while(1) {
    anonstreamop_3();
  } 
  return 0;
} 
