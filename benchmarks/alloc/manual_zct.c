#include<stdio.h>
#include<stdlib.h>
#include "/home/newton/wavescript/src/linked_lib/wsc2.h"

void* ZCT[10000];
int ZCT_count = 0;

void free_Array_Array_Int(int** ptr) {
  int i_95;
  for (i_95 = 0; i_95 < ((int*)ptr)[-2]; i_95++) {
    if (ptr[i_95] && --(((int*)ptr[i_95])[-1]) == 0) /* decr refcount (Array Int) */  {
      //free((int*)ptr[i_95] - 2);
      ZCT[ZCT_count] = (void*)(ptr[i_95]);
      ZCT_count++;
    }
  }
  //free((int*)ptr - 2);
  //ZCT[ZCT_count] = (void*)(((int*)ptr) - 2);
  ZCT[ZCT_count] = (void*)ptr;
  ZCT_count++;
}

// NEED TO KNOW TYPES!
void freeitUP() {
  int i,j;
  //printf("  AT END OF operator: resident in zct %d, refcounts: ", ZCT_count);
  //for (i=0; i<ZCT_count; i++)  printf("%d ", ((int*)(ZCT[i]))[-1]);  printf("\n");
  for (i=0; i<ZCT_count; i++) {
    // Assumes the array is nested and thus has int-sized (pointer) contents:
    int* thisarr = (int*)(ZCT[i]);
    if (thisarr && 0 == thisarr[-1]) {
      // Traverse the array and decrement everybody downstream.
      for (j=0; j < thisarr[-2]; j++) {
        int* inner = (int*)thisarr[j];	
	inner[-1]--;
        if (0 == inner[-1]) {
	  //printf("    FREEING inner: %p\n", inner-2);
	  // NOTE: TODO - would really have to recursively go deeper here.
	  free(inner-2);
	}
      }
    }
    //printf("  FREEING OUTER: %p\n", thisarr-2);
    free(thisarr-2);
  }
  ZCT_count = 0;
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
    int tmp_77 = 2; //(size_5 / (int)2);
    int* tmp_79 = arr_8[tmp_77];
    int tmp_81 = 2; //(size_5 / (int)2);
    int tmp_83 = tmp_79[tmp_81];
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

  // Tentatively must add it to the ZCT:
  ZCT[ZCT_count] = (void*)arr_12;
  ZCT_count++;

  int tmp_65 = (size_9 - (int)1);
  int i_14;
  for (i_14 = (int)0; i_14 <= tmp_65; i_14++) {
    int* arrtmp_96 = ((int*)calloc(sizeof(int) * size_9 + 2*sizeof(int), 1) + 2);
    arrtmp_96[-1] = 0;
    arrtmp_96[-2] = size_9;
    int* tmp_67 = (int*)arrtmp_96;

    // Tentatively must add it to the ZCT:
    //ZCT[ZCT_count] = (void*)arrtmp_96;
    //ZCT_count++;

    /* I AM ARRAY:SET DECR/INCR */
    if (arr_12[i_14] && --(((int*)arr_12[i_14])[-1]) == 0) /* decr refcount (Array Int) */  {
      printf("ERK freeing inner\n");
      //free((int*)arr_12[i_14] - 2);
      
    } 
    arr_12[i_14] = tmp_67;
    if (arr_12[i_14]) ((int*)arr_12[i_14])[-1]++; /* incr refcount (Array Int) */
    char tmp_69 = ((char)0);
  } 
  int tmp_61 = 2; //(size_9 / (int)2);
  int* arrinner_13 = arr_12[tmp_61];
  int tmp_63 = 2; //(size_9 / (int)2);
  /* I AM ARRAY:SET DECR/INCR */
  arrinner_13[tmp_63] = (int)39;
  anonstreamop_1(arr_12);
  freeitUP();
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

  //printevery_6 = (int)20 * 1000 * 1000 / (scalefactor * scalefactor);

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
