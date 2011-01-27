
#include "lffifo.h"
#include <stdlib.h>
#include <stdio.h>

/*
 void 	       	fifoinit(fifo* ff);
 unsigned long 	fifosize (fifo * ff);
 void 	       	fifoput (fifo * ff, fifocell * cl);
 fifocell * 	fifoget (fifo * ff);
 fifocell * 	fifoavail (fifo * ff); 
 fifocell * 	fifoflush (fifo * ff);
*/

int main() {

  fifo* ff = malloc(sizeof(fifo));

  fifocell* cell = malloc(sizeof(fifocell));

  fifoinit(ff);
  printf("initialized...\n");

  fifocell* pregot = fifoget(ff);
  printf("get... %p\n", pregot);
  pregot = fifoget(ff);
  printf("get... %p\n", pregot);
  pregot = fifoget(ff);
  printf("get... %p\n", pregot);

  fifoput(ff, cell);
  printf("put... %p\n", cell);

  fifocell* got = fifoget(ff);
  printf("get... %p\n", got);

}
