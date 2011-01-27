// This one doesn't do random generation.
// Takes only 1.46 seconds.
// 34 million ticks per second.

// If we de-randomize the splits also, that gets us down to 1.14s (43 mtick/s)
// But then we can't do O3!! because it figures out that it doesn't
// have to do any work!

#include <stdlib.h>
#include <stdio.h>
#include <time.h>

typedef struct {
  int sym;
  double t;
  int vol;
  double price;
} tuple;

//hash_map<int,double> table;
double table[5000];

int counter = 0;

// Generate them.
void generate(tuple* buf) {
  buf->sym = 3539;
  if (counter == 1000) {
    // A Split
    buf->vol = -1;
    buf->price = 1.5;
    counter = 0;
  } else {
    // A Tick
    buf->vol = 153;
    buf->price = 153.0;
  }
  counter++;
}

// Process them.
void process(tuple* in) {
  if (in->vol == -1) {
    // Price field is really a factor:
    table[in->sym] *= in->price;
    //    printf("Split: %f\n", in->price);
  } else {
    // Output is stored in the same tuple:
    in->price *= table[in->sym];
  }
}


int main() {
  int i;
  clock_t st, en;
  double time_used;
  srand((unsigned int)time(NULL));
  tuple themem;
  printf("Starting to process 50 million ticks (100*5000k).\n");
  st = clock();
  for(i=0; i<50000000; i++) {
    generate(&themem);
    process(&themem);    
  }
  en = clock();
  time_used = ((double) (en - st)) / CLOCKS_PER_SEC;
  printf("Finished, elapsed: %f.\n", time_used);
}
