
// Faith can execute 7.4 million ticks per second:

#include <stdlib.h>
#include <stdio.h>
#include <time.h>

#define drand() (rand() / ((double)RAND_MAX + 1))

typedef struct {
  int sym;
  double t;
  int vol;
  double price;
} tuple;

//hash_map<int,double> table;
double table[5000];

// Generate them.
void generate(tuple* buf) {
  buf->sym = rand() % 5000;
  if (0 == (rand() % 1000)) {
    // A Split
    buf->vol = -1;
    buf->price = 2.0 * drand();
  } else {
    // A Tick
    buf->vol = rand() % 100;
    buf->price = 300.0 * drand();
  }
}

// Process them.
void process(tuple* in) {
  if (in->vol == -1) {
    // Price field is really a factor:
    table[in->sym] *= in->price;
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
