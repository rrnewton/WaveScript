
#include <stdio.h>

void wsentry1(int);
void wsentry2(int, float);

void wsmain() {
  int counter = 5;
  while (counter > 0) {
    usleep(1000 * 1000);
    printf("Calling entry point 1.\n");
     wsentry1(counter);
    usleep(500 * 1000);
    printf("Calling entry point 2.\n");
     wsentry2(99, (float)counter + 0.5);
    counter--;
  }
  printf("Done calling into WS, exiting\n");
}

//int main() { wsmain(); }
