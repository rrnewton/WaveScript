
#include <stdio.h>

void wsentry1(int);
void wsentry2(float);

void wsmain() {
  int counter = 5;
  while (counter > 0) {
    usleep(1000 * 1000);
    printf("Calling entry point 1.\n");
    usleep(500 * 1000);
    printf("Calling entry point 2.\n");
    counter--;
  }
  printf("Done calling into WS, exiting\n");
}

int main() {
  wsmain();
}
