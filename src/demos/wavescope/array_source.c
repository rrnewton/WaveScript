
#include <stdio.h>

void wsentry1(int*, int);

void wsmain() {
  int counter = 5;
  while (counter > 0) {
    int arr[5] = {counter, counter+1, counter+2, counter+3, counter+4};
    usleep(1000 * 1000);
    printf("Calling entry point with array.\n");    
    wsentry1(arr, 5);

    counter--;
  }
  printf("Done calling into WS, exiting\n");
}

//int main() { wsmain(); }
