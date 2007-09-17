
#include <stdio.h>

void wsentry1(int);
void wsentry2(int, float);


void wserror(const char* p) {
  printf("GOT WSERROR: %s\n", p);
}

void wsmain(int argc, char* argv[]) {
  int counter = 5;

  printf("Inside wsmain, %d command line args, argv pointer: %p\n", argc, argv);
  for (int i = 0; i<argc; i++) {   printf("  Arg %d: %s\n", i, argv[i]);  }

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

//void wsinit() {}
/*
void wsinit(int argc, char* argv[]) {
  printf("Inside wsinit, %d command line args, argv pointer: %p\n", argc, argv);
  for (int i = 0; i<argc; i++) {
    printf("  Arg %d: %s\n", argv[i]);
  }
}
*/

void wsinit(int argc, char* argv[]) {
  printf("Inside wsinit, %d command line args, argv pointer: %p\n", argc, argv);
  for (int i = 0; i<argc; i++) {
    printf("  Arg %d: %s\n", i, argv[i]);
  }
}

//int main() { wsmain(); }
