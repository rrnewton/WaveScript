#include "Client.h"

int main() {
  Client c(2020, "127.30.76.32", "~~~~~~Connecting to the Server~~~~~");
  //s.start();
  //s.join();
  //s.run();
  c.run();

  double sum = 0;
  for(int i=0; i< 100; i++) {

    sum += i*(i+10);
    usleep(10);
    printf("i = %d \n" , i);
  }

  printf("main finish\n");
  return 0;
}
