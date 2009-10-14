#include "Thread.h"
#include <math.h>

class ThreadTest : public Thread {
  double sum;

public:
  ThreadTest(const char* name) : Thread(name), sum(0){}
  ~ThreadTest() {};
  
  void run() {
    for(int i=0; i<10000; i++)
      sum += sin(i) * cos(i);
    
    printf("Thread %s with ID %d has finished, sum is %e......\n", 
	   getName(), getID(), sum);

 }

  
  
};

int main() {
  ThreadTest t1 = ThreadTest("t1");
  ThreadTest t2 = ThreadTest("t2");


  t1.start();
  t2.start();
  t1.join(); t2.join();

  printf("the main program has finished.....\n");

  return 0;
}
