#include "EventStream.h"
#include "Thread.h"
#include "ESBox.h"
#include "Server.h"


int main() {
  Catalog c;
  Server s(2020, "piquin", &c);
  //s.start();
 
  s.start();
  pthread_exit(NULL);

}
