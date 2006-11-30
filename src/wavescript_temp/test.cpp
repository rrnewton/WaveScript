

#include "stdio.h"
#include <boost/shared_ptr.hpp>
#include <boost/enable_shared_from_this.hpp>

using boost::enable_shared_from_this;
using namespace std;

class Foo { //: public enable_shared_from_this< RawSignal > {
public:
  int x;
};

typedef boost::shared_ptr<Foo> FooPtr;

struct cons;

typedef boost::shared_ptr<cons> ConsPtr;

struct cons {
  int car;
  ConsPtr cdr;
  //boost::shared_ptr<cons> cdr;
};



int main(int argc, char** argv) {

  FooPtr fp(new Foo());

  fp->x = 3;

  printf("Test %d\n", fp->x);  
  printf("Pointer %p\n", fp.get()); 

  cons* test = new cons;

  ConsPtr ls(new cons);
  ls->car = 998;

  //  fp = FooPtr();

  printf("Got a list %p\n", ls.get());  
  printf("List head: %d\n", ls->car);  
  
}
