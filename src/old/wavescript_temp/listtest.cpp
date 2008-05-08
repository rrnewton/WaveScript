

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

template <class T> class cons;

//template <class T>
//typedef typename boost::shared_ptr< cons<T> > ConsPtr;

template <class T>
class cons {
public: 
  typedef boost::shared_ptr< cons<T> > ptr;

  cons() {
  }

  cons(T a, ptr b) {
    car = a;
    cdr = b;
  }

  T car;
  ptr cdr;

  //  static ptr null;
  //    = ptr((cons<T>*)0);
};


cons<int>::ptr NULL_LIST = cons<int>::ptr((cons<int>*)0);

cons<float>::ptr NULL_LIST2 = (cons<float>::ptr)NULL_LIST2;

int main(int argc, char** argv) {

  FooPtr fp(new Foo());

  fp->x = 3;

  printf("Test %d\n", fp->x);  
  printf("Pointer %p\n", fp.get()); 

  //   cons<int>* test = new cons<int>;

   cons<int>::ptr test2;
   test2 = cons<int>::ptr(new cons<int>);

   cons<int>::ptr cell1(new cons<int>);
   cell1->car = 998;

   cons<int>::ptr cell2(new cons<int>);
   cell2->car = 333;

   cell1->cdr = cell2;

  //  fp = FooPtr();
   //int i;
   //int x = {i=64; i};
   

   printf("Got a list %p\n", cell1.get());  
   printf("List head: %d\n", cell1->car);  
   printf("List tail: %p\n", cell1->cdr.get());  
   printf("List second elt: %d\n", cell1->cdr->car);  
   printf("List tail tail: %p\n", cell1->cdr->cdr.get());  
  
   cell1->cdr = cons<int>::ptr((cons<int>*)0);

   printf("(Rewired) List tail: %p\n", cell1->cdr.get());  

}
