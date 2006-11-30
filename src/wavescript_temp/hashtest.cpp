

#include "stdio.h"
#include <iostream>
#include <boost/shared_ptr.hpp>
#include <boost/enable_shared_from_this.hpp>

#include <ext/hash_map>

using boost::enable_shared_from_this;
using namespace std;
using namespace __gnu_cxx;

struct eqstr
{
  bool operator()(const char* s1, const char* s2) const
  {
    return strcmp(s1, s2) == 0;
  }
};


struct test
{
  int x,y;
};

//template <class Tk, cl>
// class cons {
// public: 
//   typedef boost::shared_ptr< hash_map<const char*, int> > ptr;
//   cons(T a, ptr b) {
//     car = a;
//     cdr = b;
//   }
//   ptr cdr;

//   //  static ptr null;  //    = ptr((cons<T>*)0);
// };


typedef boost::shared_ptr< hash_map<const char*, int> > hashptr;

int main()
{
  hash_map<const char*, int, hash<const char*>, eqstr> months;
  
  months["january"] = 31;
  months["february"] = 28;
  months["march"] = 31;
  months["april"] = 30;
  months["may"] = 31;
  months["june"] = 30;
  months["july"] = 31;
  months["august"] = 31;
  months["september"] = 30;
  months["october"] = 31;
  months["november"] = 30;
  months["december"] = 31;
  
  cout << "september -> " << months["september"] << endl;
  cout << "april     -> " << months["april"] << endl;
  cout << "june      -> " << months["june"] << endl;
  cout << "november  -> " << months["november"] << endl;

  //  hashptr boosted = hashptr(new hash_map<const char*, int>() );

  hashptr boosted(new hash_map<const char*, int>() );

  (*boosted)["foo"] = 399;
  (*boosted)["bar"] = 3988;

  cout << "lookup  -> " << (*boosted)["foo"] << endl;
  cout << "lookup2  -> " << (*boosted)["baz"] << endl;
  cout << "lookup3  -> " << (*boosted)["bar"] << endl;

  boost::shared_ptr< hash_map<test, int> > 
    foo(new hash_map<test, int> ); 
  
  test s;  
  s.x = 99;
  s.y = 98;

  (*boosted)[(const char*)&s] = 12345;

  //  (*foo)[s] = 12345;

  cout << "dangerous I think: " << (*boosted)[(const char*)&s] << endl;

  s.y += 100;
  cout << "Now increment y field: (doesn't affect)" << (*boosted)[(const char*)&s] << endl;
  s.y -= 100;

  s.x++;
  cout << "Now increment x field: " << (*boosted)[(const char*)&s] << endl;
  s.x--;
  cout << "Now decrement again: " << (*boosted)[(const char*)&s] << endl;

}



// int main(int argc, char** argv) {
// }
