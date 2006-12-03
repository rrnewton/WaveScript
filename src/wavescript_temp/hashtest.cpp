

#include "stdio.h"
#include <iostream>
#include <boost/shared_ptr.hpp>
#include <boost/enable_shared_from_this.hpp>
//#include <boost/hash/hash.hpp>
#include <boost/functional/hash.hpp>
//#include <boost/hash.hpp>

//#include <boost/tuple/tuple.hpp>
#include "boost/tuple/tuple_comparison.hpp"
#include "boost/tuple/tuple_io.hpp"

#include <ext/hash_map>

using boost::enable_shared_from_this;
//using boost::tuples;
//using boost::hash;
using namespace std;
using namespace __gnu_cxx;

struct eqstr
{
  bool operator()(const char* s1, const char* s2) const
  {
    return strcmp(s1, s2) == 0;
  }
};

static size_t myhash(unsigned char* ptr, int size) {
  size_t hash = 5381;
  int c;
  for(int i=0; i<size; i++) 
    hash = ((hash << 5) + hash) + ptr[i]; /* hash * 33 + c */	 	 
  return hash;
}

struct test
{
  int x,y;
};
struct hashtest {
  size_t operator()(struct test tup) {
    //return myhash((unsigned char*)&tup, sizeof(struct test));
    return 0;
  }
};
struct eqtest {
  bool operator()(struct test tup1, struct test tup2) {
    //myhash((unsigned char*)&tup, sizeof(struct test));
    return 1;
  }
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
  hash_map<const char*, int, hash<const char*> > months;
  
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

  boost::shared_ptr< hash_map<struct test, int> > 
    foo(new hash_map<struct test, int> ); 
  
  test s;  
  s.x = 99;
  s.y = 98;

  (*boosted)[(const char*)&s] = 12345;

  cout << "\n Testing const char* casting... \n";
  cout << "dangerous I think: " << (*boosted)[(const char*)&s] << endl;

  s.y += 100;
  cout << "Now increment y field: (doesn't affect) " << (*boosted)[(const char*)&s] << endl;
  s.y -= 100;

  s.x++;
  cout << "Now increment x field: " << (*boosted)[(const char*)&s] << endl;
  s.x--;
  cout << "Now decrement again: " << (*boosted)[(const char*)&s] << endl;


  boost::shared_ptr< hash_map<struct test, int> > 
    foo2(new hash_map<struct test, int> ); 

  boost::hash<int> inthash;
  boost::hash<struct test> testhash;
  boost::hash< boost::tuple<int,char> > tuphash;
  
  boost::tuple<int,char> mytup(1,'a');

  cout << "Here's a tup: " << mytup << endl;

  // This doesn't work.  Big template related error.
  //  (*foo)[s] = 12345;
  printf("\nInt hash: %d\n", inthash(35));
  //printf("\ntest hash: %d\n", testhash(s));  // FAILS
  //printf("\nTup hash: %d\n", tuphash(mytup));  // FAILS


  // Ok, now doing manually:
  //hash_map<struct test, int, hashtest, eqtest > manual;
  hash_map<struct test, int, hashtest, eqtest > manual;
  manual[s] = 3295;

  //hash_map<int, int, hash<int>, equal_to<int> > manual;
  //manual[s.x] = 3295;
  
}



// int main(int argc, char** argv) {
// }
