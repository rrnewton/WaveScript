

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

typedef const struct test* thistest;

struct test
{
  int x,y;
};
struct hashtest {
  size_t operator()(thistest tup) const 
  {
    return myhash((unsigned char*)tup, sizeof(struct test));   
    //return 33;
  }
};
struct eqtest {
  bool operator()(thistest tup1, thistest tup2) {
    return (tup1->x == tup2->x && 
	    tup1->y == tup2->y );
    //return 0;
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


  boost::hash<int> inthash;
  boost::hash<struct test> testhash;
  boost::hash< boost::tuple<int,char> > tuphash;
  
  //hash_map<tuple<int,char>, int, hash<tuple<int,char> >, equal_to<tuple<int,char> > > manual;
  //hash_map< boost::tuple<int,char>, int> tuplemap;
  boost::tuple<int,char> mytup(1,'a');
  cout << "Here's a tup: " << mytup << endl;
  //tuplemap[mytup] = 39;

  // This doesn't work.  Big template related error.
  //  (*foo)[s] = 12345;
  printf("\nInt hash: %d\n", inthash(35));
  //printf("\ntest hash: %d\n", testhash(s));  // FAILS
  //printf("\nTup hash: %d\n", tuphash(mytup));  // FAILS


  // Ok, now doing manually:
  //hash_map<struct test, int, hashtest, eqtest > manual;
  //hash_map<test, int, hashtest, equal_to<test> > manual;
  hash_map<thistest, int, hashtest, eqtest > manual;
  manual[&s] = 3295;
  cout << "Victory: " << manual[&s] << endl;

   s.x++;
   cout << "This should miss:: " << manual[&s] << endl;
   s.x--;
   s.y++;
   cout << "This should miss:: " << manual[&s] << endl;
   
   //   int status = scanf("%d %d", &(s.x), &(s.y));
   //scanf("%d", &(s.x));
   //scanf("%d", &(s.y));
   //   printf("SCANNED: %d, %d\n", s.x, s.y);

   printf("Testing string-indexed hashes.\n");
   string str = "foob";
   boost::hash< string > strhash;
   //hash_map<thistest, int, hashtest, eqtest > str;
   printf("Hashed string: %d\n", strhash(str));
}



// int main(int argc, char** argv) {
// }
