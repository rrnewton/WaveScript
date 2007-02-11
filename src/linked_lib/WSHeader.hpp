
// These are the headers & #defines that occur at the beginning of all
// wavescript-generated header files.
#include <WaveScope.h>
#include <Heartbeat.hpp>
#include <PrintBox.hpp>
#include <RawFileSource.hpp>
#include <AsciiFileSink.hpp>
#include <Boxes.hpp>

/* for boost smart pointers */
#include <boost/shared_ptr.hpp>
#include <boost/enable_shared_from_this.hpp>

#include <boost/functional/hash.hpp>

#include <stdio.h>
#include <list>
#include <vector>
#include <string>
#include <iostream>
#include <ext/hash_map>


#include <fftw3.h>
#include <complex>
//#include <time.h>

using boost::enable_shared_from_this;
using namespace std;
using namespace __gnu_cxx;

#define TRUE 1
#define FALSE 0

#define WSNULL 0
#define WSNULLSEG (RawSeg::NullRef)

typedef int wsint_t;
//typedef double wsfloat_t;
typedef float wsfloat_t; 
typedef bool wsbool_t;
typedef string wsstring_t;
//typedef fftw_complex wscomplex_t;
//typedef _Complex double wscomplex_t;
typedef _Complex float wscomplex_t;

#define WS_DEFINE_OUTPUT_TYPE(type)                \
  inline void emit(const type &tuple) {         \
    uint i;                                     \
    for (i=0; i<m_outputs.size(); i++) {        \
      m_outputs[i]->enqueue(new type (tuple), this);  \
    } \
    totalEmits++; \
  }

/*
#define WS_DEFINE_OUTPUT_TYPE(type)                \
  inline void emit(const type &tuple) {         \
    uint i;                                     \
    for (i=0; i<m_outputs.size(); i++) {        \
      m_outputs[i]->enqueue(new type (tuple), this);  \
    } \
    totalEmits++; \
  } \
  void freeTuple(void *tuple) { \
  type *tuplePtr = (type *)tuple; \
  delete tuplePtr; \
  } 
*/





/********** LISTS **********/
//template <class T>
//int foo();

template <class T> class cons; 

template <class T> 
  bool wsequal (const boost::shared_ptr< cons<T> > & v1, 
		const boost::shared_ptr< cons<T> > & v2);

template <class T>
class cons {
public: 
  typedef boost::shared_ptr< cons<T> > ptr;
  cons(T a, ptr b);
  T car;
  ptr cdr;
  //static ptr null_ls;
  static int testtest;
  static ptr append(const ptr& x, const ptr &y);

  //friend int goo<T> (int x) { return x;  }
  //friend int foo();

  friend bool wsequal<T> (const boost::shared_ptr< cons<T> > & x, const boost::shared_ptr< cons<T> > & y);
  //friend bool operator ==<T> (const ptr & x, const ptr &y);
  //static bool operator ==<T> (const ptr & x, const ptr &y);
  //bool operator == (const ptr &y);
};


// template<class S> g();
// template<class T> class A {
//    friend int e();
//    friend int g<T>();
// };

// template <class T>
// friend void goo<T> (int x) {
//   return;
// }

//template class cons <int>;
template<typename T>
int cons<T>::testtest = 39;

//template<typename T>
//const boost::shared_ptr< cons<T> > cons<T>::null_ls;

//template<typename T>
//boost::shared_ptr< cons<T> > NULL_LIST = boost::shared_ptr< cons<T> >((cons<T>*)0);

// template<typename T>
// boost::shared_ptr< cons<T> >  MAKE_NULL_LIST() 
// { 
//   return boost::shared_ptr< cons<T> >((cons<T>*)0);
// }

template<typename T>
bool IS_NULL(boost::shared_ptr< cons<T> > p) 
{ 
  return ((cons<T>*)0 == p.get());
  //return (0 == p.get());
}



// We construct a single null object which we cast to what we need.
//cons<int>::ptr NULL_LIST = cons<int>::ptr((cons<int>*)0);
//boost::shared_ptr< cons<int> > NULL_LIST = boost::shared_ptr< cons<int> >((cons<int>*)0);

template <class T>
cons<T>::cons(T a, boost::shared_ptr< cons<T> > b)
{
    car = a;
    cdr = b;
}

bool wsequal(wsint_t x,     wsint_t y)     { return x==y; }
bool wsequal(wsbool_t x,    wsbool_t y)    { return x==y; }
bool wsequal(wsfloat_t x,   wsfloat_t y)   { return x==y; }
bool wsequal(wscomplex_t x, wscomplex_t y) { return x==y; }
bool wsequal(wsstring_t x,  wsstring_t y)  { return x==y; }
//bool wsequal(int& x, int& y)     { return x==y; }

// Recursive for now... should change.
template <class T>
bool wsequal
        (const boost::shared_ptr< cons<T> > & x, 
	 const boost::shared_ptr< cons<T> > & y) 
{
  printf("EQUALITY: \n");
  //printf("   %d %d \n", (x==null_ls), (y==null_ls));
   if (IS_NULL(x)) {
     if (IS_NULL(y))
       return TRUE;
     else return FALSE;
   } else if (IS_NULL(y))
     return FALSE;
   else 
     if (wsequal(x->car, y->car))
       return wsequal(x->cdr, y->cdr);
     else return FALSE;

//    if (x == cons<T>::null_ls) {
//      if (y == cons<T>::null_ls)
//        return TRUE;
//      else return FALSE;
//    } else if (y == cons<T>::null_ls)
//      return FALSE;
//    else 
//      if (wsequal(x->car, y->car))
//        return wsequal(x->cdr, y->cdr);
//      else return FALSE;
}

// Not tail recursive!
template <class T>
//cons<T>::ptr cons<T>::append(const cons<T>::ptr & x, const cons<T>::ptr & y) {
boost::shared_ptr< cons<T> > cons<T>::append(
       const boost::shared_ptr< cons<T> > & x, 
       const boost::shared_ptr< cons<T> > & y) 
{
  //printf("INAPPEND: %d %d\n", x->car, y->car);
  //printf("  x REFLEXIVE: %d NULL?: %d \n", wsequal(x, x), wsequal(x, null_ls));

  //if (x == cons<T>::null_ls)   return y;
  if (IS_NULL(x))   return y;
  else return ptr(new cons<T>(x->car, append(x->cdr, y)));

  //else return ptr(new cons<T>(33, append(null_ls, y)));
  //printf("FIN APPEND: %d %d\n", x->car, y->car);
}

//   friend ostream& operator<<(ostream& output, const ptr& p) {
//     while(p != null_ls) {
//     }
//   }

/******** END LISTS ********/

// Global setting:
bool WSOUTPUT_PREFIX = TRUE;

//cons<bool>::ptr NULL_LIST2 = (cons<bool>::ptr)NULL_LIST;



