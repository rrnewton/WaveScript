
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
// forward declaration of function template operator==

template <class T> 
  bool operator== (const cons<T> & v1, 
                   const cons<T> & v2);

template <class T>
class cons {
public: 
  typedef boost::shared_ptr< cons<T> > ptr;
  cons(T a, ptr b);
  T car;
  ptr cdr;
  static ptr null;
  static ptr append(const ptr& x, const ptr &y);

  //friend int goo<T> (int x) { return x;  }
  //friend int foo();

  friend bool operator ==<T> (const cons<T> & x, const cons<T> & y);
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



// We construct a single null object which we cast to what we need.
cons<int>::ptr NULL_LIST = cons<int>::ptr((cons<int>*)0);
//boost::shared_ptr< cons<int> > NULL_LIST = boost::shared_ptr< cons<int> >((cons<int>*)0);

template <class T>
cons<T>::cons(T a, boost::shared_ptr< cons<T> > b)
{
    car = a;
    cdr = b;
}

// THIS WON'T WORK! Can't access null...
// Recursive for now... should change.
// template <class T>
// operator bool cons<T>::==(const boost::shared_ptr< cons<T> > & y)
// {
//   if (self == NULL_LIST) {
//     if (y == NULL_LIST) 
//       return TRUE;
//     else return FALSE;
//   } else if (y == NULL_LIST) 
//     return FALSE;
//   else 
//     if (car == y->car)
//       return cdr == y->cdr;
//     else return FALSE;
// }

// THIS WON'T WORK! Can't access null...
// Recursive for now... should change.
// template <class T>
// friend operator
//   bool cons<T>::==<>
//         (const boost::shared_ptr< cons<T> > & x, 
// 	 const boost::shared_ptr< cons<T> > & y) 
// {
//   if (x == NULL_LIST) {
//     if (y == NULL_LIST) 
//       return TRUE;
//     else return FALSE;
//   } else if (y == NULL_LIST) 
//     return FALSE;
//   else 
//     if (x->car == y->car)
//       return x->cdr == y->cdr;
//     else return FALSE;
// }

template <class T>
bool operator== (const cons<T> & x, const cons<T> & y)
{
  if (x == NULL_LIST) {
    if (y == NULL_LIST) 
      return TRUE;
    else return FALSE;
  } else if (y == NULL_LIST) 
    return FALSE;
  else 
    if (x.car == y.car)
      return x.cdr == y.cdr;
    else return FALSE;
}


// Not tail recursive!
template <class T>
//cons<T>::ptr cons<T>::append(const cons<T>::ptr & x, const cons<T>::ptr & y) {
boost::shared_ptr< cons<T> > cons<T>::append(
       const boost::shared_ptr< cons<T> > & x, 
       const boost::shared_ptr< cons<T> > & y) {
    if (x == NULL_LIST)
      return y;
    else 
      return ptr(new cons<T>(x->car, 
			     append(x->cdr, y)));
}

//   friend ostream& operator<<(ostream& output, const ptr& p) {
//     while(p != NULL_LIST) {
//     }
//   }

/******** END LISTS ********/

// Global setting:
bool WSOUTPUT_PREFIX = TRUE;


bool FOOBAR = NULL_LIST == NULL_LIST;
