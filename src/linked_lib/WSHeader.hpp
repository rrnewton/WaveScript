
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
#define WSNULLTIMEBASE 0

typedef int16_t wsint16_t;
typedef uint16_t wsuint16_t;

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



bool wsequal(wsint_t x,     wsint_t y)     { return x==y; }
bool wsequal(wsbool_t x,    wsbool_t y)    { return x==y; }
bool wsequal(wsfloat_t x,   wsfloat_t y)   { return x==y; }
bool wsequal(wscomplex_t x, wscomplex_t y) { return x==y; }
bool wsequal(wsstring_t x,  wsstring_t y)  { return x==y; }

bool wsequal(const RawSeg& x, const RawSeg& y)  { return x==y; }

//ostream& operator<< (ostream& os, wsint_t x) { return os << (int)x; }
//ostream& operator<< (ostream& os, wsfloat_t x) { return os << (float)x; }


/********** LISTS **********/
template <class T> class cons; 

template <class T> 
  bool wsequal (const boost::shared_ptr< cons<T> > & v1, 
		const boost::shared_ptr< cons<T> > & v2);

template <class T> 
ostream& operator<< (ostream& output, const boost::shared_ptr< cons<T> > & );

template <class T>
class cons {
public: 
  typedef boost::shared_ptr< cons<T> > ptr;
  cons(T a, ptr b);
  T car;
  ptr cdr;

  static ptr append(const ptr& x, const ptr &y);
  static ptr reverse(const ptr & ls);

  friend bool wsequal<T> (const boost::shared_ptr< cons<T> > & x, const boost::shared_ptr< cons<T> > & y);
  friend ostream& operator<< <T>(ostream& output, const boost::shared_ptr< cons<T> > & );
};

template<typename T>
bool IS_NULL(boost::shared_ptr< cons<T> > p) 
{ 
  return ((cons<T>*)0 == p.get());
}

template <class T>
cons<T>::cons(T a, boost::shared_ptr< cons<T> > b)
{
    car = a;
    cdr = b;
}

// Recursive for now... should change.
template <class T>
bool wsequal
        (const boost::shared_ptr< cons<T> > & x, 
	 const boost::shared_ptr< cons<T> > & y) 
{
  //printf("EQUALITY: \n");
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
}


// Currently unused:
template <class T>
ostream& operator<< (ostream& output, const boost::shared_ptr< cons<T> > & ls) 
{
  boost::shared_ptr< cons<T> > ptr = ls;
  output << "[";
  while(! IS_NULL(ptr)) {
    // Problem here is that we need to cast RawSeg to SigSeg and can't:
    //output << (RawSeg)(ptr->car);
    output << (wsint_t)3;
    output << ", ";
    ptr = ptr->cdr;
  }
  output << "]";
  return output;
}


// Not tail recursive!
template <class T>
//cons<T>::ptr cons<T>::append(const cons<T>::ptr & x, const cons<T>::ptr & y) {
boost::shared_ptr< cons<T> > cons<T>::append(
       const boost::shared_ptr< cons<T> > & x, 
       const boost::shared_ptr< cons<T> > & y) 
{
  if (IS_NULL(x))   return y;
  else return ptr(new cons<T>(x->car, append(x->cdr, y)));
}


template <class T>
boost::shared_ptr< cons<T> > cons<T>::reverse(
       const boost::shared_ptr< cons<T> > & ls)
{
  boost::shared_ptr< cons<T> > ptr = ls;
  boost::shared_ptr< cons<T> > acc( (cons<T>*) 0);
  while (!IS_NULL(ptr)) {
    acc = boost::shared_ptr< cons<T> >(new cons<T>(ptr->car, acc));
    ptr = ptr->cdr;
  }
  return acc;
}


/******** END LISTS ********/





/********** ARRAYS **********/
template <class T>
boost::shared_ptr< vector<T> > makeArray(wsint_t count, T initelem) {
  vector<T>* vec = new vector<T>((int) count);
  for(int i=0; i<(int)count; i++) {
    (*vec)[i] = (T)initelem;
  }
  return boost::shared_ptr< vector<T> >( vec );
}

//boost::shared_array<int> foo(new int[34]);

// TODO: wsequal


/******** END ARRAYS ********/





// Global setting:
bool WSOUTPUT_PREFIX = TRUE;



