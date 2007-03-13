
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
#include <math.h>

#include <list>
#include <vector>
#include <string>
#include <iostream>
#include <ext/hash_map>

#include <fftw3.h>
//#include <complex.h>
#include <complex>
//#include <time.h>

#include <gsl/gsl_linalg.h>
#include <gsl/gsl_matrix.h>

using boost::enable_shared_from_this;
using namespace std;
using namespace __gnu_cxx;

#define TRUE 1
#define FALSE 0

#define WSNULL 0
#define WSNULLSEG (RawSeg::NullRef)
#define WSNULLTIMEBASE 0

typedef bool wsunit_t;

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

  static T ref(const ptr & ls, wsint_t ind);
  static wsint_t length(const ptr & ls);
  static ptr make(wsint_t len, T init);

  friend bool wsequal<T> (const boost::shared_ptr< cons<T> > & x, const boost::shared_ptr< cons<T> > & y);
  friend ostream& operator<< <T>(ostream& output, const boost::shared_ptr< cons<T> > & );
};
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



