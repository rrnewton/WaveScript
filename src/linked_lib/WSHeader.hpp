
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


/******** LISTS ********/
template <class T>
class cons {
public: 
  typedef boost::shared_ptr< cons<T> > ptr;
  cons(T a, ptr b) {
    car = a;
    cdr = b;
  }
  T car;
  ptr cdr;

  //  static ptr null;  //    = ptr((cons<T>*)0);
};

// We construct a single null object which we cast to what we need.
cons<int>::ptr NULL_LIST = cons<int>::ptr((cons<int>*)0);

