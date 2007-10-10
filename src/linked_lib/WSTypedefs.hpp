
using boost::enable_shared_from_this;
using namespace std;
using namespace __gnu_cxx;

#define TRUE 1
#define FALSE 0

#define WSNULL 0
#define WSNULLSEG (RawSeg::NullRef)
#define WSNULLTIMEBASE 0

typedef bool wsunit_t;

typedef int16_t  wsint16_t;
typedef uint16_t wsuint16_t;
typedef int64_t  wsint64_t;

typedef int wsint_t;
//typedef double wsfloat_t;
typedef float  wsfloat_t; 
typedef double wsdouble_t; 
typedef bool   wsbool_t;
typedef string wsstring_t;
//typedef fftw_complex wscomplex_t;
//typedef _Complex double wscomplex_t;
typedef _Complex float wscomplex_t;


//typedef boost::intrusive_ptr<WSArrayStruct> wsarray_t;
//#define wsarray_t boost::intrusive_ptr<WSArrayStruct>

// [2007.04.15] I can't remember why I branched this off from the
// standard DEFINE_OUTPUT_TYPE.  Trying to go back.
//
/*
#define WS_DEFINE_OUTPUT_TYPE(type)                \
  inline void emit(const type &tuple) {         \
    uint i;                                     \
    for (i=0; i<m_outputs.size(); i++) {        \
      m_outputs[i]->enqueue(new type (tuple), this);  \
    } \
    totalEmits++; \
  }
*/

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
bool wsequal(wsint16_t x,   wsint16_t y)   { return x==y; }
bool wsequal(wsint64_t x,   wsint64_t y)   { return x==y; }
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

//template <class T> 
//ostream& operator<< (ostream& output, const boost::shared_ptr< cons<T> > & );

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
  //  friend ostream& operator<< <T>(ostream& output, const boost::shared_ptr< cons<T> > & );
};
/******** END LISTS ********/


/********** ARRAYS **********/

// Can flatten this when the size of the array is known.
template <class T>
struct WSArrayStruct {
  int rc;
  int len;
  //void* data;
  T* data;
};

template <class T>
boost::intrusive_ptr< WSArrayStruct<T> >
makeArrayUnsafe(wsint_t count, T initelem) {
  //T* vec = (T*) malloc(sizeof(T) * (int)count);
  T* vec = new T[count];
  WSArrayStruct<T>* arr = new WSArrayStruct<T>;
  arr->len = (int)count;
  arr->data = vec;
  arr->rc = 0;
  return boost::intrusive_ptr< WSArrayStruct<T> >(arr);
}

/*
makeArrayUnsafe(wsint_t count, int eltsize) {
  // FIXME: This is a bug too: 
  WSArrayStruct* stct = (WSArrayStruct*)malloc(sizeof(WSArrayStruct));
  stct->rc  = 0;
  stct->len = count;
  stct->data = malloc(count * eltsize);
  return boost::intrusive_ptr< WSArrayStruct<T> >(stct);
}
*/

template <class T>
boost::intrusive_ptr< WSArrayStruct<T> >
makeArray(wsint_t count, T initelem) {
  //T* vec = (T*) malloc(sizeof(T) * (int)count);
  T* vec = new T[count];
  for(int i=0; i<(int)count; i++) {
    vec[i] = (T)initelem;
  }

  WSArrayStruct<T>* arr = new WSArrayStruct<T>;
  arr->len = (int)count;
  arr->data = vec;
  arr->rc = 0;
  return boost::intrusive_ptr< WSArrayStruct<T> >(arr);
}

template <class T>
void intrusive_ptr_add_ref(WSArrayStruct<T>* p) {
  //  printf("Add rc! %d -> %d\n", p->rc, p->rc+1);
  p->rc ++;
}

template <class T>
void intrusive_ptr_release(WSArrayStruct<T>* p) {
  //printf("Decr rc! %d -> %d\n", p->rc, p->rc-1);
  p->rc --;
  if (p->rc == 0) {
    //printf("Freeing!\n");
    //free(p->data);
    delete [](T*)(p->data);
    free(p);
  }
}


//boost::shared_array<int> foo(new int[34]);

// TODO: wsequal


/******** END ARRAYS ********/

