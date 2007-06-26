//WSLIBDEPS: 

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
#include <boost/intrusive_ptr.hpp>
#include <boost/enable_shared_from_this.hpp>

#include <boost/functional/hash.hpp>

#include <stdio.h>
#include <math.h>

#include <list>
#include <vector>
#include <string>
#include <iostream>
#include <ext/hash_map>

//#include <fftw3.h>
//#include <complex.h>
#include <complex>
//#include <time.h>

//#include <gsl/gsl_linalg.h>
//#include <gsl/gsl_matrix.h>


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
typedef float  wsfloat_t; 
typedef double wsdouble_t; 
typedef bool   wsbool_t;
typedef string wsstring_t;
//typedef fftw_complex wscomplex_t;
//typedef _Complex double wscomplex_t;
typedef _Complex float wscomplex_t;


//typedef boost::intrusive_ptr<WSArrayStruct> wsarray_t;
#define wsarray_t boost::intrusive_ptr<WSArrayStruct>

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

// Can flatten this when the size of the array is known.
struct WSArrayStruct {
  int rc;
  int len;
  void* data;
};

wsarray_t makeArrayUnsafe(wsint_t count, int eltsize) {
  WSArrayStruct* stct = (WSArrayStruct*)malloc(sizeof(WSArrayStruct));
  stct->rc  = 0;
  stct->len = count;
  stct->data = malloc(count * eltsize);
  return wsarray_t(stct);
}

template <class T>
wsarray_t makeArray(wsint_t count, T initelem) {
  T* vec = (T*) malloc(sizeof(T) * (int)count);
  for(int i=0; i<(int)count; i++) {
    vec[i] = (T)initelem;
  }
  WSArrayStruct* arr = new WSArrayStruct;
  arr->len = (int)count;
  arr->data = vec;
  arr->rc = 0;
  return wsarray_t(arr);
}

void intrusive_ptr_add_ref(WSArrayStruct* p) {
  //  printf("Add rc! %d -> %d\n", p->rc, p->rc+1);
  p->rc ++;
}

void intrusive_ptr_release(WSArrayStruct* p) {
  //printf("Decr rc! %d -> %d\n", p->rc, p->rc-1);
  p->rc --;
  if (p->rc == 0) {
    //printf("Freeing!\n");
    free(p->data);
    free(p);
  }
}



//boost::shared_array<int> foo(new int[34]);

// TODO: wsequal


/******** END ARRAYS ********/



#include <ENSBox.hpp>




// Global setting:
bool WSOUTPUT_PREFIX = TRUE;

// This is a lame work-around:
static char global_show_buffer[500];
static ostringstream global_show_stream(ostringstream::out);

//============================================================
// This defines the WaveScript primitives that the code generated by
// the WaveScript compiler depends upon.
 class WSPrim {

   public:   


  
   // This is a work-around to the fact that we can't have stmt blocks
   // {...} in expression position.
   inline static string show_helper(int ignored) {
     return string(global_show_buffer);
   }
   // This is a work-around to the fact that we can't have stmt blocks
   // {...} in expression position.
   inline static string show_helper2(void* ignored) {
     string result = global_show_stream.str();
     global_show_stream.str(string(""));
     return string(result);
   }
   static wsstring_t stringappend(const wsstring_t& A, const wsstring_t& B) {
     return A+B;
   }

   inline static wsbool_t wsnot(wsbool_t b) {
     return (wsbool_t)!b;
   }

   // SigSegs:
   static wsint_t width(const RawSeg& w) {
     return (wsint_t)w.length();
   }
   static wsint_t start(const RawSeg& w) {
     return (wsint_t)w.start();
   }
   static wsint_t end(const RawSeg& w) {
     return (wsint_t)w.end();
   }

   // Old Array representation.
   /*
   template <class T>
   static RawSeg toSigseg(const boost::shared_ptr< vector< T > >& arr, 
			  wsint_t startsamp, int timebase) {
     int len = (*arr).size();
     RawSeg rs((SeqNo)startsamp,len, DataSeg,0,sizeof(T),Unitless,true);     
     Byte* direct;
     rs.getDirect(0, len, direct);
     for(int i=0; i<len; i++) ((T*)direct)[i] = (*arr)[i];
     rs.releaseAll();
     return rs;
   }
   */

   static RawSeg joinsegs(const RawSeg& a, const RawSeg& b) {
     return RawSeg::append(a,b);
   }
   // Currently takes start sample number (inclusive) and length.
   // TODO: Need to take SeqNo for start!
   static RawSeg subseg(const RawSeg& ss, wsint_t start, wsint_t len) {
     uint32_t offset = (uint32_t)((SeqNo)start - ss.start());
     //return ss.subseg(offset, offset+len);
     return RawSeg::subseg(ss, offset, len); // INCONSISTENT DOCUMENTATION! FIXME!
   }
      
   // Simple hash function, treat everything as a block of bits.
   static size_t generic_hash(unsigned char* ptr, int size) {
     size_t hash = 5381;
     int c;
     for(int i=0; i<size; i++) 
       hash = ((hash << 5) + hash) + ptr[i]; /* hash * 33 + c */	 	 
     return hash;
   }   
   
   static void wserror(wsstring_t str) {
     printf("wserror: %s\n", str.c_str());
     exit(1);
   }

//    template <class T>
//    static boost::shared_ptr<vector< boost::shared_ptr< vector<T> > > >
//        m_invert(boost::shared_ptr<vector< boost::shared_ptr< vector<T> > > > mat)
//    {
//      return mat;
//    }


   // Optimized version, unfinished.
   /*
   static unsigned long hash(unsigned char* ptr, int size) {
     int stride = sizeof(unsigned long);
     unsigned long hash = 5381;
     unsigned long* chunked = (unsigned long*)ptr;
     int rem = size % stride;
     for (int i=0; i < size/stride; i++) {
       hash = ((hash << 5) + hash) + chunked[i];
     }
     for (int i=0; i < rem; i++) {
       //FINISH
     }
     return hash;
   }
   */

};


// LISTS: 
//==============================================================================
// We can't define list functions inside WSPrim because of template magic

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

template <class T>
T cons<T>::ref(const boost::shared_ptr< cons<T> > & ls,
		   wsint_t ind)
{  
  boost::shared_ptr< cons<T> > ptr = ls;
  for(int i=0; i<ind; i++) {       
    if (IS_NULL(ptr)) WSPrim::wserror(string("listRef: not enough elements"));
    ptr = ptr->cdr;
  }
  if (IS_NULL(ptr)) WSPrim::wserror(string("listRef: not enough elements"));
  return ptr->car;
}

template <class T>
wsint_t cons<T>::length(const boost::shared_ptr< cons<T> > & ls)
{  
  boost::shared_ptr< cons<T> > ptr = ls;
  int count = 0;
  while (!(IS_NULL(ptr))) { 
    ptr = ptr->cdr;
    count++; 
  }
  return (wsint_t)count;
}

template <class T>
boost::shared_ptr< cons<T> > cons<T>::make(wsint_t n, T init)
{
  int count = n;
  // Start out null.
  boost::shared_ptr< cons<T> > p((cons<T>*)0);
  while (count > 0) {
    p = boost::shared_ptr< cons<T> >(new cons<T>(init, p));
    count--;
  }  
  return p;
}




//==============================================================================
// These are built-in WSBoxes. 
// Most of these are intended to go away at some point.
class WSBuiltins {
   
public:

   /* Zip2 operator: takes 2 input streams of types T1 and T2 and emits zipped
      tuples, each containing exactly one element from each input stream. */
   template <class T1, class T2> class Zip2: public WSBox {
   public:
     Zip2< T1, T2 >() : WSBox("zip2") {}
  
     /* Zip2 output type */
     struct Output
     {
       T1 _first;
       T2 _second;
    
       Output(T1 first, T2 second) : _first(first), _second(second) {}
       friend ostream& operator << (ostream& o, const Output& output) { 
	 cout << "< " << output._first << ", " << output._second << " >"; return o; 
       }
     };

   private:
     DEFINE_OUTPUT_TYPE(Output);
  
     bool iterate(uint32_t port, void *item)
     {
       m_inputs[port]->requeue(item);

       bool _e1, _e2; /* indicates if elements available on input streams */
       _e1 = (m_inputs[0]->peek() != NULL); _e2 = (m_inputs[1]->peek() != NULL);
    
       while(_e1 && _e2) {
	 T1* _t1 = (T1*)(m_inputs[0]->dequeue()); 
	 T2* _t2 = (T2*)(m_inputs[1]->dequeue()); 
	 emit(Output(*_t1, *_t2)); /* emit zipped tuple */
	 delete _t1; delete _t2;
	 _e1 = (m_inputs[0]->peek() != NULL); _e2 = (m_inputs[1]->peek() != NULL);
       }
       return true;
     }
   };


  /* This takes Signal(T) to Signal(SigSeg(T)) */
  class Window : public WSBox{    
   
    public:
    Window(int winsize, size_t bitsize) : WSBox("Window"),
      //rs(new RawSeg(RawSignalPtr(new RawSignal(0)),(SeqNo)0,winsize))
      rs(new RawSeg((SeqNo)0,winsize,DataSeg,0,bitsize,Unitless,true))      
      //RawSeg(const RawSignalPtr parent, SeqNo start, uint32_t length, GapType isGap = DataSeg);
    {      
      window_size = winsize;
      elem_size = bitsize;
      ind = 0;      
      sampnum = 0;      
      assert(rs->getDirect(0,winsize,current_buf));
    }

    private:
    DEFINE_OUTPUT_TYPE(RawSeg);
    
    int window_size;
    size_t elem_size;

    SeqNo sampnum;
    RawSeg* rs;
    Byte* current_buf;
    int ind;
        
    bool iterate(uint32_t port, void *item)
    {
      memcpy((current_buf + (ind*elem_size)),
	     item,
	     elem_size);
      ind++;
      sampnum++;
      if (ind == window_size) {
	rs->release(current_buf);
	emit(*rs);
	ind = 0;
	rs = new RawSeg(sampnum,window_size,DataSeg,0,elem_size,Unitless,true);
	//rs = new RawSeg(RawSignalPtr(new RawSignal(0)), sampnum, window_size);
	assert(rs->getDirect(0,window_size,current_buf));
      }
      return true;
    }
  };



  /* This takes Signal(T) to Signal(SigSeg(T)) */
  class Timer : public WSSource{
   
    public:
    Timer(wsfloat_t freq) : WSSource("Timer") {
      period = (int)(1000000.0 * (1.0 / freq));
      Launch();
    }

    DEFINE_SOURCE_TYPE(wsunit_t);

    private:
    int period; 

    void *run_thread() {
      while (!Shutdown()) {
	source_emit(0);
	usleep(period);
      }
      return NULL;
    }
  };
  
};

/* These structs represent tuples in the WS program. */

class Iter_tmp_18 : public WSBox {
  public:
  DEFINE_OUTPUT_TYPE(RawSeg);
  
  Iter_tmp_18() {
  } 
  
  private:
  
  /* WaveScript input type: (Sigseg Int16) */
  bool iterate(uint32_t portnum, void* datum) {
    /* Naturalize input type to meet expectations of the iterate-body. */
    /* Sigseg input.  This is a bit of a trick, uses ref rather than value: */
    RawSeg& x_2 = *(RawSeg*) datum;
    void* ___VIRTQUEUE____1 = (void*)0;
    wsint_t tmp_4 = WSPrim::width(x_2);
    wsstring_t tmp_6 = WSPrim::show_helper(sprintf(global_show_buffer, "%d", tmp_4));
    wsstring_t tmp_8 = WSPrim::stringappend(tmp_6, string("\n"));
    wsstring_t tmp_10 = WSPrim::show_helper2(global_show_stream << tmp_8);
    wsstring_t tmp_12 = WSPrim::stringappend(string("GOT WINDOW "), tmp_10);
    cout << tmp_12;
    emit((RawSeg)x_2);
    return FALSE;
  } 
} 
;


class PrintQueryOutput : public WSBox {
  public:
  PrintQueryOutput(const char *name) : WSBox("PrintQueryOutput") {}
  
  private:
  DEFINE_NO_OUTPUT_TYPE;
  
  bool iterate(uint32_t port, void *input) {
    RawSeg *element = (RawSeg *)input;
    if(WSOUTPUT_PREFIX) printf("WSOUT: ");
    cout << SigSeg<wsint16_t>((*element));
    ;
    printf("\n");
    return false;
  } 
} 
;



int main(int argc, char ** argv)
{
  /* set global variable(s) */
  if (misc_parse_out_switch(&argc, argv, "no_prefix", 0))
    WSOUTPUT_PREFIX = FALSE;
  else WSOUTPUT_PREFIX = TRUE;

  /* initialize subsystems */ 
  WSInit(&argc, argv);

  /* declare variable to hold final result */
  //WSBox* toplevel;

  /* begin constructing operator graph */
  WSSource* tmp_16 = (WSSource*) new ENSBoxSource<wsint16_t>(1);WSBox* tmp_18 = new Iter_tmp_18();
  tmp_18->connect(tmp_16);
  WSBox* toplevel = tmp_18;

  /* dump output of query -- WaveScript type = (Stream (Sigseg Int16)) */
  PrintQueryOutput out = PrintQueryOutput("WSOUT");
  out.connect(toplevel);

  /* now, run */
  WSRun();

  return 0;
}

