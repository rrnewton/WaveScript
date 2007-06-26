//WSLIBDEPS:  -lfftw3f

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



#include <fftw3.h>
#include "/home/newton/wavescript/src/linked_lib/FFTW_wrappers.cpp"




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

   inline static wsfloat_t CNorm(wscomplex_t c) {
     wsfloat_t re =  __real__ c;
     wsfloat_t im =  __imag__ c;

     return sqrt ((re*re) + (im*im));
     /*
     if     (0.0 == re)  return im;
     else if (0.0 == im) return re;
     else {
       wsfloat_t absre = abs(re);
       wsfloat_t absim = abs(im);
       if (absre >= absim ) return (absre * sqrt(1.0 + (im*im / (re*re))));
       else                 return (absim * sqrt(1.0 + (im*im / (re*re))));
     }
     */
   }
  
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
// We can't define list functions inside the WSPrim class because of template magic

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
struct tuptyp_451 {
  RawSeg fld1;
  RawSeg fld2;
  tuptyp_451() {}
  tuptyp_451(RawSeg tmp_474, RawSeg tmp_473) :
    fld1(tmp_474), 
    fld2(tmp_473) {}
} 
;

string show_tuptyp_451(tuptyp_451 rec) {
  ostringstream oss(ostringstream::out);  oss << "(";
  oss << WSPrim::show_helper2(global_show_stream << SigSeg<wscomplex_t>(rec.fld1)); 
;
  oss << ", ";
  oss << WSPrim::show_helper2(global_show_stream << SigSeg<wsfloat_t>(rec.fld2)); 
;
  oss << ")";
  return oss.str();}

bool wsequal(const tuptyp_451& x, const tuptyp_451& y) {
 return  wsequal(x.fld1, y.fld1)  &&  wsequal(x.fld2, y.fld2) ;
}

struct tuptyp_455 {
  wsfloat_t fld1;
  RawSeg fld2;
  tuptyp_455() {}
  tuptyp_455(wsfloat_t tmp_472, RawSeg tmp_471) :
    fld1(tmp_472), 
    fld2(tmp_471) {}
} 
;

string show_tuptyp_455(tuptyp_455 rec) {
  ostringstream oss(ostringstream::out);  oss << "(";
  oss << WSPrim::show_helper(sprintf(global_show_buffer, "%f", rec.fld1)); 
;
  oss << ", ";
  oss << WSPrim::show_helper2(global_show_stream << SigSeg<wsfloat_t>(rec.fld2)); 
;
  oss << ")";
  return oss.str();}

bool wsequal(const tuptyp_455& x, const tuptyp_455& y) {
 return  wsequal(x.fld1, y.fld1)  &&  wsequal(x.fld2, y.fld2) ;
}

struct tuptyp_460 {
  wsbool_t fld1;
  wsint_t fld2;
  wsint_t fld3;
  tuptyp_460() {}
  tuptyp_460(wsbool_t tmp_470, wsint_t tmp_469, wsint_t tmp_468) :
    fld1(tmp_470), 
    fld2(tmp_469), 
    fld3(tmp_468) {}
} 
;

string show_tuptyp_460(tuptyp_460 rec) {
  ostringstream oss(ostringstream::out);  oss << "(";
  oss << WSPrim::show_helper(sprintf(global_show_buffer, "%s", (rec.fld1 ? "true" : "false"))); 
;
  oss << ", ";
  oss << WSPrim::show_helper(sprintf(global_show_buffer, "%d", rec.fld2)); 
;
  oss << ", ";
  oss << WSPrim::show_helper(sprintf(global_show_buffer, "%d", rec.fld3)); 
;
  oss << ")";
  return oss.str();}

bool wsequal(const tuptyp_460& x, const tuptyp_460& y) {
 return  wsequal(x.fld1, y.fld1)  &&  wsequal(x.fld2, y.fld2)  &&  wsequal(x.fld3, y.fld3) ;
}

class WSDataFileSource_462 : public WSSource {
  public:
  WSDataFileSource_462(wsstring_t path, wsstring_t mode, wsint_t repeats) {
    _f = fopen(path.c_str(), binarymode ? "rb" : "r");
    binarymode = (mode == string("binary"));
    if (_f == NULL) {
      chatter(LOG_CRIT, "Unable to open data file %s: %m", path.c_str());
      abort();
    }
    Launch();
  } 
  
    DEFINE_SOURCE_TYPE(RawSeg);
  
  private:
    FILE* _f;
    bool binarymode;
  void *run_thread() {
    int sampnum = 0;
    fseek(_f, (wsint_t)0, SEEK_SET);
    while (!Shutdown()) {
      RawSeg storage(sampnum, 4096, DataSeg, 0, sizeof(wsint16_t), Unitless, true);
      Byte* buf;
      storage.getDirect(0, 4096, buf);// Nasty hack to handle strings separately, since we're not using C strings:
      // Cap of a 100 on length of strings read in:
      int status = 0;
      if (!binarymode) {
        // ERROR //status = fscanf(_f, "%hd", &tup);
      } 
      else {
        // The binary format of tuples matches that in the file:
        for (int i=0; i<4096; i++) {
          status += fread(buf+(i*sizeof(wsint16_t)),sizeof(wsint16_t), 1,_f);
          fseek(_f, 6, SEEK_CUR);
        } 
      } 
      if (status != 4096 * (binarymode ? 1 : 1)) {
        chatter(LOG_WARNING, "dataFile EOF encountered (%d).", status);
        WSSched::stop();
        return NULL;
      } 
      storage.release(buf);
      source_emit(storage);
      storage = RawSeg(sampnum, 4096, DataSeg, 0, sizeof(wsint16_t), Unitless, true);
      sampnum += 4096;
    } 
    return NULL;} 
} 
;
class Iter__ch1_1 : public WSBox {
  public:
  DEFINE_OUTPUT_TYPE(RawSeg);
  
  Iter__ch1_1() {
  } 
  
  private:
  
  /* WaveScript input type: (Sigseg Int16) */
  bool iterate(uint32_t portnum, void* datum) {
    /* Naturalize input type to meet expectations of the iterate-body. */
    /* Sigseg input.  This is a bit of a trick, uses ref rather than value: */
    RawSeg& w_4 = *(RawSeg*) datum;
    void* ___VIRTQUEUE____3 = (void*)0;
    wsint_t len_81 = WSPrim::width(w_4);
    WSArrayStruct* tmp_464 = new WSArrayStruct;
    tmp_464->len = (int)len_81;
    tmp_464->data = malloc(sizeof(wsfloat_t) * (int)len_81);
    tmp_464->rc = 0;
    wsarray_t outls_80 = wsarray_t(tmp_464);
    wsint_t tmp_90 = (len_81 - (wsint_t)1);
    for (int i_82 = (wsint_t)0; i_82 <= tmp_90; i_82++) {
      wsint16_t a_7 = (*((wsint16_t*)(*(w_4.index_i(i_82)))));
      wsfloat_t tmp_92 = (wsfloat_t)a_7;
      ((wsfloat_t *)outls_80->data)[i_82] = tmp_92;
      wsunit_t tmp_94 = ((wsunit_t)0);
    } 
    wsint_t tmp_96 = WSPrim::start(w_4);
    int tmp_98 = (w_4.getTimebase());
    
         RawSeg tmp_100((SeqNo)tmp_96, outls_80->len, DataSeg, 0, sizeof(wsfloat_t), Unitless, true);
         { 
           Byte* direct;
           tmp_100.getDirect(0, outls_80->len, direct);
           memcpy(direct, outls_80->data, sizeof(wsfloat_t) * outls_80->len);
           tmp_100.releaseAll(); 
         }
    emit((RawSeg)tmp_100);
    return FALSE;
  } 
} 
;

class Iter_rw1_8 : public WSBox {
  public:
  DEFINE_OUTPUT_TYPE(RawSeg);
  
  Iter_rw1_8() {
    RawSeg tmp_108 = WSNULLSEG;
    RawSeg tmp_110 = tmp_108;
     acc_11 = tmp_110;
    wsbool_t tmp_106 = FALSE;
     need_feed_10 = tmp_106;
    wsbool_t tmp_104 = TRUE;
     go_9 = tmp_104;
  } 
  
  private:
  RawSeg acc_11; // WS type: (Ref (Sigseg Float))
  wsbool_t need_feed_10; // WS type: (Ref Bool)
  wsbool_t go_9; // WS type: (Ref Bool)
  
  /* WaveScript input type: (Sigseg Float) */
  bool iterate(uint32_t portnum, void* datum) {
    /* Naturalize input type to meet expectations of the iterate-body. */
    /* Sigseg input.  This is a bit of a trick, uses ref rather than value: */
    RawSeg& win_13 = *(RawSeg*) datum;
    void* ___VIRTQUEUE____12 = (void*)0;
    RawSeg tmp_182 = acc_11;
    RawSeg tmp_184 = WSPrim::joinsegs(tmp_182, win_13);
    acc_11 = tmp_184;
    go_9 = TRUE;
    while (1) {
      wsbool_t tmp_112 = go_9;
      wsbool_t grosshack = tmp_112;
      if (grosshack) {
        wsbool_t tmp_114 = need_feed_10;
        wsunit_t tmp_180;
        if (tmp_114) {
          RawSeg tmp_116 = acc_11;
          wsint_t tmp_118 = WSPrim::width(tmp_116);
          wsbool_t tmp_120 = (tmp_118 > (wsint_t)96);
          wsunit_t tmp_142;
          if (tmp_120) {
            RawSeg tmp_122 = acc_11;
            RawSeg tmp_124 = acc_11;
            wsint_t tmp_126 = WSPrim::start(tmp_124);
            wsint_t tmp_128 = (tmp_126 + (wsint_t)96);
            RawSeg tmp_130 = acc_11;
            wsint_t tmp_132 = WSPrim::width(tmp_130);
            wsint_t tmp_134 = (tmp_132 - (wsint_t)96);
            RawSeg tmp_136 = WSPrim::subseg(tmp_122, tmp_128, tmp_134);
            acc_11 = tmp_136;
            need_feed_10 = FALSE;
            wsunit_t tmp_138 = ((wsunit_t)0);
             tmp_142 = 0;
          } else {
            go_9 = FALSE;
            wsunit_t tmp_140 = ((wsunit_t)0);
             tmp_142 = 0;
          }
           tmp_180 = tmp_142;
        } else {
          RawSeg tmp_144 = acc_11;
          wsint_t tmp_146 = WSPrim::width(tmp_144);
          wsbool_t tmp_148 = (tmp_146 > (wsint_t)32);
          wsunit_t tmp_178;
          if (tmp_148) {
            RawSeg tmp_166 = acc_11;
            RawSeg tmp_168 = acc_11;
            wsint_t tmp_170 = WSPrim::start(tmp_168);
            RawSeg tmp_172 = WSPrim::subseg(tmp_166, tmp_170, (wsint_t)32);
            emit((RawSeg)tmp_172);
            RawSeg tmp_150 = acc_11;
            RawSeg tmp_152 = acc_11;
            wsint_t tmp_154 = WSPrim::start(tmp_152);
            wsint_t tmp_156 = (tmp_154 + (wsint_t)32);
            RawSeg tmp_158 = acc_11;
            wsint_t tmp_160 = WSPrim::width(tmp_158);
            wsint_t tmp_162 = (tmp_160 - (wsint_t)32);
            RawSeg tmp_164 = WSPrim::subseg(tmp_150, tmp_156, tmp_162);
            acc_11 = tmp_164;
            need_feed_10 = TRUE;
            wsunit_t tmp_174 = ((wsunit_t)0);
             tmp_178 = 0;
          } else {
            go_9 = FALSE;
            wsunit_t tmp_176 = ((wsunit_t)0);
             tmp_178 = 0;
          }
           tmp_180 = tmp_178;
        }
      } else break; 
    } 
    return FALSE;
  } 
} 
;

class Iter_hn_14 : public WSBox {
  public:
  DEFINE_OUTPUT_TYPE(RawSeg);
  
  Iter_hn_14() {
    wsint_t tmp_192 = (wsint_t)0;
     _lastLen_16 = tmp_192;
    wsarray_t tmp_188 = wsarray_t(0);
    wsarray_t tmp_190 = tmp_188;
     _hanning_15 = tmp_190;
  } 
  
  private:
  wsint_t _lastLen_16; // WS type: (Ref Int)
  wsarray_t _hanning_15; // WS type: (Ref (Array Float))
  
  /* WaveScript input type: (Sigseg Float) */
  bool iterate(uint32_t portnum, void* datum) {
    /* Naturalize input type to meet expectations of the iterate-body. */
    /* Sigseg input.  This is a bit of a trick, uses ref rather than value: */
    RawSeg& win_18 = *(RawSeg*) datum;
    void* ___VIRTQUEUE____17 = (void*)0;
    wsint_t tmp_216 = _lastLen_16;
    wsint_t tmp_218 = WSPrim::width(win_18);
    wsbool_t tmp_220 = wsequal(tmp_216, tmp_218);
    wsbool_t tmp_222 = !(tmp_220);
    if (tmp_222) {
      wsint_t tmp_254 = WSPrim::width(win_18);
      _lastLen_16 = tmp_254;
      wsint_t tmp_250 = _lastLen_16;
      wsarray_t tmp_252 = makeArray(tmp_250, (wsfloat_t)0.0);
      _hanning_15 = tmp_252;
      wsint_t tmp_224 = _lastLen_16;
      wsint_t tmp_226 = (tmp_224 - (wsint_t)1);
      for (int i_21 = (wsint_t)0; i_21 <= tmp_226; i_21++) {
        wsarray_t tmp_228 = _hanning_15;
        wsfloat_t tmp_230 = (wsfloat_t)i_21;
        wsfloat_t tmp_232 = ((wsfloat_t)6.283185307179586 * tmp_230);
        wsint_t tmp_234 = _lastLen_16;
        wsint_t tmp_236 = (tmp_234 - (wsint_t)1);
        wsfloat_t tmp_238 = (wsfloat_t)tmp_236;
        wsfloat_t tmp_240 = (tmp_232 / tmp_238);
        wsfloat_t tmp_242 = cos(tmp_240);
        wsfloat_t tmp_244 = ((wsfloat_t)1.0 - tmp_242);
        wsfloat_t tmp_246 = ((wsfloat_t)0.5 * tmp_244);
        ((wsfloat_t *)tmp_228->data)[i_21] = tmp_246;
        wsunit_t tmp_248 = ((wsunit_t)0);
      } 
      wsunit_t tmp_256 = ((wsunit_t)0);
    } else {
    }
    wsint_t tmp_194 = _lastLen_16;
    wsarray_t buf_19 = makeArray(tmp_194, (wsfloat_t)0.0);
    wsint_t tmp_202 = _lastLen_16;
    wsint_t tmp_204 = (tmp_202 - (wsint_t)1);
    for (int i_20 = (wsint_t)0; i_20 <= tmp_204; i_20++) {
      wsarray_t tmp_206 = _hanning_15;
      wsfloat_t tmp_208 = ((wsfloat_t *)tmp_206->data)[i_20];
      wsfloat_t tmp_210 = (*((wsfloat_t*)(*(win_18.index_i(i_20)))));
      wsfloat_t tmp_212 = (tmp_208 * tmp_210);
      ((wsfloat_t *)buf_19->data)[i_20] = tmp_212;
      wsunit_t tmp_214 = ((wsunit_t)0);
    } 
    wsint_t tmp_196 = WSPrim::start(win_18);
    int tmp_198 = (win_18.getTimebase());
    
         RawSeg tmp_200((SeqNo)tmp_196, buf_19->len, DataSeg, 0, sizeof(wsfloat_t), Unitless, true);
         { 
           Byte* direct;
           tmp_200.getDirect(0, buf_19->len, direct);
           memcpy(direct, buf_19->data, sizeof(wsfloat_t) * buf_19->len);
           tmp_200.releaseAll(); 
         }
    emit((RawSeg)tmp_200);
    return FALSE;
  } 
} 
;

class Iter_freq_22 : public WSBox {
  public:
  DEFINE_OUTPUT_TYPE(tuptyp_451);
  
  Iter_freq_22() {
  } 
  
  private:
  
  /* WaveScript input type: (Sigseg Float) */
  bool iterate(uint32_t portnum, void* datum) {
    /* Naturalize input type to meet expectations of the iterate-body. */
    /* Sigseg input.  This is a bit of a trick, uses ref rather than value: */
    RawSeg& x_24 = *(RawSeg*) datum;
    void* ___VIRTQUEUE____23 = (void*)0;
    int len_467 = x_24.length();
    wsarray_t tmp_260 = makeArrayUnsafe(len_467, sizeof(wsfloat_t));
    for(int i=0; i<len_467; i++) {
      wsfloat_t tmp_466 = (*((wsfloat_t*)(*(x_24.index_i(i)))));
      ((wsfloat_t *)tmp_260->data)[i] = tmp_466;
    }
    wsarray_t tmp_262 = fftR2C(tmp_260);
    wsint_t tmp_264 = WSPrim::start(x_24);
    int tmp_266 = (x_24.getTimebase());
    
         RawSeg tmp_268((SeqNo)tmp_264, tmp_262->len, DataSeg, 0, sizeof(wscomplex_t), Unitless, true);
         { 
           Byte* direct;
           tmp_268.getDirect(0, tmp_262->len, direct);
           memcpy(direct, tmp_262->data, sizeof(wscomplex_t) * tmp_262->len);
           tmp_268.releaseAll(); 
         }
    tuptyp_451 tmp_270 = tuptyp_451(tmp_268, x_24);
    emit((tuptyp_451)tmp_270);
    return FALSE;
  } 
} 
;

class Iter_wscores_25 : public WSBox {
  public:
  DEFINE_OUTPUT_TYPE(tuptyp_455);
  
  Iter_wscores_25() {
  } 
  
  private:
  
  /* WaveScript input type: (Struct tuptyp_451) */
  bool iterate(uint32_t portnum, void* datum) {
    /* Naturalize input type to meet expectations of the iterate-body. */
    tuptyp_451 pattmp_27 = *((tuptyp_451*) datum);
    void* ___VIRTQUEUE____26 = (void*)0;
    RawSeg w_28 = (pattmp_27.fld1);
    RawSeg orig_29 = (pattmp_27.fld2);
    wscomplex_t tmp_274 = (*((wscomplex_t*)(*(w_28.index_i((wsint_t)4)))));
    wscomplex_t tmp_276 = (*((wscomplex_t*)(*(w_28.index_i((wsint_t)5)))));
    wscomplex_t tmp_278 = (tmp_274 + tmp_276);
    wscomplex_t tmp_280 = (*((wscomplex_t*)(*(w_28.index_i((wsint_t)6)))));
    wscomplex_t tmp_282 = (tmp_278 + tmp_280);
    wscomplex_t tmp_284 = (*((wscomplex_t*)(*(w_28.index_i((wsint_t)7)))));
    wscomplex_t tmp_286 = (tmp_282 + tmp_284);
    wsfloat_t result_30 = WSPrim::CNorm(tmp_286);
    tuptyp_455 tmp_290 = tuptyp_455(result_30, orig_29);
    emit((tuptyp_455)tmp_290);
    return FALSE;
  } 
} 
;

class Iter_detections_31 : public WSBox {
  public:
  DEFINE_OUTPUT_TYPE(tuptyp_460);
  
  Iter_detections_31() {
    wsfloat_t tmp_310 = (wsfloat_t)0.0;
     thresh_value_40 = tmp_310;
    wsbool_t tmp_308 = FALSE;
     trigger_39 = tmp_308;
    wsfloat_t tmp_306 = (wsfloat_t)0.0;
     smoothed_mean_38 = tmp_306;
    wsfloat_t tmp_304 = (wsfloat_t)0.0;
     smoothed_var_37 = tmp_304;
    wsint_t tmp_302 = (wsint_t)0;
     _start_36 = tmp_302;
    wsfloat_t tmp_300 = (wsfloat_t)0.0;
     trigger_value_35 = tmp_300;
    wsint_t tmp_298 = (wsint_t)300;
     startup_34 = tmp_298;
    wsint_t tmp_296 = (wsint_t)0;
     refract_33 = tmp_296;
    wsint_t tmp_294 = (wsint_t)0;
     noise_lock_32 = tmp_294;
  } 
  
  private:
  wsfloat_t thresh_value_40; // WS type: (Ref Float)
  wsbool_t trigger_39; // WS type: (Ref Bool)
  wsfloat_t smoothed_mean_38; // WS type: (Ref Float)
  wsfloat_t smoothed_var_37; // WS type: (Ref Float)
  wsint_t _start_36; // WS type: (Ref Int)
  wsfloat_t trigger_value_35; // WS type: (Ref Float)
  wsint_t startup_34; // WS type: (Ref Int)
  wsint_t refract_33; // WS type: (Ref Int)
  wsint_t noise_lock_32; // WS type: (Ref Int)
  
  /* WaveScript input type: (Struct tuptyp_455) */
  bool iterate(uint32_t portnum, void* datum) {
    /* Naturalize input type to meet expectations of the iterate-body. */
    tuptyp_455 pattmp_42 = *((tuptyp_455*) datum);
    void* ___VIRTQUEUE____41 = (void*)0;
    wsfloat_t score_43 = (pattmp_42.fld1);
    RawSeg win_44 = (pattmp_42.fld2);
    wsbool_t tmp_312 = trigger_39;
    if (tmp_312) {
      wsint_t tmp_346 = WSPrim::end(win_44);
      wsint_t tmp_348 = _start_36;
      wsint_t tmp_350 = (tmp_346 - tmp_348);
      wsbool_t tmp_352 = (tmp_350 > (wsint_t)48000);
      if (tmp_352) {
        wsstring_t tmp_358 = WSPrim::show_helper(sprintf(global_show_buffer, "%d", (wsint_t)48000)); 
        ;
        wsstring_t tmp_360 = WSPrim::show_helper2(global_show_stream << tmp_358); 
        ;
        wsstring_t tmp_362 = WSPrim::stringappend(tmp_360, string(", re-estimating noise\n"));
        wsstring_t tmp_364 = WSPrim::show_helper2(global_show_stream << tmp_362); 
        ;
        wsstring_t tmp_366 = WSPrim::stringappend(string("Detection length exceeded maximum of "), tmp_364);
        cout << tmp_366;
        wsint_t tmp_354 = noise_lock_32;
        wsint_t tmp_356 = (tmp_354 + (wsint_t)1);
        noise_lock_32 = tmp_356;
        thresh_value_40 = (wsfloat_t)0.0;
        trigger_39 = FALSE;
        smoothed_mean_38 = (wsfloat_t)0.0;
        smoothed_var_37 = (wsfloat_t)0.0;
        _start_36 = (wsint_t)0;
        trigger_value_35 = (wsfloat_t)0.0;
        startup_34 = (wsint_t)300;
        refract_33 = (wsint_t)0;
        wsunit_t tmp_368 = ((wsunit_t)0);
      } else {
      }
      wsfloat_t tmp_314 = thresh_value_40;
      wsbool_t tmp_316 = (score_43 > tmp_314);
      wsunit_t tmp_370;
      if (tmp_316) {
        refract_33 = (wsint_t)40;
        wsunit_t tmp_318 = ((wsunit_t)0);
         tmp_370 = 0;
      } else {
        wsint_t tmp_320 = refract_33;
        wsbool_t tmp_322 = (tmp_320 > (wsint_t)0);
        wsunit_t tmp_344;
        if (tmp_322) {
          wsint_t tmp_324 = refract_33;
          wsint_t tmp_326 = (tmp_324 - (wsint_t)1);
          refract_33 = tmp_326;
          wsunit_t tmp_328 = ((wsunit_t)0);
           tmp_344 = 0;
        } else {
          trigger_39 = FALSE;
          wsint_t tmp_332 = _start_36;
          wsint_t tmp_334 = (tmp_332 - (wsint_t)2400);
          wsint_t tmp_336 = WSPrim::end(win_44);
          wsint_t tmp_338 = (tmp_336 + (wsint_t)2400);
          tuptyp_460 tmp_340 = tuptyp_460(TRUE, tmp_334, tmp_338);
          emit((tuptyp_460)tmp_340);
          tuptyp_460 tmp_330 = tuptyp_460(TRUE, (wsint_t)0, (wsint_t)0);
          emit((tuptyp_460)tmp_330);
          _start_36 = (wsint_t)0;
          wsunit_t tmp_342 = ((wsunit_t)0);
           tmp_344 = 0;
        }
         tmp_370 = tmp_344;
      }
    } else {
      wsfloat_t tmp_372 = smoothed_var_37;
      wsfloat_t tmp_374 = sqrt(tmp_372);
      wsfloat_t tmp_376 = ((wsfloat_t)8.0 * tmp_374);
      wsfloat_t tmp_378 = smoothed_mean_38;
      wsfloat_t thresh_45 = (tmp_376 + tmp_378);
      wsint_t tmp_398 = startup_34;
      wsbool_t tmp_400 = wsequal(tmp_398, (wsint_t)0);
      wsbool_t tmp_406;
      if (tmp_400) {
        wsbool_t tmp_402 = (score_43 > thresh_45);
        wsbool_t tmp_404;
        if (tmp_402) {
           tmp_404 = TRUE;
        } else {
           tmp_404 = FALSE;
        }
         tmp_406 = tmp_404;
      } else {
         tmp_406 = FALSE;
      }
      if (tmp_406) {
        trigger_39 = TRUE;
        refract_33 = (wsint_t)40;
        thresh_value_40 = thresh_45;
        wsint_t tmp_408 = WSPrim::start(win_44);
        _start_36 = tmp_408;
        trigger_value_35 = score_43;
        wsunit_t tmp_410 = ((wsunit_t)0);
      } else {
        wsfloat_t tmp_424 = (score_43 * (wsfloat_t)0.0010000000000000009);
        wsfloat_t tmp_426 = smoothed_mean_38;
        wsfloat_t tmp_428 = (tmp_426 * (wsfloat_t)0.999);
        wsfloat_t tmp_430 = (tmp_424 + tmp_428);
        smoothed_mean_38 = tmp_430;
        wsfloat_t tmp_412 = smoothed_mean_38;
        wsfloat_t delt_46 = (score_43 - tmp_412);
        wsfloat_t tmp_414 = (delt_46 * delt_46);
        wsfloat_t tmp_416 = (tmp_414 * (wsfloat_t)0.0010000000000000009);
        wsfloat_t tmp_418 = smoothed_var_37;
        wsfloat_t tmp_420 = (tmp_418 * (wsfloat_t)0.999);
        wsfloat_t tmp_422 = (tmp_416 + tmp_420);
        smoothed_var_37 = tmp_422;
        wsunit_t tmp_432 = ((wsunit_t)0);
      }
      wsint_t tmp_388 = startup_34;
      wsbool_t tmp_390 = (tmp_388 > (wsint_t)0);
      if (tmp_390) {
        wsint_t tmp_392 = startup_34;
        wsint_t tmp_394 = (tmp_392 - (wsint_t)1);
        startup_34 = tmp_394;
        wsunit_t tmp_396 = ((wsunit_t)0);
      } else {
      }
      wsint_t tmp_380 = WSPrim::end(win_44);
      wsint_t tmp_382 = (tmp_380 - (wsint_t)2400);
      wsint_t tmp_384 = max((wsint_t)0, tmp_382);
      tuptyp_460 tmp_386 = tuptyp_460(FALSE, (wsint_t)0, tmp_384);
      emit((tuptyp_460)tmp_386);
      wsunit_t tmp_434 = ((wsunit_t)0);
    }
    return FALSE;
  } 
} 
;

class Iter_positives_47 : public WSBox {
  public:
  DEFINE_OUTPUT_TYPE(tuptyp_460);
  
  Iter_positives_47() {
  } 
  
  private:
  
  /* WaveScript input type: (Struct tuptyp_460) */
  bool iterate(uint32_t portnum, void* datum) {
    /* Naturalize input type to meet expectations of the iterate-body. */
    tuptyp_460 x_49 = *((tuptyp_460*) datum);
    void* ___VIRTQUEUE____48 = (void*)0;
    wsbool_t b_50 = (x_49.fld1);
    if (b_50) {
      emit((tuptyp_460)x_49);
      wsunit_t tmp_440 = ((wsunit_t)0);
    } else {
    }
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
    tuptyp_460 *element = (tuptyp_460 *)input;
    if(WSOUTPUT_PREFIX) printf("WSOUT: ");
    printf("%s", show_tuptyp_460((*element)).c_str());
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
  WSSource* S_2 = new WSDataFileSource_462(string("6sec_marmot_sample.raw"), string("binary"), (wsint_t)0);
  WSBox* _ch1_1 = new Iter__ch1_1();
  _ch1_1->connect(S_2);
  WSBox* rw1_8 = new Iter_rw1_8();
  rw1_8->connect(_ch1_1);
  WSBox* hn_14 = new Iter_hn_14();
  hn_14->connect(rw1_8);
  WSBox* freq_22 = new Iter_freq_22();
  freq_22->connect(hn_14);
  WSBox* wscores_25 = new Iter_wscores_25();
  wscores_25->connect(freq_22);
  WSBox* detections_31 = new Iter_detections_31();
  detections_31->connect(wscores_25);
  WSBox* positives_47 = new Iter_positives_47();
  positives_47->connect(detections_31);
  WSBox* toplevel = positives_47;

  /* dump output of query -- WaveScript type = (Stream (Struct tuptyp_460)) */
  PrintQueryOutput out = PrintQueryOutput("WSOUT");
  out.connect(toplevel);

  /* now, run */
  WSRun();

  return 0;
}

