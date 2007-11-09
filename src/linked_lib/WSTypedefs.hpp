
using boost::enable_shared_from_this;
using namespace std;
using namespace __gnu_cxx;

#define TRUE 1
#define FALSE 0

#define WSNULL 0
#define WSNULLSEG (RawSeg::NullRef)
#define WSNULLTIMEBASE 0



/*
 * FIXME: copied from WaveScope's WSBox.hpp,
 *        with getTotalByteSize() pulled out;
 *        this is definitely a big, but for now
 *        very convenient, hack.
 */

#if defined(DEPTH_FIRST)
// Goes into the graph as far as it can

#define DEFINE_OUTPUT_TYPE(type)                \
  inline void emit(const type &tuple) {         \
    uint i;                                     \
    for (i=0; i<m_outputs.size(); i++) { \
      m_outputs[i]->getParent()->running();\
      m_outputs[i]->getParent()->iterate(m_outputs[i]->port(), \
                                         const_cast<type *>(&tuple)); \
      m_outputs[i]->getParent()->finished(); \
    } \
    totalEmits++; \
  } \
  void freeTuple(void *tuple) { \
  type *tuplePtr = (type *)tuple; \
  delete tuplePtr; \
  }

// ^^^ No need to free the tuple, the tuple is allocated by the

//                                       const_cast<type *>(&tuple)); 

#elif defined(MEMALLOC)

// Default emit()

#define DEFINE_OUTPUT_TYPE(type)                \
  MemAlloc<type> tupleAlloc; \
  inline void emit(const type &tuple) {         \
    uint i;                                     \
    for (i=0; i<m_outputs.size(); i++) {        \
      m_outputs[i]->enqueue(tupleAlloc.alloc(tuple), \
                            this);  \
    } \
    totalEmits++; \
  } \
  void *cloneTuple(void *tuple) { \
  type *tuplePtr = (type *)tuple; \
  type *outPtr = (new type (*tuplePtr)); \
  return outPtr; \
  } \
  void freeTuple(void *tuple) {\
  type *tuplePtr = (type *)tuple; \
  delete tuplePtr; \
  } \
  void clearOutputBuffer() \
  { }

#elif defined(COREFIT_SCHEDULER_DF) 

#ifdef CACHE_FIT_SORT
// Defined CACHE_FIT_SORT

       // Sample the L1 cache misses here

// Sample the L1 cache missed here
// Calculate the difference
//     l1missStats[m_outputs[i]->port()]->sample(difference)

#define DEFINE_OUTPUT_TYPE(type)                \
 inline void emit(const type &tuple) {         \
   uint i, j;                                     \
    outputStats.sample(getTotalByteSize(tuple)); \
    for (j=0; j<m_outputs.size(); j++) { \
      i = outputOrder[j]; \
      WSBox *nextBox = m_outputs[i]->getParent(); \
     if (nextBox->getPartition() == partition &&  \
         !inCycle(nextBox)) {                           \
       nextBox->prevScheduled = this;                           \
       nextBox->iterate(m_outputs[i]->port(),                   \
                        const_cast<type *>(&tuple));    \
      } else { \
          m_outputs[i]->enqueue(new type (tuple), this);  \
          m_outputs[i]->commit(this);                     \
          nextBox->getPartition()->scheduleActive(nextBox);     \
      } \
    } \
    totalEmits++;                               \
    misc++;                                     \
    totalEmitBytes += getTotalByteSize(tuple); \
  } \
  void *cloneTuple(void *tuple) { \
  type *tuplePtr = (type *)tuple; \
  type *outPtr = (new type (*tuplePtr)); \
  return outPtr; \
  } \
  void freeTuple(void *tuple) {\
  type *tuplePtr = (type *)tuple; \
  delete tuplePtr; \
  } \
  void clearOutputBuffer() \
  { }

#elif defined(CACHE_FIT_RADIUS)
// Defined CACHE_FIT_RADIUS

#define DEFINE_OUTPUT_TYPE(type)                \
 inline void emit(const type &tuple) {         \
   uint i;                                        \
   uint outputSize = getTotalByteSize(tuple); \
   PAPI_read(partition->papiEventSet,                   \
             partition->papiCtrValues);                 \
   long_long temp = (partition->papiCtrValues[0] + \
                     partition->papiCtrValues[1]); \
   uint footPrintSize = (temp - cacheReuse.cacheMisses) * \
     WSSystem::L1DCacheLine(); \
   cacheReuse.cacheMisses = temp; \
   if (outputSize > cacheReuse.outpSizeEst) \
     cacheReuse.outpSizeEst = outputSize; \
   cacheReuse.mult = m_outputs.size(); \
   cacheReuse.outpSize = outputSize; \
   cacheReuse.opFootprint = footPrintSize; \
   if (cacheReuse.opFootprint > cacheReuse.opFootprintEst) \
     cacheReuse.opFootprintEst = cacheReuse.opFootprint; \
   if (prevScheduled != NULL)                            \
     cacheReuse.cacheFill = prevScheduled->cacheReuse.cacheFill + \
       cacheReuse.opFootprint; \
   else \
     cacheReuse.cacheFill = cacheReuse.opFootprint; \
   for (i=0; i<m_outputs.size(); i++, cacheReuse.mult--) {   \
      WSBox *nextBox = m_outputs[i]->getParent(); \
     if (!doPostpone(nextBox) && \
         nextBox->getPartition() == partition &&        \
         !inCycle(nextBox)) {                           \
       nextBox->prevScheduled = this;                           \
       nextBox->iterate(m_outputs[i]->port(),                   \
                        const_cast<type *>(&tuple));    \
      } else { \
          m_outputs[i]->enqueue(new type (tuple), this);  \
          m_outputs[i]->commit(this);                     \
          nextBox->getPartition()->scheduleActive(nextBox);     \
      } \
    } \
    totalEmits++;                               \
    misc++;                                     \
    totalEmitBytes += getTotalByteSize(tuple); \
  } \
  void *cloneTuple(void *tuple) { \
  type *tuplePtr = (type *)tuple; \
  type *outPtr = (new type (*tuplePtr)); \
  return outPtr; \
  } \
  void freeTuple(void *tuple) {\
  type *tuplePtr = (type *)tuple; \
  delete tuplePtr; \
  } \
  void clearOutputBuffer() \
  { }


#else

#define DEFINE_OUTPUT_TYPE(type)                \
 inline void emit(const type &tuple) {         \
    uint i;                                     \
    for (i=0; i<m_outputs.size(); i++) { \
      WSBox *nextBox = m_outputs[i]->getParent(); \
     if (nextBox->getPartition() == partition &&  \
         !inCycle(nextBox)) {                           \
       nextBox->prevScheduled = this;                           \
       nextBox->iterate(m_outputs[i]->port(),                   \
                        const_cast<type *>(&tuple));    \
      } else { \
          m_outputs[i]->enqueue(new type (tuple), this);  \
          m_outputs[i]->commit(this);                     \
          nextBox->getPartition()->scheduleActive(nextBox);     \
      } \
    } \
    totalEmits++;                               \
    misc++;                                     \
    totalEmitBytes += getTotalByteSize(tuple); \
  } \
  void *cloneTuple(void *tuple) { \
  type *tuplePtr = (type *)tuple; \
  type *outPtr = (new type (*tuplePtr)); \
  return outPtr; \
  } \
  void freeTuple(void *tuple) {\
  type *tuplePtr = (type *)tuple; \
  delete tuplePtr; \
  } \
  void clearOutputBuffer() \
  { }

#endif
// #ifdef CACHE_FIT_SORT, not defined

#elif defined(COREFIT_SCHEDULER_EX)
// Default emit() + bytewise accounting

#ifdef LONG_CALL
#define DEFINE_OUTPUT_TYPE(type)                \
 inline void emit(const type &tuple) {         \
    uint i;                                     \
    for (i=0; i<m_outputs.size(); i++) {        \
          m_outputs[i]->enqueue(new type (tuple), this);  \
    } \
    totalEmits++; \
    misc++;\
    totalEmitBytes += getTotalByteSize(tuple); \
  } \
  void *cloneTuple(void *tuple) { \
  type *tuplePtr = (type *)tuple; \
  type *outPtr = (new type (*tuplePtr)); \
  return outPtr; \
  } \
  void freeTuple(void *tuple) {\
  type *tuplePtr = (type *)tuple; \
  delete tuplePtr; \
  } \
  void clearOutputBuffer() \
  { }

#else
// Not defined(LONG_CALL)
#define DEFINE_OUTPUT_TYPE(type)                \
 inline void emit(const type &tuple) {         \
    uint i;                                     \
    for (i=0; i<m_outputs.size(); i++) {        \
      if (m_outputs[i]->getParent()->getPartition() == \
          partition) { \
          m_outputs[i]->enqueue_direct(new type (tuple)); \
      } else { \
          m_outputs[i]->enqueue(new type (tuple), this);  \
      } \
    } \
    totalEmits++; \
    misc++;\
      totalEmitBytes += getTotalByteSize(tuple);       \
  } \
  void *cloneTuple(void *tuple) { \
  type *tuplePtr = (type *)tuple; \
  type *outPtr = (new type (*tuplePtr)); \
  return outPtr; \
  } \
  void freeTuple(void *tuple) {\
  type *tuplePtr = (type *)tuple; \
  delete tuplePtr; \
  } \
  void clearOutputBuffer() \
  { }

#endif

#elif defined(COREFIT_SCHEDULER)
// Default emit() + bytewise accounting

#define DEFINE_OUTPUT_TYPE(type)                \
  inline void emit(const type &tuple) {         \
    type *outputStorage = (type *)outputStorageBuf;\
    type *placement = \
    new(&outputStorage[outputStorageIdx++]) type(tuple);     \
    outputStorageUse += sizeof(type); \
    outputBuffer.push_back(placement); \
    totalEmits++; \
    misc++;\
    totalEmitBytes += m_outputs.size() * \
                      getTotalByteSize(tuple); \
  } \
  void *cloneTuple(void *tuple) { \
  type *tuplePtr = (type *)tuple; \
  type *outPtr = (new type (*tuplePtr)); \
  return outPtr; \
  } \
  void freeTuple(void *tuple) {\
  type *tuplePtr = (type *)tuple; \
  delete tuplePtr; \
                     } \
  void clearOutputBuffer() \
  { type *outputStorage = (type *)outputStorageBuf; \
    for(uint32_t i=0;i<outputStorageIdx;i++) outputStorage[i].~type(); outputStorageIdx=outputStorageUse=0; outputBuffer.clear();} 

#else
// Default emit()

#define DEFINE_OUTPUT_TYPE(type)                \
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

#endif



typedef int16_t         wsint16_t;
typedef uint16_t        wsuint16_t;
typedef int64_t         wsint64_t;
typedef int             wsint_t;
typedef float           wsfloat_t;
typedef double          wsdouble_t;
typedef bool            wsbool_t;
typedef string          wsstring_t;
typedef char            wschar_t;
typedef _Complex float  wscomplex_t;

typedef wsbool_t        wsunit_t;


bool wsequal(wsint_t x,     wsint_t y)     { return x==y; }
bool wsequal(wsint16_t x,   wsint16_t y)   { return x==y; }
bool wsequal(wsint64_t x,   wsint64_t y)   { return x==y; }
//bool wsequal(wsbool_t x,    wsbool_t y)    { return x==y; }
bool wsequal(wsfloat_t x,   wsfloat_t y)   { return x==y; }
bool wsequal(wsdouble_t x, wsdouble_t y)   { return x==y; }
bool wsequal(wscomplex_t x, wscomplex_t y) { return x==y; }
bool wsequal(wsstring_t x,  wsstring_t y)  { return x==y; }

template <class T>
bool wsequal(const SigSeg<T>& x, const SigSeg<T>& y)  { return x==y; }

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
#ifndef BOOST_SP_DISABLE_THREADS
  //Mutex mut;
#endif
  T* data;
};

template <class T>
uint32_t getTotalByteSize(const boost::intrusive_ptr< WSArrayStruct<T> > &e);

template <class T>
boost::intrusive_ptr< WSArrayStruct<T> >
makeArrayUnsafe(wsint_t count, T initelem);

template <class T> 
boost::intrusive_ptr< WSArrayStruct<T> > makeArray(wsint_t count, T initelem);

template <class T> void intrusive_ptr_add_ref(WSArrayStruct<T>* p);
template <class T> void intrusive_ptr_release(WSArrayStruct<T>* p);

template <class T> 
bool wsequal (const boost::intrusive_ptr< WSArrayStruct<T> > arr1,
              const boost::intrusive_ptr< WSArrayStruct<T> > arr2);

/******** END ARRAYS ********/

