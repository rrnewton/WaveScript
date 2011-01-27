#ifndef ESBUFFER_H_
#define ESBUFFER_H_


#define Esrc_iter    vector<EventPtr>::iterator

#include <fstream>
#include <string>
#include <vector>
#include "Event.h"
#include "Thread.h"

/* This version is using simple growing vector
*/

// no need to lock, because of the _curr <-> _end schema

class ESBuffer {
 private:
  vector<EventPtr> _eventQueue;
  pthread_mutex_t _buffer_mutex;    // buffer mutex

  int _curr, _end;

  //int curdebug, enddebug;
  

  // other parameter
  int buffersize;
  size_t sz;  
  
 public:
  ESBuffer(size_t size) { 
    _curr = 0; _end = 0; buffersize = BUFFERSIZE; sz = size; 
    //curdebug = 0; enddebug = 0;

    init();
  }
  ESBuffer(size_t size, int bs) { 
    _curr = 0; _end = 0; buffersize = bs; sz = size; 
    init();
  }  
  
  ~ESBuffer() {
    pthread_mutex_destroy(&_buffer_mutex);
    for(Esrc_iter i = _eventQueue.begin(); i != _eventQueue.end(); i++) 
      delete (*i);
  }

  // pre-allocate the buffer
  void init() {
    pthread_mutex_init(&_buffer_mutex, NULL);
    for(int i=0; i<buffersize; i++) {
      EventPtr eptr = new Event(sz);
      _eventQueue.push_back(eptr);
    }
  }

  EventPtr next(){
    //printf("curr: %d\n", curdebug++);
    EventPtr ep = _eventQueue[_curr]; 
    increment(_curr);
    return ep;
  }
  
  //  void push_back(EventPtr& eptr) { _eventQueue.push_back(eptr); }
  void add(char* v) {
    //    printf("end: %d\n", enddebug++);
   _eventQueue[_end]->copy(v); increment(_end); 
  }

  bool requrielock() {
    if(pthread_mutex_lock(&_buffer_mutex) == 0) return true;
    else return false;
  }

  bool releaselock() {
    if(pthread_mutex_unlock(&_buffer_mutex) == 0) return true;
    else return false;
  }

  // % buffersize because we do not want to lock
  bool hasNext() { return _curr != _end % buffersize; }
  bool full() { return (_end + 1) % buffersize == _curr % buffersize; }  
  void reset() { _curr = 0; _end = 0; }
  
  void print(ostream& os, EventType* etptr) {
    os << "Source Events Begin: [ \n";
    for(Esrc_iter i = _eventQueue.begin(); i != _eventQueue.end(); i++) {
      os << "address : " + _to_string(*i) + "\t";
      (*i)->print(cout, etptr);
    }
    os << " ] Source Events End \n";
  }

 private:
  void increment(int& index) { index++; index %= buffersize; }
    
};

#endif /*ESBUFFER_H_*/
