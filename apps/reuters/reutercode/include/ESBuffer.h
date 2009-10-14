#ifndef ESBUFFER_H_
#define ESBUFFER_H_


#define Esrc_iter    vector<EventPtr>::iterator

#include <fstream>
#include <string>
#include <vector>
#include "Event.h"

class ESBuffer {
 private:
  vector<EventPtr> _eventQueue;

  int _curr;
  
 public:
  ESBuffer() { _curr = 0; }
  ~ESBuffer() {
    for(Esrc_iter i = _eventQueue.begin(); i != _eventQueue.end(); i++) 
      delete (*i);
  }

  EventPtr next(){ return _eventQueue[_curr++]; }
  bool hasNext() { return (_eventQueue.begin() + _curr) != _eventQueue.end(); }
  void push_back(EventPtr& eptr) { _eventQueue.push_back(eptr); }
  
  void print(ostream& os, EventType* etptr) {
    os << "Source Events Begin: [ \n";
    for(Esrc_iter i = _eventQueue.begin(); i != _eventQueue.end(); i++) {
      os << "address : " + _to_string(*i) + "\t";
      (*i)->print(cout, etptr);
    }
    os << " ] Source Events End \n";
  }
};

#endif /*ESBUFFER_H_*/
