#ifndef EVENT_H_
#define EVENT_H_

#define EventPtr        Event*

#include "Field.h"
#include "EventType.h"

class Event {
 public:
  char* _values;    // network communication sending characters 
  size_t _sz;
  
 public:  
  /*
   * Constructor
   * @param
   *  ts: sequential time stamp
   *  etptr: event type
   *  values: binary format contents of event
   */
  Event(const char* values, size_t sz) {
    _sz = sz;
    _values = (char*)malloc(_sz);
    memcpy(_values, values, _sz);
  }
 

  /*
   * Constructor
   * @param
   *  etptr: event type
   *  values: ascii format contents of event
   */
  Event(EventType* etptr, string* values) {   
    
    _sz = etptr->bytes();
    _values = (char*)malloc(_sz);
    memset(_values, 0, _sz);

    int noFields = etptr->fieldsno();
    size_t offset = 0;

    for(int i=0; i<noFields; i++) {      
      Field::convert_a(etptr->fieldtype(i), _values+offset, values[i]);
      offset += etptr->fieldtype(i)->bytes();
    }
  }

  Event(const Event& e) {
    _sz = e._sz;
    _values = (char*)malloc(_sz);
    memcpy(_values, e._values, _sz);    
  }

  /*
   * Constructor
   * @param
   *  etptr: event type
   *  construct an empty event
   */
  Event(EventType* etptr) {
    _sz = etptr->bytes();
    _values = (char*)malloc(_sz);
    memset(_values, 0, _sz);
  }  
  
  ~Event() {
    if (_values != 0) { free(_values); }
  }
  
  // copy the v to the _values
  void copy(char* v) { memcpy(_values, v, _sz); }
  void copy(EventPtr ep) { memcpy(_values, ep->_values, _sz); }
  
  //EventType* type() { return _etypeptr; }	
  //int fieldsno() { return _etypeptr->fieldsno(); }
  char* field(int index, EventType* type) { 
    size_t offset = 0;
    for(int i=0; i<index; i++)
      offset += type->fieldtype(i)->bytes();
    return _values + offset; 
  }
  
  
  string tostring(EventType* etptr) {
    string s = "[";
    size_t offset = 0;

    int noFields = etptr->fieldsno();
    s += Field::tostring(etptr->fieldtype(0), _values+offset);
    offset += etptr->fieldtype(0)->bytes();	

    for(int i=1; i<noFields; i++) {  
      s += "," + Field::tostring(etptr->fieldtype(i), _values+offset);
      offset += etptr->fieldtype(i)->bytes();	        
    }
    return s +"]";
  }

  char* values() { return _values; }
  size_t sz() { return _sz; }

  void print(ostream& os, EventType* etptr) { os << tostring(etptr) << endl; }
  
};

#endif /*EVENT_H_*/
