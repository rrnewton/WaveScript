#ifndef ESBOX_H_
#define ESBOX_H_

#include "EventStream.h"
#include "ESBuffer.h"
#include "Catalog.h"

enum Btype { SRC, FILTER, CLIENT };
class Catalog;

/* ESBox: Base Class for all boxes */
//template<class BufferT, class IteratorT, class T>
class ESBox {	
 protected:
  int _index;
  Btype _type;
  bool _open; 

  EventType* _inetptr;    // read in Event Type 
  EventType* _outetptr;   // output Event Type

  ESBuffer* _sockbuffer;
  bool _ndbuffer;

 public:
  ESBox(int index, Btype type, EventType* intype, EventType* outtype) { 
    _index = index; _type = type; _open = false; 
    _inetptr = intype; 
    _outetptr = outtype;
    _sockbuffer = 0;
    _ndbuffer = false;
  }	

  virtual ~ESBox() {
    if(_sockbuffer != 0) delete _sockbuffer;     
  }
  
  virtual bool open(Catalog* cat) { 
    if(!_open) { _open = true; cat->addoperator(_index, this); return true; }
    return false;
  }   
  virtual void close(Catalog* cat) {
    if(_open) {_open = false; cat->eraseoperator(_index); }    
  }

  void setsockbuffer() {
    _ndbuffer = true;
    if(_sockbuffer == 0) _sockbuffer = new ESBuffer(_outetptr->bytes());
  }

  void closesockbuffer() {
    _ndbuffer = false;    
    _sockbuffer->reset();
  }
  
  bool needbuffer() { return _ndbuffer; }

  virtual EventPtr next() { return 0; };
  virtual bool hasNext() { return false; }

  Btype type() { return _type; }  
  EventType*& inetptr() { return _inetptr; }
  EventType*& outetptr() { return _outetptr; }
  ESBuffer* sockbuffer() { return _sockbuffer; }
  
};

#endif /*ESBOX_H_*/

