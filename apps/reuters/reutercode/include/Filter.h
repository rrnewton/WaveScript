#ifndef FILTER_H_
#define FILTER_H_

#include "ESBox.h"
#include "Predicate.h"

// no buffer needed
//class Filter : public ESBox<Event> {
class Filter : public ESBox {

  ESBox* _child;
  FPredList* _flist;    // predicate list

  EventPtr _teptr;       // temporally store the next event

  bool nextflag;

 public:
  Filter(int index, EventType* etptr, ESBox* child) 
    : ESBox(index, FILTER, etptr, etptr) { 
    _child = child; _flist = 0;
    _teptr = new Event(etptr);

    nextflag = false;
  }
  
  ~Filter() { delete _teptr; }

  bool open(Catalog* cat) {
    if(!_open) {
      if(_child->open(cat)) { 
	_open = true; nextflag = false;
	cat->addoperator(_index, this);
	return true;
      }
    }
    return false;  
  }

  void close(Catalog* cat) { 
    if(_open) { 
      _child->close(cat); 
      _open = false; 
      cat->eraseoperator(_index); }    
  }

  bool hasNext() {
    if(nextflag) return true;
    while(_child->hasNext()) {
      EventPtr cptr = _child->next();
      if(_flist == 0 || _flist->filter(cptr, _inetptr)) {
	_teptr->copy(cptr); nextflag = true; return true;
      }
    }
    return false;
  }

  EventPtr next() { 
    if(_ndbuffer && _sockbuffer->full()) {
      printf("filter sockbuffer is full, hold on.....\n"); return 0; 
    }

    nextflag = false; 
    if(_ndbuffer) {
      _sockbuffer->add(_teptr->values());	
    }
    return _teptr; 
  }
  
  void setflist(FPredList* flist) { _flist = flist; } 
};
#endif /*FILTER_H_*/
