#ifndef RECBUFFER_H_
#define RECBUFFER_H_

#include "ESBox.h"

class RecBuffer : public ESBox {

  FPredList* _flist; 
  bool _neg;  // whether it is a neg buffer or not.

 public:  	
  RecBuffer() : ESBox(BUFFER){ _flist = NULL; _neg = false;}

  bool open() {
    if (!_open) { _open = true; return true; }
    return false; 
  }

  void close() { _open = false; }

  Rec_iter next(uint sqno) { 
    for(Rec_iter iter = _buffer->begin(); iter != _buffer->end(); )
      if((*iter)->_start >= sqno) { return iter; }
      else { iter = _buffer->erase(iter); }
    return _buffer->begin(); 
  }    

  // the right one
  void push_back(Event* eventptr) {
    if(_flist == NULL || _flist->filter(eventptr)) {
      Rec* recptr = new Rec(eventptr);
      _buffer->push_back(recptr);	
    }
  } 

  // neg one temporally
  /*void push_back(Event* eventptr) {
    if(_flist == NULL || _flist->filter(eventptr)) {

      if (_neg) {
	if (_buffer->begin() == _buffer->end()){
	  Rec* recptr = new Rec(eventptr);
	  _buffer->push_back(recptr);
	}
	else {
	  Rec_iter iter = _buffer->begin();
	  (*iter)->cp(eventptr);
	}	  
      }

      else {
	Rec* recptr = new Rec(eventptr);
	_buffer->push_back(recptr);	
      }
    }
    }*/
    
  void setflist(FPredList* flist) { _flist = flist; }
  void setneg(bool neg) {_neg = neg; }
  int startsqno() {
    if (_buffer->empty()) return -1;
    else return (*_buffer->begin())->_start;
  }
};
#endif /*RECBUFFER_H_*/
