#ifndef MLJOIN_H_
#define MLJOIN_H_

#include "ESBox.h"

class MLJoin : public ESBox {
  ESBox* _left;
  ESBox* _right;
  PPredList* _plist; 
  
 public:
  MLJoin(ESBox* left, ESBox* right) : ESBox(MLJOIN) {
    _left = left; _right = right; _plist = NULL;  
  }
    
  MLJoin(ESBox* left, ESBox* right, PPredList* plist) : ESBox(MLJOIN) {
    _left = left; _right = right; _plist = plist;
  }

  ESBox* left() { return _left; }
  ESBox* right() { return _right; }
  
  bool open() {
    if (!_open && _left->open() && _right->open()) {
      _open = true; return true; 
    }
    return false;
  }
  
  void close() { _left->close(); _right->close(); _open = false; }
  
  Rec_iter next(uint sqno) {
    Rec_iter leftiter = _left->next(sqno);
    Rec_iter rightiter = _right->next(sqno);	   

    // clear the old buffer, not benefit much
    /*for(Rec_iter iter = _buffer->begin(); iter != _buffer->end();) {
      if((*iter)->_start < sqno) { iter = _buffer->erase(iter); }
      else { iter++; }
      }*/

    // for every right tuple, loop through left tables to keep time order.
    for(Rec_iter riter = rightiter; riter != _right->end(); riter++) {
      for(Rec_iter liter = leftiter; liter != _left->end();) {

	if((*liter)->_end >= (*riter)->_start) break; 

	// compared to sqno: the earliest time allowed in this bunch, not for each combined tuple
	if((*liter)->_start < sqno) { 
	  liter = _left->erase(liter);
	  continue;
	}

	if(_plist == NULL || _plist->filter((*liter)->_eventsptr, (*riter)->_eventsptr)) {
	  Rec* rec = new Rec((*liter)->_eventsptr, (*riter)->_eventsptr);
	  _buffer->push_back(rec);
	}
	liter++;
      }
    }

    _right->clear();

    for(Rec_iter iter = _buffer->begin(); iter != _buffer->end();) {
      if((*iter)->_start < sqno) { iter = _buffer->erase(iter); }
      else { return iter; }
    }
    return _buffer->begin();
  }

  string nextstring() {
    string s = "MLJoin \n " + tostring();
    s = s + _left->nextstring();
    s = s + _right->nextstring();
    return s;
  }
};

#endif /*MLJOIN_H_*/
