#ifndef PREDICATE_H_
#define PREDICATE_H_

#define FPredListPtr	FPredList*

#include <vector>
#include <string>
#include <deque>
#include "EventStream.h"

bool _op_compare(char* v1, Operator op, char* v2, FieldType* ftypeptr) {  

  int res = Field::compare(v1, v2, ftypeptr);	
  if (op == EQUAL) {
    if (res == 0) return true;
    else return false;
  }
  else if (op == GREATER_THAN) {
    if (res > 0) return true;
    else return false;
  }	
  else if (op == LESS_THAN) {
    if (res < 0) return true;
    else return false;
  }
  else if (op == GREATER_EQUAL) {
    if (res >= 0) return true;
    else return false;
  }
  else if (op == LESS_EQUAL) {
    if (res <= 0) return true;
    else return false;
  }
  else {
    fprintf(stderr, "unknown operators \n");
    return false;
  }
}

/*bool _op_compare(char* v1, Operator op, char* v2, AOperator aop, char* v3, FieldType* ftypeptr) {
  if(aop == ANULL) return _op_compare(v1, op, v2, ftypeptr);
  int res = Field::compare(v1, v2, aop, v3, ftypeptr);
  if (op == EQUAL) {
    if (res == 0) return true;
    else return false;
  }
  else if (op == GREATER_THAN) {
    if (res > 0) return true;
    else return false;
  }
  else if (op == LESS_THAN) {
    if (res < 0) return true;
    else return false;
  }
  else if (op == GREATER_EQUAL) {
    if (res >= 0) return true;
    else return false;
  }
  else if (op == LESS_EQUAL) {
    if (res <= 0) return true;
    else return false;
  }
  else {
    fprintf(stderr, "unknown operators \n");
    return false;
  }
  }*/

string op_tostring(Operator op) {
  return ( op == EQUAL			? 	"=="		:
           op == GREATER_THAN		?	">"		:
	   op == LESS_THAN	 	?	"<"		:
	   op == GREATER_EQUAL		?	">="		:
	   op == LESS_EQUAL		?	"<="		:
	   op == CONTAINS		?	"/"		:
	                                        "unknown op");	 
}

string aop_tostring(AOperator aop) {
  return ( aop == PLUS			?	"+"		:
	   aop == SUB			?       "-"		:
	   aop == MUL			?	"*"		:
	   aop == DIV			?	"/"		:
	   aop == ANULL			?       "NULL"		:
	                                        "unknown aop");
}

struct FPred {
  char* _v;	       // the field to compare against
  FieldType* _ftype;   // filed type
  Operator _op;	       // operator
  int _atrIdx; 	       // attribute of the event

  //string* _s;          // for sub-string comparison
  
  FPred(FieldType* ftype, const char* fvalue, Operator op, int atrIdx) {
    _ftype = ftype; _op = op; _atrIdx = atrIdx;

    size_t sz = ftype->bytes();
    _v = (char*)malloc(sz);
    memset(_v, 0, sz); 
    memcpy(_v, fvalue, strlen(fvalue) < sz ? strlen(fvalue) : sz);    
    //_s = NULL;
  }
  
  /*FPred(const FPred& pred) {
    _fptr = new Field(*(pred._fptr));
    _op = pred._op;
    _atrIdx = pred._atrIdx;
    _s = new string(*(pred._s));
    }*/

  // detecting substring
  /*FPred(const char* s, Operator op, int atrIdx) {
    ; _s = new string(s);
    _op = op;
    _atrIdx = atrIdx;

    _v = NULL;    
    }*/
	
  ~FPred() { 
    if (_v != NULL) free(_v); 
    //    if (_s != NULL) delete _s; 
  }
	
  bool filter(Event* e, EventType* etype) { 
    if (_op == CONTAINS) 
      return Field::contains(e->field(_atrIdx, etype), _v, _ftype);
    else 
      return _op_compare(e->field(_atrIdx, etype), _op, _v, _ftype); 
  }
	
  string tostring() {
    string s = "compare " + _to_string(_atrIdx) + "th attribute with operator " 
      + op_tostring(_op) + " against Field: [ ";
    s = s + Field::tostring(_ftype, _v) + " ]";		
    return s;		
  }
};

class FPredList {
  vector<FPred*> _fpreds;		// filters the event should pass
  
 public:
  FPredList() {};	
  FPredList(const FPredList& list) {
    for(uint i=0; i<list.size(); i++)	{
      FPred* pred = new FPred(*(list._fpreds[i]));
      _fpreds.push_back(pred);
    }
  }	
		
  void add_fpred(FieldType* ftype, const char* fvalue, Operator op, int atrIdx) {
    FPred* pred = new FPred(ftype, fvalue, op, atrIdx);
    _fpreds.push_back(pred);
  }


  // for substring comprison 
  /*void add_fpred(const char* fvalue, Operator op, int atrIdx) {
    FPred* pred = new FPred(fvalue, op, atrIdx);
    _fpreds.push_back(pred);
    }*/
  
  ~FPredList() {
    for(uint i=0; i<_fpreds.size(); i++) 
      if (_fpreds[i] != NULL)
	delete _fpreds[i];


  }
	
  bool filter(Event* e, EventType* etype) {		
    for(uint i=0; i<_fpreds.size(); i++) 
      if (!_fpreds[i]->filter(e, etype)) 
	return false;
    return true;
  }
  
  uint size() const { return _fpreds.size(); }	
  string tostring() {
    string s;
    for(uint i=0; i<_fpreds.size(); i++)
      s = s + _fpreds[i]->tostring() + "\n";
    return s;
  }
};

/*
struct Opr {
  int _evIdx;  int _atrIdx;
  
  Opr(){ _evIdx = -1; _atrIdx = -1; }
  Opr(int evIdx, int atrIdx) { _evIdx = evIdx; _atrIdx = atrIdx; }
  Opr(const Opr& opr) { _evIdx = opr._evIdx; _atrIdx = opr._atrIdx; }
  
};

struct PPred {
  Field* _fptr;	    // arithmetic operand if needed
  AOperator _aop;   // arithmetic opator if needed
  Operator _op; 	// operator
  Opr _opr1;		// left operand
  Opr _opr2;		// right operand
  
  PPred(Opr opr1, Operator op, Opr opr2) {
    _opr1 = opr1; _opr2 = opr2; _op = op;
    _fptr = NULL; _aop = ANULL;
  }
  
  PPred(Opr opr1, Operator op, Opr opr2, FieldType* ftype, const char* fvalue, AOperator aop) {
    _opr1 = opr1; _opr2 = opr2; _op = op; 
    _fptr = new Field(ftype, fvalue); _aop = aop;
  }
	
  PPred(const PPred& pred) {
    _opr1 = pred._opr1; _opr2 = pred._opr2; _op = pred._op;
    if(pred._fptr == NULL) _fptr = NULL;
    else _fptr = new Field(*(pred._fptr));
    _aop = pred._aop;
  }
  
  ~PPred() { if (_fptr!= NULL) delete _fptr; }	
  bool filter(Events& e1, Events& e2) { 
    return _op_compare(e1[_opr1._evIdx]->field(_opr1._atrIdx), _op, 
		       e2[_opr2._evIdx]->field(_opr2._atrIdx), _aop, _fptr); 
  }
	
  string tostring() {
    string s = "compare " + _to_string(_opr1._evIdx) + "." + _to_string(_opr1._atrIdx) 
      + "th attribute with operator " + op_tostring(_op) + " against " + _to_string(_opr2._evIdx) 
      + "." + _to_string(_opr2._atrIdx) + " " + aop_tostring(_aop) + " ";
    
    if (_fptr != NULL) s = s + _fptr->tostring();		
    return s;		
  }
};

class PPredList {
  vector<PPred*> _ppreds;		// correlation between events
  
 public:
  PPredList() {};	
  PPredList(const PPredList& list) {
    for(uint i=0; i<list.size(); i++)	{
      PPred* pred = new PPred(*(list._ppreds[i]));
      _ppreds.push_back(pred);
    }
  }	
  
  void add_ppred(Opr opr1, Operator op, Opr opr2) {
    PPred* pred = new PPred(opr1, op, opr2);
    _ppreds.push_back(pred);
  }
  
  void add_ppred(Opr opr1, Operator op, Opr opr2, FieldType* ftype, const char* fvalue, AOperator aop) {
    PPred* pred = new PPred(opr1, op, opr2, ftype, fvalue, aop);
    _ppreds.push_back(pred);
  }
  
  ~PPredList() {
    for(uint i=0; i<_ppreds.size(); i++)
      if (_ppreds[i] != NULL)
	delete _ppreds[i];
  }
  
  bool filter(Events* e1, Events* e2) {		
    for(uint i=0; i<_ppreds.size(); i++) 
      if (!_ppreds[i]->filter(*e1, *e2)) 
	return false;
    return true;
  }
  
  uint size() const { return _ppreds.size(); }	
  
  string tostring() {
    string s;
    for(uint i=0; i<_ppreds.size(); i++)
      s = s + _ppreds[i]->tostring() + "\n";
    return s;
  }
  };*/

#endif /*PREDICATE_H_*/
