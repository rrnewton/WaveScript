#ifndef FIELD_H_
#define FIELD_H_

#define FieldPtr Field*

#include "EventStream.h"
#include "FieldType.h"

using namespace std;

class Field {
public:
  FieldType* _ftypeptr;
  char* _v; 
	
public:
	
  Field(FieldType* ftype) {
    _ftypeptr = ftype;
    size_t tsize = _ftypeptr->bytes();
    _v = new char[tsize];
  }

  /*
   * Constructor
   * @param
   *  ftype: fieldtype
   *  fvalues: binary format contents of field
   */
  Field(FieldType* ftype, const char* fvalue) {
    _ftypeptr = ftype;		
    size_t tsize =  _ftypeptr->bytes();
    _v = new char[tsize];
    convert_b(_ftypeptr, _v, fvalue);
  }

  /*
   * Constructor
   * @param
   *  ftype: fieldtype
   *  fvalues: ascii format contents of field
   */
  Field(FieldType* ftype, const string& fvalue) {
    _ftypeptr = ftype;
    size_t tsize = _ftypeptr->bytes();
    _v = new char[tsize];		
    convert_a(_ftypeptr, _v, fvalue);
  }

  /* Copy Constructor */	
  Field(const Field& f) {
    _ftypeptr = f._ftypeptr;
    size_t tsize = _ftypeptr->bytes();
    _v = new char[tsize];
    convert_b(_ftypeptr, _v, f._v);
  }
  
  Field() { _ftypeptr = NULL; _v = NULL; }	
  ~Field() { if(_v != NULL)	 delete[] _v; }

  // binary copy, when _v != NULL;
  void copy(const char* fvalue) { convert_b(_ftypeptr, _v, fvalue); }  
  
  /* convert and copy raw value in binary format to v */
  static inline void convert_b(FieldType* ftypeptr, 
			       char* v, const char* fvalue) {
    if(*ftypeptr == INT) { *((int*)v)  = *((int*)fvalue); }
    else if(*ftypeptr == STRING) { memcpy(v, fvalue, ftypeptr->bytes()); }
    else if(*ftypeptr == FLOAT) { *((float*)v) = *((float*)fvalue); }
    else if(*ftypeptr == DOUBLE) { *((double*)v) = *((double*)fvalue); }
    else if(*ftypeptr == LONG) { *((long*)v) = *((long*)fvalue); }
    else { fprintf(stderr, "unknown types \n"); }    
  }

  /* convert and copy raw value in ascii format to v */
  static inline void convert_a(FieldType* ftypeptr, 
			       char* v, const string& fvalue) {
    if(*ftypeptr == INT) { *((int*)v)  = atoi(fvalue.c_str()); }
    else if(*ftypeptr == STRING) { 
      int len = strlen(fvalue.c_str());
      memcpy(v, fvalue.c_str(), len);
      v[len] = 0;     
    }
    else if(*ftypeptr == FLOAT) { *((float*)v) = atof(fvalue.c_str()); }
    else if(*ftypeptr == DOUBLE) { *((double*)v) = atof(fvalue.c_str()); }
    else if(*ftypeptr == LONG) { *((long*)v) = strtol(fvalue.c_str(), NULL, 10); } // base is 10
    else { fprintf(stderr, "unknown types \n"); }
  }

  static int compare(const char* v1, const char* v2, FieldType* ftypeptr) {
    if(*ftypeptr == INT) {
      if (*((int*)v1) == *((int*)v2))
	return 0;
      else
	return *((int*)v1) > *((int*)v2) ? 1 : -1;
    }
    else if(*ftypeptr == STRING)
      return string(v1).compare(v2);
    else if(*ftypeptr == FLOAT)
      return *((float*)v1) > *((float*)v2) ? 1 : -1;
    else if(*ftypeptr == DOUBLE)
      return *((double*)v1) > *((double*)v2) ? 1 : -1;
    else if(*ftypeptr == LONG)
      return *((long*)v1) > *((long*)v2) ? 1 : -1;
    else { fprintf(stderr, "unknown types \n"); return 0; }    
  }

  static int compare(const char* v1, const char* f1, AOperator aop, const char* f2,
		     FieldType* ftypeptr) {
    if(*ftypeptr == INT) {
      int temp = 0;
      if(aop == PLUS) temp = *((int*)(f1)) + *((int*)(f2));
      else if(aop == SUB) temp = *((int*)(f1)) - *((int*)(f2));
      else if(aop == MUL) temp = *((int*)(f1)) * *((int*)(f2));
      else if(aop == DIV) temp = *((int*)(f1)) / *((int*)(f2));
      else { fprintf(stderr, "unknown arith types \n"); return 0; }		
      
      if (*((int*)v1) == temp)
	return 0;
      else
	return *((int*)v1) > temp ? 1 : -1;
    }
    else if(*ftypeptr == FLOAT) {
      float temp = 0;
      if(aop == PLUS) temp = *((float*)(f1)) + *((float*)(f2));
      else if(aop == SUB) temp = *((float*)(f1)) - *((float*)(f2));
      else if(aop == MUL) temp = *((float*)(f1)) * *((float*)(f2));
      else if(aop == DIV) temp = *((float*)(f1)) / *((float*)(f2));
      else { fprintf(stderr, "unknown arith types \n"); return 0; }
      
      return *((float*)v1) > temp ? 1 : -1;
    }
    else if(*ftypeptr == DOUBLE) {
      double temp = 0;
      if(aop == PLUS) temp = *((double*)(f1)) + *((double*)(f2));
      else if(aop == SUB) temp = *((double*)(f1)) - *((double*)(f2));
      else if(aop == MUL) temp = *((double*)(f1)) * *((double*)(f2));
      else if(aop == DIV) temp = *((double*)(f1)) / *((double*)(f2));
      else { fprintf(stderr, "unknown arith types \n"); return 0; }
      
      return *((double*)v1) > temp ? 1 : -1;
    }
    else if(*ftypeptr == LONG) {
      long temp = 0;
      if(aop == PLUS) temp = *((long*)(f1)) + *((long*)(f2));
      else if(aop == SUB) temp = *((long*)(f1)) - *((long*)(f2));
      else if(aop == MUL) temp = *((long*)(f1)) * *((long*)(f2));
      else if(aop == DIV) temp = *((long*)(f1)) / *((long*)(f2));
      else { fprintf(stderr, "unknown arith types \n"); return 0; }
      
      return *((long*)v1) > temp ? 1 : -1;
    }
    else { fprintf(stderr, "unknown types \n"); return 0; }
  }

  static bool contains(const char* v, string s, FieldType* ftypeptr) {
    if (*ftypeptr != STRING) {
      cout << " Type other than STRING uses contains " << endl;
      return false;
    }
    size_t found = string(v).find(s);
    if (found == string::npos) return false;
    else return true;
  }
  
  static string tostring(FieldType* ftypeptr, char* v) {
    return ( *ftypeptr == INT			? 	_to_string(*((int*)v))		:
	     *ftypeptr == STRING		?	string(v)			:
	     *ftypeptr == FLOAT		        ?	_to_string(*((float*)v))	:
	     *ftypeptr	== DOUBLE		?	_to_string(*((double*)v))	:
	     *ftypeptr	== LONG         	?	_to_string(*((long*)v))	:
	     " unknown types "			);   
  }  
  
  bool contains(string s) const {
    if (*_ftypeptr != STRING) {
      cout << " Type other than STRING uses contains " << endl;
      return false;
    }
    size_t found = string(_v).find(s);
    if (found == string::npos) return false;
    else return true;
  }
	
  int compare(Field& f) const {
    if(*_ftypeptr == *(f._ftypeptr))
      return compare(f._v);	
    else {
      fprintf(stderr, "unmatching types \n");
      return 0;
    }		
  }
  
  int compare(const char* f) const {
    if(*_ftypeptr == INT) {
      if (*((int*)_v) == *((int*)f))
	return 0;
      else
	return *((int*)_v) > *((int*)f) ? 1 : -1;
    }
    else if(*_ftypeptr == STRING)
      return string(_v).compare(f);
    else if(*_ftypeptr == FLOAT)
      return *((float*)_v) > *((float*)f) ? 1 : -1;
    else if(*_ftypeptr == DOUBLE)
      return *((double*)_v) > *((double*)f) ? 1 : -1;
    else if(*_ftypeptr == LONG)
      return *((long*)_v) > *((long*)f) ? 1 : -1;
    else { fprintf(stderr, "unknown types \n"); return 0; }
  }
  
  int compare(Field& f1, AOperator aop, Field& f2) {
    if(*_ftypeptr == INT) {
      int temp = 0;
      if(aop == PLUS) temp = *((int*)(f1._v)) + *((int*)(f2._v));
      else if(aop == SUB) temp = *((int*)(f1._v)) - *((int*)(f2._v));
      else if(aop == MUL) temp = *((int*)(f1._v)) * *((int*)(f2._v));
      else if(aop == DIV) temp = *((int*)(f1._v)) / *((int*)(f2._v));
      else { fprintf(stderr, "unknown arith types \n"); return 0; }		
      
      if (*((int*)_v) == temp)
	return 0;
      else
	return *((int*)_v) > temp ? 1 : -1;
    }
    else if(*_ftypeptr == FLOAT) {
      float temp = 0;
      if(aop == PLUS) temp = *((float*)(f1._v)) + *((float*)(f2._v));
      else if(aop == SUB) temp = *((float*)(f1._v)) - *((float*)(f2._v));
      else if(aop == MUL) temp = *((float*)(f1._v)) * *((float*)(f2._v));
      else if(aop == DIV) temp = *((float*)(f1._v)) / *((float*)(f2._v));
      else { fprintf(stderr, "unknown arith types \n"); return 0; }
      
      return *((float*)_v) > temp ? 1 : -1;
    }
    else if(*_ftypeptr == DOUBLE) {
      double temp = 0;
      if(aop == PLUS) temp = *((double*)(f1._v)) + *((double*)(f2._v));
      else if(aop == SUB) temp = *((double*)(f1._v)) - *((double*)(f2._v));
      else if(aop == MUL) temp = *((double*)(f1._v)) * *((double*)(f2._v));
      else if(aop == DIV) temp = *((double*)(f1._v)) / *((double*)(f2._v));
      else { fprintf(stderr, "unknown arith types \n"); return 0; }
      
      return *((double*)_v) > temp ? 1 : -1;
    }
    else if(*_ftypeptr == LONG) {
      long temp = 0;
      if(aop == PLUS) temp = *((long*)(f1._v)) + *((long*)(f2._v));
      else if(aop == SUB) temp = *((long*)(f1._v)) - *((long*)(f2._v));
      else if(aop == MUL) temp = *((long*)(f1._v)) * *((long*)(f2._v));
      else if(aop == DIV) temp = *((long*)(f1._v)) / *((long*)(f2._v));
      else { fprintf(stderr, "unknown arith types \n"); return 0; }
      
      return *((long*)_v) > temp ? 1 : -1;
    }
    else { fprintf(stderr, "unknown types \n"); return 0; }
  }
  
  string tostring() {			
    return ( *_ftypeptr == INT			? 	_to_string(*((int*)_v))		:
	     *_ftypeptr == STRING		?	string(_v)			:
	     *_ftypeptr == FLOAT		?	_to_string(*((float*)_v))	:
	     *_ftypeptr	== DOUBLE		?	_to_string(*((double*)_v))	:
	     *_ftypeptr	== LONG         	?	_to_string(*((long*)_v))	:
	     " unknown types "			);
  }
};

#endif /*FIELD_H_*/
