#ifndef FIELDTYPE_H_
#define FIELDTYPE_H_

#include <cstring>
#include <cstdlib>

enum Rep { INT, STRING, FLOAT, DOUBLE, LONG, UNKNOWN };

/**
 * class representing a field type
 */
class FieldType {
  
 private:
  Rep _type;
  
 public:
  FieldType(const Rep& type) { _type = type; }
  FieldType(){ _type = UNKNOWN; }	
  inline bool operator == (const FieldType& t) { return _type == t._type; }	
  inline bool operator != (const FieldType& t) { return _type != t._type; }	
  inline bool operator == (const Rep& a) { return _type == a; }
  
  size_t bytes() {
    return ( _type == INT		? 	sizeof(int)	:
	     _type == STRING		?	S_SIZE		:
	     _type == FLOAT		?	sizeof(float)	:
	     _type == DOUBLE		?	sizeof(double)	:
	     _type == LONG		?	sizeof(long)	:	     
	                                        -1		);				
  }
	
  string tostring() {
    return( _type == INT 		?       "int"		:
	    _type == STRING		?       "string"	:
	    _type == FLOAT		?       "float"	        :
	    _type == DOUBLE		?       "double"	:	
	    _type == LONG		?       "long"	:	
	                                        "[unknown]"     );		
  }
};

#endif /*FIELDTYPE_H_*/

