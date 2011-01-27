#ifndef EVENTTYPE_H_
#define EVENTTYPE_H_

#define EventTypePtr	EventType*

#include "EventStream.h"
#include "FieldType.h"
#include <map>

/**
 * class representing a type of an event
 */
class EventType {
 private:
  string _name;			// name of this event type
  int _fnumber;			// number of fields  
  string* _fnames;		// name of each field
  FieldType* _ftypes; 	        // type of each field
  
  // map each field name to an internel index, for search by name
  map<string, int> _map;	
  
 public:
  /*
   * constructor
   * @param: 
   *  nm: name of this event type
   *  fn: number of fields
   *  fieldnames: name of each field
   *  fieldtypes: type of each field 
   */	
  EventType(const char* name, int fnumber, const char** fnames, FieldType* ftypes) {
    _name = string(name);
    _fnumber = fnumber;
    _fnames = new string[_fnumber];
    _ftypes = new FieldType[_fnumber];

    for(int i=0; i<_fnumber; i++) {
      _fnames[i] = string(fnames[i]);
      _ftypes[i] = ftypes[i];
      _map[_fnames[i]] = i;
    }
  }

  EventType(const EventType& etype) {
    _name = string(etype._name);
    _fnumber = etype._fnumber;
    _fnames = new string[_fnumber];
    _ftypes = new FieldType[_fnumber];

    for(int i=0; i<_fnumber; i++) {
      _fnames[i] = string(etype._fnames[i]);
      _ftypes[i] = etype._ftypes[i];
      _map[_fnames[i]] = i;
    }    

  }
  
  ~EventType() { delete[] _fnames; delete[] _ftypes; }
  
  inline bool operator == (const EventType& et) {
    if(_name.compare(et._name) != 0)	// we just compare whether they have same name
      return false;
    return true;
  }
  
  inline bool operator != (const EventType& et) {
    if(_name.compare(et._name) == 0)	// we just compare whether they have same name
      return false;
    return true;
  }
  
  // return type of field
  FieldType* fieldtype(const char* name) { return &_ftypes[_map[string(name)]]; }	
  FieldType* fieldtype(int index) { return &_ftypes[index]; }	
  string& name() { return _name; }
  int fieldsno() { return _fnumber; }
  int name2idx(const char* name) { return _map[string(name)];	}
  
  const char* idx2name(int index) {
    if (index >= _fnumber) {
      fprintf(stderr, "index out of bound of number fields \n");
      exit(0);
    }
    return _fnames[index].c_str();
  }

  size_t bytes() {
    size_t length = 0;
    for(int i=0; i<_fnumber; i++)
      length += _ftypes[i].bytes();
    return length;    
  }
  
  string tostring() {
    string s = "type name: " + _name + "\t" + "number fields: " + _to_string(_fnumber) + "\n";
    for(int i=0; i<_fnumber; i++)
      s = s + "[field" + _to_string(i) + " name: " + _fnames[i] + " ; type: " + _ftypes[i].tostring() + "] ";
    s = s + "\n";
    return s;		
  }
};

#endif /*EVENTTYPE_H_*/

