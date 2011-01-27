#ifndef QUERYENGINE_H_
#define QUERYENGINE_H_

#include "FileSource.h"
#include "Filter.h"

class QueryEngine {
  string _name;
  ESBox* _root;
  Catalog* _catalog;
 public:
  //QueryEngine(string name, ESBox* root) : Thread(), _root(root){
  QueryEngine(string name, ESBox* root, Catalog* catalog) : 
  _name(name+".queryengine"), _root(root), _catalog(catalog) {
    
    //    string threadname = name + ".queryengine";
    //strcpy(_threadname, threadname.c_str());
  }
  
  bool open() { 
    cout << _name << "started" << endl; 
    return _root->open(_catalog); 
  }
  void close() { 
    _root->close(_catalog); 
    cout << _name << " is finished" << endl; 
  }

  void run() {
    if(!open()) { 
      cout << _name << "cannot be opened twice" << endl; 
      pthread_exit(NULL);
    }
    
    // run the query
    while(_root->hasNext()) {
      EventPtr ept = _root->next();
      ept->print(cout, _root->outetptr());
    }    
    
    //close();
    while(true) { sleep(10); }
  }
};
#endif
