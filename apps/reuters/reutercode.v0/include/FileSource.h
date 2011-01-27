#ifndef FILESOURCE_H_
#define FILESOURCE_H_

#include "ESBox.h"

//class FileSource : public ESBox<SourceBuffer, Esrc_iter, EventPtr>{
class FileSource : public ESBox{  
  string _path;
  ifstream _file;  
  ESBuffer* _buffer;   // internal buffer
  
 public:
  FileSource(int index, EventType* etype, const char* path) 
    : ESBox(index, SRC, etype, etype) {
    _path = path; _buffer = new ESBuffer();
  }
  
  ~FileSource() { delete _buffer; }   
    
  bool open(Catalog* cat) {
    if(open()) {
      cat->addoperator(_index, this); 
      return true;
    }
    return false;
  }

  void close(Catalog* cat) {
    if(close()) { cat->eraseoperator(_index); }
  }  
  
  EventPtr next() { 
    EventPtr eptr = _buffer->next();
    if(_ndbuffer) { 
      EventPtr ep = new Event(*eptr); 
      _sockbuffer->push_back(ep); 
    } 
    return eptr;     
  }    

  bool hasNext() { return _buffer->hasNext(); }
  void print(ostream& os) { _buffer->print(os, _inetptr); }

 private:
  bool open() {
    if(!_open) { 
      _file.open(_path.c_str(), ios::in);
      
      // read data into buffer, and set active pointer to the front;
      readfile(); 
      _open = true; return _open;
    }
    return false;	
  }

  bool close() { 
    if(_open) { _file.close(); _open = false; return true; } 
    return false;
  }   
  
  void readfile() {
    char buf[BUFTEMP];
    int fno = _inetptr->fieldsno();
    string* values = new string[fno];
    
    // read all into the input buffer
    while(_file.good() && (!(_file.getline( buf, BUFTEMP ).eof()))) {
      values[0] = string(strtok(buf, ","));
      for(int i=1; i<fno; i++)
	values[i] = string(strtok(NULL, ","));
      
      Event* e = new Event(_inetptr, values);
      _buffer->push_back(e);		
    }
    delete[] values;	
  }
  
};

#endif /*FILESOURCE_H_*/
