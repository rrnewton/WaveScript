#ifndef FILESOURCE_H_
#define FILESOURCE_H_

#include "ESBox.h"

//class FileSource : public ESBox<SourceBuffer, Esrc_iter, EventPtr>{
class FileSource : public ESBox, public Thread {  
  string _path;
  ifstream _file;  
  ESBuffer* _buffer;                // internal buffer

  // for read file usage
  string* values; Event* e;
 

 public:
  FileSource(int index, EventType* etype, const char* path) 
    : ESBox(index, SRC, etype, etype), Thread("source") {
    _path = path; _buffer = new ESBuffer(etype->bytes());   

    values = new string[_inetptr->fieldsno()];
    e = new Event(_inetptr);
  }
  
  ~FileSource() { delete _buffer; delete[] values; delete e; }   
    
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
    // if need buffered
    if(_ndbuffer && _sockbuffer->full()) { 
      //printf("source sockbuffer is full, hold on.....\n"); 
      return 0; 
    }
    
    // _buffer->requirelock();
    EventPtr eptr = _buffer->next();
    //_buffer->releaselock();

    if(_ndbuffer) {       
      _sockbuffer->add(eptr->values()); 
    }
    return eptr;     
  }    

  bool hasNext() { return _buffer->hasNext(); }
  void print(ostream& os) { _buffer->print(os, _inetptr); }
  
  void run() {
    // read data into buffer, and set active pointer to the front;
    while(!readfile()) {
      usleep(SLEEPTIME);
    } 
    printf("endof\n");
  }

  //  bool source() { return readfile(); }

 private:
  bool open() {
    if(!_open) { 
      _file.open(_path.c_str(), ios::in);  
      start();
      _open = true; return _open;

    }
    return false;	
  }

  bool close() { 
    if(_open) { _file.close(); _open = false; return true; } 
    return false;
  }   

  
  bool readfile() {
    char buf[BUFTEMP];
    int fno = _inetptr->fieldsno();
        
    // read all into the input buffer
    while( !_buffer->full() && 
	  _file.good() && (!(_file.getline( buf, BUFTEMP ).eof()))) {
      values[0] = string(strtok(buf, ","));
      for(int i=1; i<fno; i++)
	values[i] = string(strtok(NULL, ","));      
      e->copy(_inetptr, values);
      _buffer->add(e->values());		
    }

    if(_file.eof()) return true;    // file is reaching end; 
    else return false;
  }
  
};

#endif /*FILESOURCE_H_*/
