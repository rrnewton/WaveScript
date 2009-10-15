#ifndef CLIENTWRITER_H_
#define CLIENTWRITER_H_

#include "Catalog.h"

class ClientWriter : public Thread {
  Catalog* _catalog;
  char tempbuf[BUFTEMP];
  
 public:
  ClientWriter(string threadname, Catalog* c) : Thread(threadname.c_str()),
    _catalog(c){
      string t = "This is meiyuan's message";
      memcpy(tempbuf, t.c_str(), strlen(t.c_str()) );
  };
 
  ~ClientWriter() {};
  
  void run() { 
   
    while(true) {
      // loop through the operator buffers to see whether they have data to send
      if(hasnewdata()) {
	printf("I am writing \n");
	// go through the buffer to send data
	_catalog->requireoutputmaplock();
	_catalog->requireoperatormaplock();

	MapIter first = _catalog->outputmap.upper_bound(-1);
	while(first != _catalog->outputmap.end()) {
	  ESBuffer* buf = _catalog->operatormap[first->first]->sockbuffer();  
	  if (buf == 0) { 
	    perror("buffer is NULL\n"); 
	    first = _catalog->outputmap.upper_bound(first->first);
	    continue;
	  }
	  EventPtr ep;
	  if(buf->hasNext()) {
	    pair<MapIter, MapIter> ret = 
	      _catalog->outputmap.equal_range(first->first);
	    ep = buf->next();
	    for(MapIter iter = ret.first; iter != ret.second; iter++) 
	      if(send(iter->second, ep->values(), ep->sz(), 0) == -1)
		printf("error sending\n");
	  }
	  
	  
	  first = _catalog->outputmap.upper_bound(first->first);
	}

	/*for(map<int, ESBox*>::iterator iter = _catalog->operatormap.begin(); 
	    iter != _catalog->operatormap.end(); iter++) {
	  ESBuffer* buf = iter->second->buffer();	  
	  if(buf == 0) break;
	  printf("here0\n");
	  if(buf->hasNext()) {
	    printf("here1\n");
	    pair<MapIter, MapIter> ret = 
	      _catalog->outputmap.equal_range(iter->first);
	    for(MapIter iter = ret.first; iter != ret.second; iter++)
	      if(send(iter->second, tempbuf, 25, 0) == -1)
		printf("error sending\n");
	  }
	  
	  }*/
	_catalog->releaseoperatormaplock();
	_catalog->releaseoutputmaplock();
	usleep(100);
	
      }
      
    
      /*while(true) {
        _catalog->requireoutputmaplock();
	pair<MapIter, MapIter> ret = 
	  _catalog->outputmap.equal_range(0);
	for(MapIter iter = ret.first; iter != ret.second; iter++) {
	  printf("sending data.....\n");
	  if(send(iter->second, tempbuf, 25, 0) == -1)
	    printf("error sending\n");
	}
	_catalog->releaseoutputmaplock();
	usleep(10);
	}*/
    }
    
  }

 private:
  // do not lock operatormap, because not perform real write
  bool hasnewdata() {
    bool flag = false;
    _catalog->requireoutputmaplock();
    MapIter first = _catalog->outputmap.upper_bound(-1);
    while(first != _catalog->outputmap.end()) {
      //   int my = first->first;
      //printf("%d\n", my);
      ESBox* op = _catalog->operatormap[first->first];
      if(op->sockbuffer() !=0 && op->sockbuffer()->hasNext()) {
	flag = true; break;
      }
      first = _catalog->outputmap.upper_bound(first->first);
      
    }
    _catalog->releaseoutputmaplock();

    /*    for(map<int, ESBox*>::iterator iter = _catalog->operatormap.begin(); 
	iter != _catalog->operatormap.end(); iter++) {
      if (iter->second->buffer() !=0 && iter->second->buffer()->hasNext()) {
	flag = true; break;
      }
      }*/

    return flag;
  }
};
#endif // CLIENTWRITER_H_
