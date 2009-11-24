#ifndef CATALOG_H_
#define CATALOG_H_

#include <map>
#include "ESBox.h"

#define MapIter        multimap<int, int>::iterator
class ESBox;

class Catalog {

 public:
  /* operator id -> client */
  multimap<int, int> outputmap;
  pthread_mutex_t outputmap_mutex;

  /* operator id -> operator pointer */
  map<int, ESBox*> operatormap;
  pthread_mutex_t operatormap_mutex;

  Catalog() {
    pthread_mutex_init(&outputmap_mutex, NULL);
    pthread_mutex_init(&operatormap_mutex, NULL);
  }
  
  ~Catalog() {
    pthread_mutex_destroy(&outputmap_mutex);
    pthread_mutex_destroy(&operatormap_mutex);    
  }


  //--------  outputmap  --------
  bool requireoutputmaplock() {
    //printf("requiring outputmaplock.....\n");
    if(pthread_mutex_lock(&outputmap_mutex) == 0) return true;
    else return false;
  }
  
  bool releaseoutputmaplock() {
    //printf("releasing outputmaplock.....\n");
    if(pthread_mutex_unlock(&outputmap_mutex) == 0) return true;
    else return false;
  }

  void printoutputmap() {
    for(MapIter iter=outputmap.begin(); iter!=outputmap.end(); iter++) {
      printf("pair(%d, %d)\n", iter->first, iter->second);
    }
  }


  //--------  operatormap  --------  
  bool requireoperatormaplock() {
    //printf("requiring operatormaplock.....\n");
    if(pthread_mutex_lock(&operatormap_mutex) == 0) return true;
    else return false;
  }

  bool releaseoperatormaplock() {
    //printf("releasing operatormaplock.....\n");
    if(pthread_mutex_unlock(&operatormap_mutex) == 0) return true;
    else return false;
  }

  /*bool hasnewdata() {
    bool flag = false;
    requireoperatormaplock();			
    for(map<int, ESBox*>::iterator iter = operatormap.begin(); 
	iter != operatormap.end(); iter++) {
      if (iter->second->buffer()->hasNext()) {
	flag = true; break;
      }
    }
    releaseoperatormaplock();
    return flag;
    }*/

  void addoperator(int index, ESBox* opt) {
    requireoperatormaplock();
    operatormap[index] = opt;
    releaseoperatormaplock();
  }

  void eraseoperator(int index) {
    requireoperatormaplock();
    operatormap.erase(index);
    releaseoperatormaplock();    
  }
  
};
#endif
