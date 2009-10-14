#ifndef THREAD_H_
#define THREAD_H_

#include <pthread.h>
#include <errno.h>
#include <stdio.h>
#include <cstdlib>
#include <string>
#include <cstring>

/*
 * Thread.hpp
 *
 * This defines a Thread class
 * Any class that wishes to become a thread should extend
 *  this class and override the run() method.
 *
 * Just like Java!
 */

# ifndef THREAD_NAME_LEN
# define THREAD_NAME_LEN 32
# endif

// Thread state bits
#define STATE_RUNNING  1
#define STATE_DETACHED 2
#define STATE_DEFAULT (0) // defined by pthread library

using namespace std;

class Thread {

 protected:
  pthread_t _thread;                      // thread itself
  unsigned int _state;                    // thread status
  char _threadname[THREAD_NAME_LEN];      // see debug.hpp

 public:
  
  Thread() : _thread(0), _state(STATE_DEFAULT){};
  Thread(const char* s) : _thread(0), _state(STATE_DEFAULT) { 
    strcpy(_threadname, s);
  }
  
  Thread(const Thread&); // copy constructor
  virtual ~Thread(){};

  // Override this method
  virtual void run() = 0;

  // Call this method to start a thread
  void start() {
    // do nothing if the thread has already been started
    if (_thread != 0) return;   

    pthread_attr_t attr;
    int init_ret = pthread_attr_init(&attr);

    if(init_ret != 0) {
      errno = init_ret;
      perror("pthread_attr_init failed\n"); exit(1);
    }

    if (_state & STATE_DETACHED) {
      int sds_ret = pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
     
      if(sds_ret != 0) {
	errno = sds_ret;
	perror("pthread_attr_setdetachstate failed\n"); exit(1);
      }
    }

    // default set up as a joinable thread
    int sds_ret = pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
    if(sds_ret != 0) {
      errno = sds_ret;
      perror("pthread_attr_setdetachstate joinable failed\n"); exit(1);
    }

    // create the thread
    int create_ret = pthread_create(&_thread, &attr, Thread::_start, this);
    if(create_ret != 0) {
      errno = create_ret;
      perror("pthread_create failed\n"); exit(1);
    }

    pthread_attr_destroy(&attr);    
  }

  void stop() {
    if(_state & STATE_RUNNING) {
      printf("STOPPING THREAD %s\n", getName());

      int cancel_ret = pthread_cancel(_thread);
      if(cancel_ret != 0) {
	errno = cancel_ret;
	perror("pthread_cancel failed\n"); exit(1);
      }
      else {
	printf("THREAD %s STOPPED\n", getName());
	_state &= ~STATE_RUNNING;
      }
    }
    else {
      printf("Cannot Cancel THREAD %s, not running\n", getName());
    }
  }

  void join() {
    int join_ret = pthread_join(_thread, NULL);
    if(join_ret != 0) {
      errno = join_ret;
      perror("pthread_join failed\n"); exit(1);
    }
  }

  void setDetached() {
    // Do nothing if already detached
    if(_state & STATE_DETACHED)
      return;

    if(_state & STATE_RUNNING) {
      int detach_ret = pthread_detach(_thread);
      if(detach_ret != 0) {
	errno = detach_ret;
	perror("pthread_detach failed\n"); exit(1);
      }
    }
    _state |= STATE_DETACHED;
  }

  bool isDetached() { return _state & STATE_DETACHED;  }
  bool isLive() { return _state & STATE_RUNNING; }
  int getID() { return (int)_thread; }
  const char *getName() { return _threadname; }

  inline static pthread_t myID() { return pthread_self(); }

  // new and initialize the new mutex
  static pthread_mutex_t* initmutex() { 
    pthread_mutex_t* mutex = new pthread_mutex_t;
    pthread_mutex_init(mutex, NULL);    
    return mutex;
  }

private:
  static void* _start(void* thisPtr) {
    Thread* t = (Thread *)thisPtr;
    printf("STARTTED THREAD %s.........\n", t->getName());

    pthread_cleanup_push(_cleanup, thisPtr);
    t->_state |= STATE_RUNNING;
    t->run();  // user code here
    pthread_cleanup_pop(1);
    pthread_exit(NULL);    
  }

  static void _cleanup(void *thisPtr) {
    Thread* t = (Thread*) thisPtr;
    t->_state &= ~STATE_RUNNING;
    //    delete t;    
  }  
};

#endif
