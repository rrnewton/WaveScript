


#include <tbb/spin_mutex.h>
#include <tbb/queuing_mutex.h>

extern "C"  {
  void* make_spin_mutex()    { return new tbb::spin_mutex(); }
  void acquire_spin_mutex    (tbb::spin_mutex* M) { M->lock(); }
  bool try_acquire_spin_mutex(tbb::spin_mutex* M) { return M->try_lock(); }
  void release_spin_mutex    (tbb::spin_mutex* M) { M->unlock(); }

    /*
  void* make_queuing_mutex()    { return new tbb::queuing_mutex(); }
  void acquire_queuing_mutex    (tbb::queuing_mutex* M) { tbb::queuing_mutex::scoped_lock::acquire(*M); }
  bool try_acquire_queuing_mutex(tbb::queuing_mutex* M) { return tbb::queuing_mutex::scoped_lock::try_acquire(*M); }
  void release_queuing_mutex    (tbb::queuing_mutex* M) { tbb::queuing_mutex::scoped_lock::unlock(*M); }
    */
}
