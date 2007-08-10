
//
//  PUBLISH
//

//includes = ["wavescope_ensbox.h", "libwavescope2.a"];
includes = ["wavescope_ensbox.h"];

// rrn: void* needs to be changed to the correct type!

// registration function.  this registers a new stream for export.
c_wsnet_register :: String -> Pointer "void*" = 
  foreign("wsnet_register", includes);

// enqueue function.  this pushes an int into a network stream
c_wsnet_enqueue_int :: (Pointer "void*", Int) -> Int = 
  foreign("wsnet_enqueue_int", includes);


raw_log :: (Int, String, Int) -> () = foreign("log_msg", includes);
fun log(l,s) raw_log(l,s++" ",s`String:length);

// is there a way to generalize this on types?
fun netpub_int(s, name) {
  iterate i in s {
    state {
      ns = c_wsnet_register(name);
    }
    c_wsnet_enqueue_int(ns, i);
    emit(i);
  }
}



//
//  SUBSCRIBE
//


fun gen_glue_int (host,name,id) {
"
  #include <devel/wavescope/wavescope_ensbox.h>

  int __ready_"++id++"(ev_tcp_peer_t *peer, char *buf, uint size) {
    // cast buffer to int and call entry point with it.
    int x = *(int*)buf; 
    __entry_"++id++"(x);
    return EVENT_RENEW;
  }

  void __init_"++id++"() {
    elog(LOG_WARNING, \"connecting to stream\");
    subscription_client_create(\""++host++"\", \""++name++"\", __ready_"++host++"_"++name++", NULL);
  }
"}

fun netsub_int(host, name) {
  id = host++"_"++name;
  ccode = inline_C(gen_glue_int(host,name,id), "__init_"++id);
  src = (foreign_source("__entry_"++id, []) :: Stream Int);
  merge(ccode, src)
}
