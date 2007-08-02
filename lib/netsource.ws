
//
//  PUBLISH
//

// registration function.  this registers a new stream for export.
c_wsnet_register :: String -> Pointer = foreign "wsnet_register" in ["wavescope_ensbox.h" "libwavescope2.a"]

// enqueue function.  this pushes an int into a network stream
c_wsnet_enqueue_int :: Pointer Int -> Int = foreign "wsnet_enqueue_int" in ["wavescope_ensbox.h" "libwavescope2.a"]

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


fun gen_glue_int (host, name) ("
  int __ready_"++host++"_"++name++"(ev_tcp_peer_t *peer, char *buf, uint size) {
    // cast buffer to int and call entry point with it.
    int x = *(int*)buf; 
    __entry_"++host++"_"++name++"(x);
  }

  void __init_"++host++"_"++name++"() {
    elog(LOG_WARNING, \"connecting to stream\");
    subscription_client_create(\""++host++"\", \""++name++"\", __ready_"++host++"_"++name++");
  }
")

fun netsub_int(host, name) {
  ccode = inline_C(gen_glue_int(host, name), "__init_"++host++"_"++name);
  src = foreign_source("__entry_"++host++"_"++name++", []);
  merge(ccode, src)
}



