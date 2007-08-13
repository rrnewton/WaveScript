

fun dots_to_underbars(str) {
  String:implode(List:map(fun (x) if (x == '.') then 
                          '_' else x, String:explode(str)));
}


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

// enqueue function, which pushes a sigseg and associated 
// stream, sigseg data, start sample, timebase, length, sample_size
c_wsnet_enqueue_sigseg4 :: (Pointer "void*", Int64, Int, Timebase, Array Int16) -> Int =
  foreign("wsnet_enqueue_sigseg4", includes);

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

// specialised sigseg version
fun netpub_sigseg4(s, name) {
  iterate l in s {
    state {
      ns = c_wsnet_register(name);
    }
    seg = List:ref(l,0);
    arr = Array:append(map(toArray,l));
    c_wsnet_enqueue_sigseg4(ns, seg`start, seg`width, seg`timebase, arr);
    emit(l);
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
    WRAP_WSENTRY(__entry_"++id++"(x));
    return EVENT_RENEW;
  }

  void __init_"++id++"() {
    elog(LOG_WARNING, \"connecting to stream\");
    subscription_client_create(\""++host++"\", \""++name++"\", __ready_"++id++", NULL);
  }
"}

fun netsub_int(host, name) {
  id = dots_to_underbars(host)++"_"++name;
  ccode = inline_C(gen_glue_int(host,name,id), "__init_"++id);
  src = (foreign_source("__entry_"++id, []) :: Stream Int);
  merge(ccode, src)
}



// host is the ip addr, name is the name of the stream, id is 

c_globalsample_get_tb :: () -> Timebase = 
  foreign("globalsample_get_tb", []);

fun gen_glue_sigseg4 (host,name,id) {
"
  #include <devel/wavescope/wavescope_ensbox.h>

  int __globalsample_tb = 0;
  int globalsample_get_tb() { return __globalsample_tb; }

  int __ready_"++id++"(ev_tcp_peer_t *peer, char *buf, uint size) {
    // cast buffer to char * and parse out:
    // timestamp (double)
    // sig seg
    // cast buffer to int and call entry point with it.

    /* unmarshal from the buffer to call into entry */
    struct sigseg4_msg *cast = (struct sigseg4_msg *)buf;
    double time = tv_to_double(&(cast->start_time));
    int len = (size - sizeof(struct sigseg4_msg))/4/sizeof(int16_t);

    double sample = 0;
    if (timebase_conv(gps_timebase(), time, __globalsample_tb, &sample) < 0) {
      elog(LOG_WARNING, \"can't convert sample numbers, dropping data\");
    }
    else {
      WRAP_WSENTRY(
        __entry_"++id++"(sample, len, 
                         cast->target,
                         cast->target+len,
                         cast->target+len*2,
                         cast->target+len*3));
    }
    return EVENT_RENEW;
  }

  void __init_"++id++"() {
    elog(LOG_WARNING, \"connecting to stream\");
    __globalsample_tb = timebase_new(my_node_id, CLOCK_GLOBALVXP, 0.1);
    timebase_add_segment(gps_timebase(), 1, __globalsample_tb, 48000);
    timebase_add_segment(gps_timebase(), 2, __globalsample_tb, 96000);
    subscription_client_create(\""++host++"\", \""++name++"\", __ready_"++id++", NULL);
  }
"}

fun netsub_4sigseg(host, name) {
  id = dots_to_underbars(host)++"_"++name;
  ccode = inline_C(gen_glue_sigseg4(host,name,id), "__init_"++id);
  src = (foreign_source("__entry_"++id, []) :: Stream (Int64 * Int * 
	Pointer "int16_t*" * Pointer "int16_t*" * 
	Pointer "int16_t*" * Pointer "int16_t*"));
  conv = iterate (sample, len, p1, p2, p3, p4) in src {
    arr1 :: Array Int16 = ptrToArray(p1,len);
    arr2 :: Array Int16 = ptrToArray(p2,len);
    arr3 :: Array Int16 = ptrToArray(p3,len);
    arr4 :: Array Int16 = ptrToArray(p4,len);
    emit([ toSigseg(arr1, sample, c_globalsample_get_tb()),
	   toSigseg(arr2, sample, c_globalsample_get_tb()),
	   toSigseg(arr3, sample, c_globalsample_get_tb()),
	   toSigseg(arr4, sample, c_globalsample_get_tb()) ]);
  };
  merge(ccode, conv)
}
