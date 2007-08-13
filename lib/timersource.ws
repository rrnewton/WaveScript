//
//  Timer Source
//

//includes = ["wavescope_ensbox.h", "libwavescope2.a"];

fun gen_timerglue (name,period) ("
  #include <devel/wavescope/wavescope_ensbox.h>

  int __timercb_"++name++"(void *data, int interval, g_event_t *ev) {
    static int counter = 0;
    // trigger into wavescript
    WRAP_WSENTRY(__timerentry_"++name++"(counter++));
    return EVENT_RENEW;
  }

  void __inittimer_"++name++"() {
    elog(LOG_WARNING, \"setting timer source "++name++" for period "++period++"\");
    g_timer_add("++period++", __timercb_"++name++", NULL, NULL, NULL);
  }
")

fun timer_source(name, period) {
  ccode = inline_C(gen_timerglue(name, period), "__inittimer_"++name);
  src = (foreign_source("__timerentry_"++name, []) :: Stream Int);
  merge(ccode, src)
}

