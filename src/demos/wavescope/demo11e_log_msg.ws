
// test for log

include "timersource.ws";
include "netsource.ws";

s1 = timer_source("test", 1000);

//log_msg(1, "Making timer source");

s2 = iterate f in s1 {
  print("got counter " ++ f ++ "\n");
  log(1, "got_counter" ++ f);
  emit(f);
};

main = s2
