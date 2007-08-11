
nullterm = String:implode([intToChar(0)])

raw_log :: (Int, String) -> () = foreign("log_msg", ["wavescope_ensbox.h"]);
fun log(l,s) raw_log(l, s++nullterm);

raw_log_file :: (Int, String) -> () = foreign("log_msg_to_file", ["wavescope_ensbox.h"]);
fun log_file(l,s) raw_log_file(l, s++nullterm);
