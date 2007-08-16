
nullterm = String:implode([intToChar(0)])

raw_log :: (Int, String) -> () = foreign("log_msg", ["wavescope_ensbox.h"]);
fun log(l,s) raw_log(l, s++nullterm);

raw_log_file :: (Int, String) -> () = foreign("log_msg_to_file", ["wavescope_ensbox.h"]);
fun log_file(l,s) raw_log_file(l, s++nullterm);

LOG_TIMING = 255;
// from elog.h in emstar
LOG_EMERG = 0;       /**< system is unusable */
LOG_ALERT = 1;       /**< action must be taken immediately */
LOG_CRIT = 2;      /**< critical conditions */
LOG_ERR = 3;      /**< error conditions */
LOG_WARNING = 4;       /**< warning conditions */
LOG_NOTICE = 5;      /**< normal but significant condition */
LOG_INFO = 6;      /**< informational */
LOG_DEBUG_0 = 7;       /**< debug */
LOG_OFF =  -1;	/**< don't emit log messages */
LOG_UNDEFINED = -2;      /**< loglevel not specified */
