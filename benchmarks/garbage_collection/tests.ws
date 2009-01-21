

include "gc_kernels.ws"

fun tryenv_int(str,x)
  if GETENV(str) == ""
  then x
  else GETENV(str).stringToInt

// This is a four-dimensional space to explore:
test1 = chain(16, listcont, 
              keep_random_send_random(100, 100, 100)) 


main = test1
