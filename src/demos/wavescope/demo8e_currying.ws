

include "stdlib.ws";

using Curry;

sig = stream_map(fun(ss) [ss,ss])$
   (readFile("test.ws", "mode: binary", timer(3.0)) 
    :: Stream (Sigseg Int));

BASE <- (stream_map$ map$ sigseg_map$ (+3)) (sig);
