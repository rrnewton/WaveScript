


val fftR2C = _import "fftR2C" : (Real32.real array * Word64.word array * int) -> unit;

val test = _import "testo" : (Real32.real -> Int32.int);

val _ = print "YAY\n"
val _ = print (Int32.toString (test 34.7))
val _ = print "\n"
