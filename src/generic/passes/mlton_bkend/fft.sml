


val test = _import "testo" : (Real32.real -> Int32.int);

val fftR2C = _import "fftR2C" : (Real32.real array * Word64.word array * int) -> unit;

val _ = print "YAY\n"
val _ = print (Int32.toString (test 34.7))
val _ = print "\n"


val arr  = Array.fromList [1.0 ,2.0, 3.0, 4.0]
val arr2 = Array.array(100, Word64.fromInt 0)

val _ = fftR2C (arr, arr2, 4)


val _ = print "Got fft result! \n"
