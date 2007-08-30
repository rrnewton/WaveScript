
// This file uses the foreign interface to provide access to some file
// IO, shell access, and unix system calls.  A comprehensive job would
// be very involved, so we're adding to this as need arrises (as with
// all our libraries, really).

// Author: Ryan Newton

namespace Internal {

ext = "dylib" // or .so

libc = "libc."++ext

}


namespace Unix {

// Can we support error codes across backends?
system :: String -> Int = 
   foreign("system", ["stdlib.h", Internal:libc])

usleep :: Int -> () = 
   foreign("usleep",["unistd.h", Internal:libc])

fopen :: (String, String) -> Pointer "FILE*" = 
   foreign("fopen", [Internal:libc])

// Write a stream of strings to disk.  Returns an empty stream
fileSink :: Stream String  -> Stream nothing;
fun fileSink (strm) {
  iterate str in strm {  
  }
}

}

//dump_stream

BASE <- iterate _ in timer$ 3.0 {
  state {fst=true}
  
  using Unix;

  if fst then {
    print$ "woot\n";

    // WEIRD!!!! USLEEP WORKS IN ONE PLACE BUT NOT THE OTHER.
    //    usleep(1000000);
    system("ls");
    usleep(500000);
    print$ "yay finished!\n";

    fp = fopen("foo.out","w");
    
    print("Got file handle ! "++fp++"\n");

    emit 1;   
  };
  fst := false;
  emit 0;
}


