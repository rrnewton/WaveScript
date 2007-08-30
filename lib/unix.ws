
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
system :: String -> Int = foreign("system", ["stdlib.h", Internal:libc])

usleep :: Int -> () = foreign("usleep",["unistd.h", Internal:libc])


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
    usleep(1000000);
    print$ "yay finished!\n";
    emit 1;
  };
  fst := false;
  emit 0;
}


