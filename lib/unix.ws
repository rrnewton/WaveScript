
// This file uses the foreign interface to provide access to some file
// IO, shell access, and unix system calls.  A comprehensive job would
// be very involved, so we're adding to this as need arrises (as with
// all our libraries, really).

// Author: Ryan Newton

type FileDescr = Pointer "FILE*";

namespace Unix {

  namespace Internal {

    ext = "dylib" // or .so

    //libc = ["libc."++ext]
    //libc = ["./libc.so"]
    libc = []
  }

  //using Internal;

stdio = "stdio.h":::Internal:libc;

// Can we support error codes across backends?
system :: String -> Int = 
   foreign("system", "stdlib.h":::Internal:libc)

usleep :: Int -> () = 
   foreign("usleep","unistd.h":::Internal:libc)

fopen  :: (String, String) -> FileDescr = foreign("fopen",  stdio);
fclose :: FileDescr -> Int              = foreign("fclose", stdio);

// This is kind of funny, we have two entries to the same functions.
fread :: (Pointer "void*", Int, Int, FileDescr) -> Int = 
   foreign("fread", stdio)
fwrite :: (Pointer "void*", Int, Int, FileDescr) -> Int = 
   foreign("fwrite", stdio)

// Damn, strings are not mutable...
fread_arr :: (Array Char, Int, Int, FileDescr) -> Int = 
   foreign("fread", stdio)
fwrite_arr :: (Array Char, Int, Int, FileDescr) -> Int = 
  //fwrite :: (Pointer "void*", Int, Int, FileDescr) -> Int = 
   foreign("fwrite", stdio)

malloc :: Int -> Pointer "void*" = foreign("malloc",[]);

} // End namespace



// Write a stream of strings to disk.  Returns an empty stream
fileSink :: (String, Stream String)  -> Stream nothing;
fun fileSink (filename, strm) {
  iterate str in strm {  
    state {
      fp = Unix:fopen(filename, "w")
    }
    // This is very inefficient, it should be fixed:
    arr = List:toArray( String:explode(str));
    len = Array:length(arr);
    cnt = Unix:fwrite_arr(arr, 1, len, fp);
    if cnt != len 
    then wserror("fileSink: fwrite failed to write data")
  }
}



strings = iterate _ in timer(3.0) {
  state { cnt = 0 }
  if cnt < 15 then emit cnt++"\n";
  cnt += 1;  
}

BASE <- fileSink("stream.out", strings)


/*

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

    buf = List:toArray$ String:explode$ "foobar\nbaz";

    print$ "Allocated buffer to send...\n";

    cnt = fwrite_arr(buf, 1,7, fp);

    print$ "Wrote "++cnt++" characters to file!\n";

    inbuf = malloc(10);
    fclose(fp);
    fp2 = fopen("foo.out","r");

    cnt2 = fread(inbuf, 1,7, fp2);
    str = String:implode$ Array:toList$ (ptrToArray(inbuf,7) :: Array Char);
    
    print$ "Read back "++cnt2++" chars: <"++str++">\n";
    
    emit 1;
  };
  fst := false;
  emit 0;
}


*/
