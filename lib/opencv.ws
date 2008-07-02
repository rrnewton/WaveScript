/* 

 Here I will accumulate wrappers to useful functions within OpenCV

 .author Ryan Newton

 */

//include "stdlib.ws"
include "unix.ws"

// Image loading and saving:

// Scan a directory for image files and stream the file names.
//scandir ::
fun scandir(dir) {
  c_exts = ["ws_opencv.c"];  // C extensions that go with this file.

  /*  
  scandir :: (String, Pointer "struct dirent ***", 
                      Pointer "int*(const struct dirent*)", 
		      Pointer "int*(const struct dirent**, const struct dirent**)") 
          -> Int 
    = foreign("scandir", ["dirent.h"]);
  */
  
  scandir_sorted :: (String, Pointer "struct dirent ***") -> Int = foreign("scandir_sorted", c_exts);

  ws_namelist_ptr :: () -> Pointer "struct dirent ***" = foreign("ws_namelist_ptr", c_exts);

  //scandir :: String -> Pointer "struct dirent **" = foreign("ws_scandir", c_exts);
  getname :: (Pointer "struct dirent ***", Int) -> String = foreign("getname", c_exts);
  freenamelist :: (Pointer "struct dirent ***", Int) -> () = foreign("freenamelist", c_exts);
  
  iterate _ in timer$3 {
    state { count = -1; 
            index = 0;
            files = ptrMakeNull(); 
          }

    // Initialize:
    if count == -1 then {

      files := ws_namelist_ptr();
      //count := scandir(dir, files, ptrMakeNull(), ptrMakeNull());
      count := scandir_sorted(dir, files);

      //println$ " Scandir results: "++ n;

      //println$ " File name read back to WS: "++ fst;
      //print$ "  File name read back to WS: ";
      // print$ fst;
      //print$ "\n";
    };

    if index == count then {
      freenamelist(files, count);
    };
    
    if index < count then {
      name = getname(files, index);
      index += 1;
      
      // We prune out "." and "..".
      if not(name == "." || name == "..")
      then emit name;
    }
  }
}

imgs = scandir("/home/newton/wavescript_unison/apps/vision_ucla/input/FeederStation_2007-06-26_14-00-03.000")

main = iterate frame in imgs {
  //println$ "Got frame!! length "++ Array:length(frame);
  //emit String:length(frame)
  emit frame;
}
