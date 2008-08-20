/* 

 Here I will accumulate wrappers to useful functions within OpenCV

 .author Ryan Newton

 */

//include "stdlib.ws"
include "unix.ws"

// Image loading and saving:

//cv_files = ["opencv_wrappers.c", "libcv.a", "libcvaux.a", "libcxcore.a", "libhighgui.a"];
cv_files = ["opencv_wrappers.c", "libcv.so", "libcvaux.so", "libcxcore.so", "libhighgui.so"];

//ws_readImage :: String -> Array Uint8 = foreign("ws_readImage", cv_files);
ws_readImage :: String -> (Array Uint8 * Int * Int) = foreign("ws_readImage", cv_files);

ws_writeImage :: (String, Array Uint8, Int, Int, Int) -> Bool = foreign("ws_writeImage", cv_files);


// Takes a stream of full path names 
/*
fun stream_images(files) {
  iterate path in files { 
    //state { ind = 0 }
    // ind += 1;
    //if ind > startindex then {   }
    //println(" Reading filename #"++ ind-1 ++": "++path);
    //emit ws_readImage(dir ++ "/" ++ path);
    emit ws_readImage(path);
  }
}
*/

/*
 * Input a directory of images as a stream.
 */
fun stream_image_dir(dir) {
  files = scandir_stream(dir, timer(10));
  smap(fun(s) ws_readImage(dir ++ "/" ++ s), files)
}

/* 
 * Output a stream of images as files in a directory.
 * example: 
 */
fun image_files_sink(dir, name, strm) {
  iterate (img,wid,height) in strm {
    state { index = 0 }

    // Compute the number of channels!
    nChannels = Array:length(img) / wid*height;
    println("Writing image, "++ name ++" # "++ index ++" Number of channels: "++ nChannels);

    //println$ "    Width height in WS "++wid++ " "++height;
    outname = dir ++ "/" ++ name ++ index ++".jpg";
    if ws_writeImage(outname, img, wid, height, nChannels)
    then wserror("image_files_sink (opencv) failed to load image " ++ outname);
    index += 1;    
    emit (); // As an ACK, emit empty tuples.
  }
}
