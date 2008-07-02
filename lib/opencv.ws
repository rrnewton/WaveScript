/* 

 Here I will accumulate wrappers to useful functions within OpenCV

 .author Ryan Newton

 */

//include "stdlib.ws"
include "unix.ws"

// Image loading and saving:





imgs = scandir_stream("/home/newton/wavescript_unison/apps/vision_ucla/input/FeederStation_2007-06-26_14-00-03.000", 
                      timer(3))

main = iterate frame in imgs {
  //println$ "Got frame!! length "++ Array:length(frame);
  println$ "length "++ String:length(frame);
  emit frame;
}
