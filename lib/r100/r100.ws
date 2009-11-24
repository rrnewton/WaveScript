

include "stdlib.ws"


// These have the 3 RGB channels interleaved, together with width/height:
type R100FlatImage = ((Array Uint8) * Int * Int);
type R100Image     = ((Array (Uint8 * Uint8 * Uint8))   * Int * Int);

// This is a stream of frames from the front camera.
// Frames are arrays of uint16s.
//
// Only one of these streams exists.  The user cannot instantiate multiple camera streams.
front_camera :: Stream R100FlatImage;
front_camera = {
  file = ["r100/front_camera.c"];
  // This is a bit of a wacky interface.
  getwidth  :: () -> Int = foreign("cam_width",  file);
  getheight :: () -> Int = foreign("cam_height", file);
  //set_cam_scratch :: Array Uint16 -> () = foreign("set_cam_scratch", file);
  set_cam_scratch :: Array Uint8 -> () = foreign("set_cam_scratch", file);
  camera_ticks = (foreign_source("ws_camera_hookup", file)
                  :: Stream ());
                  //:: Stream (Array (Array Uint8)));

  // The C side sends ticks.  We use a hack to allocate a single
  // static frame buffer on the WS heap and pass it to C.
  // The C side will send us an extra initial tick to let us initialize.
  iterate tick in camera_ticks {
    state { cambuf = Array:null;  // Start as a null array.
            wid = 0; height = 0; } 
    if cambuf == Array:null then {
      wid    := getwidth();
      height := getheight();
      // On our very first tick we allocate the buffer, that is it.
      print$ "Allocating camera buffer "++ wid ++" by "++ height ++"\n";
      //cambuf := Array:make(wid * height * 3, (0 :: Uint8));
      cambuf := Array:make(wid * height * 3, (0::Uint8));
      // Register the buffer once and for all with C.
      set_cam_scratch(cambuf);
      print$ "Registered camera buffer with C.\n";
    } else {
      // It definitely gets filled up on the first frame.
      //emit Array:sub(cambuf, 1000, 10);
      //emit Array:fold((+), 0, cambuf);
      //print$ "Size of cambuf : "++Array:length(cambuf) ++"\n";
      emit (cambuf, wid, height);
    };
  }
}

// This simple screen interface takes a stream of frames as input, and
// displays them to the phone's lcd.  It produces a stream of empty
// tuples that signify the completion of screen updates.
display_to_screen   :: Stream R100FlatImage -> Stream ();
display_to_screen = {
  //c_fun :: (Array Uint16) -> () = foreign("display_to_screen", ["front_camera.c"]);
  c_fun :: (Array Uint8) -> () = foreign("display_to_screen", ["r100/front_camera.c"]);
  fun (imgs) 
  iterate (frame,wid,height) in imgs {
      c_fun(frame);
      emit ();
    }
}


/*
// When we get frames they are of the form RRRRRRGGGGGGBBBBB.
// We want to group those pixels into (R,G,B) tuples.
fun interleave_frame(arr) {
  total = Array:length(arr);
  len = total / 3;
  len2 = 2 * len;
  Array:build(len, fun(i) {
    R = arr[i];
    G = arr[i + len];
    B = arr[i + len2];
    (R,G,B)
  })
}

fun deinterleave_frame(arr) {
}
*/

// Pack the color values into structs.  Really, given C
// representations this is an identity function on the bits.
// Inefficient.
//tuple_pixels :: Array Uint8 -> Array (Uint8 * Uint8 * Uint8);
tuple_pixels :: R100FlatImage -> R100Image;
fun tuple_pixels((arr,w,h)) {
 len = Array:length(arr) / 3;
 newarr = 
  Array:build(len, fun (i) {
    i3 = i * 3;
    R = arr[i3];
    G = arr[i3 + 1];
    B = arr[i3 + 2];
    (R,G,B)
 });
 (newarr,w,h)
}

//untuple_pixels :: Array (Uint8 * Uint8 * Uint8) -> Array Uint8;
untuple_pixels :: R100Image -> R100FlatImage;
fun untuple_pixels((arr,w,h)) {
 using Mutable; using Array;
 newarr = makeUNSAFE(3 * length(arr));
 for i = 0 to length(arr) - 1 {
   i3 = i * 3;
   let (r,g,b) = arr[i];
   newarr[i3]   := r;
   newarr[i3+1] := g;
   newarr[i3+2] := b;
 };
 (newarr,w,h)
}

tweak_pixel :: (Uint8 * Uint8 * Uint8) -> (Uint8 * Uint8 * Uint8);
fun tweak_pixel((r,g,b)) 
//   (r,g,b)
//   (r,255,b)
//   (r + 20, g, b)
//   (r, g * 2, b)
//   (r, min(g * 2, 250), b)
//   (r, if 2*g > 250 then 250 else 2*g, b)
//   (r, if 2*g > 250 then g else 2*g, b)
//   (r, if g > 100 then 0 else g, b)
   if g > 120 then (r,g,b) else (r,2*g,b)

// (b,g,r)
//   (g,g,g)
//   (intToChar$ 0, intToChar$ 64, intToChar$ 0)
//   (intToChar$ 20, intToChar$ 20, intToChar$ 20)
//   (intToChar$ 100, intToChar$ 100, intToChar$ 100)
//   (intToChar$ 150, intToChar$ 150, intToChar$ 150)
//   (intToChar$ 200, intToChar$ 200, intToChar$ 200)
// White:
//   (intToChar$ 255, intToChar$ 255, intToChar$ 255)

//   (intToChar$ 256, intToChar$ 256, intToChar$ 256)


include "opencv.ws"

main = {

  fullpath_in = GETENV("REGIMENTD") ++ "/apps/vision_ucla/input/FeederStation_2007-06-26_14-00-03.000";
  fullpath_out = GETENV("REGIMENTD") ++ "/apps/vision_ucla/processed/";

  using Curry;

  LIVE = true;

  acquire_imgs = if LIVE then front_camera      else stream_image_dir(fullpath_in);
  output_imgs  = if LIVE then display_to_screen else fun(strm) image_files_sink(fullpath_out, "out", strm);

  mainstrm = 
     //smap(fun(arr) Array:map(charToInt, Array:sub(arr, 10000, 10)))
     output_imgs

     // $ smap(amapi(invert_color)) 

       $ smap(untuple_pixels)
       $ smap(fun((arr,w,h)) {
         print $ Array:sub(arr, 100, 15);
			   
	 print("\n");
         new = Array:map(tweak_pixel, arr);

         print $ Array:sub(new, 100, 15);
	 print("\n");
	 (new,w,h)
       })
     $ smap(tuple_pixels)

     $ acquire_imgs;

  mainstrm
}
