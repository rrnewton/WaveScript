

include "stdlib.ws"

// This is a stream of frames from the front camera.
// Frames are arrays of uint16s.
//
// Only one of these streams exists.  The user cannot instantiate multiple camera streams.
front_camera = {
  file = ["front_camera.c"];
  // This is a bit of a wacky interface.
  getwidth  :: () -> Int = foreign("cam_width",  file);
  getheight :: () -> Int = foreign("cam_height", file);
  //set_cam_scratch :: Array Uint16 -> () = foreign("set_cam_scratch", file);
  set_cam_scratch :: Array Char -> () = foreign("set_cam_scratch", file);
  camera_ticks = (foreign_source("ws_camera_hookup", file)
                  :: Stream ());
                  //:: Stream (Array (Array Char)));

  // The C side sends ticks.  We use a hack to allocate a single
  // static frame buffer on the WS heap and pass it to C.
  // The C side will send us an extra initial tick to let us initialize.
  iterate tick in camera_ticks {
    state { cambuf = Array:null } // Start as a null array.
    if cambuf == Array:null then {
      wid    = getwidth();
      height = getheight();
      // On our very first tick we allocate the buffer, that is it.
      print$ "Allocating camera buffer "++ wid ++" by "++ height ++"\n";
      //cambuf := Array:make(wid * height * 3, (0 :: Char));
      cambuf := Array:make(wid * height * 3, intToChar(0));
      // Register the buffer once and for all with C.
      set_cam_scratch(cambuf);
      print$ "Registered camera buffer with C.\n";
    } else {
      // It definitely gets filled up on the first frame.
      //emit Array:sub(cambuf, 1000, 10);
      //emit Array:fold((+), 0, cambuf);
      print$ "Size of cambuf : "++Array:length(cambuf) ++"\n";
      emit cambuf;
    };
  }
}

// This simple screen interface takes a stream of frames as input, and
// displays them to the phone's lcd.  It produces a stream of empty
// tuples that signify the completion of screen updates.
display_to_screen = {
  //c_fun :: (Array Uint16) -> () = foreign("display_to_screen", ["front_camera.c"]);
  c_fun :: (Array Char) -> () = foreign("display_to_screen", ["front_camera.c"]);
  fun (frames) 
    iterate frame in frames {
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
fun tuple_pixels(arr) {
 len = Array:length(arr) / 3;
 Array:build(len, fun (i) {
    i3 = i * 3;
    R = arr[i3];
    G = arr[i3 + 1];
    B = arr[i3 + 2];
    (R,G,B)
 })
}

fun untuple_pixels(arr) { 
 using Mutable; using Array;
 newarr = makeUNSAFE(3 * length(arr));
 for i = 0 to length(arr) - 1 {
   i3 = i * 3;
   let (r,g,b) = arr[i];
   newarr[i3]   := r;
   newarr[i3+1] := g;
   newarr[i3+2] := b;
 };
 newarr
}

fun invert_color(ind, c) {  
  //if ind > 240 * 320 
  if (moduloI(ind, 3) == 2) // 0 red, 1 green, 2 blue
  then {
    c //intToChar(0)
  } 
  else //c
    intToChar(0)
}

fun tweak_pixel((r,g,b)) 
   (r,g,b)
// (intToChar$ charToInt(r) + 20, g, b)
//  (r, intToChar$  min(charToInt(g) * 2, 255), b)
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



using Curry;

main = 
     //smap(fun(arr) Array:map(charToInt, Array:sub(arr, 10000, 10)))
     display_to_screen 
       $ smap(untuple_pixels)
     // $ smap(amapi(invert_color)) 

     $ smap(fun(arr) {
         print $ Array:map(fun((x,y,z)) (charToInt(x), charToInt(y), charToInt(z)),
			   Array:sub(arr, 100, 15));
	 print("\n");
         new = Array:map(tweak_pixel, arr);

         print $ Array:map(fun((x,y,z)) (charToInt(x), charToInt(y), charToInt(z)),
			   Array:sub(new, 100, 15));
	 print("\n");
	 new
       })

     $ smap(tuple_pixels)
     $ front_camera;
