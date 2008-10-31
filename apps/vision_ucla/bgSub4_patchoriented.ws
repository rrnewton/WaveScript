
/* 
  Teresa Ko's background subtraction code, ported from C++ to WS by Ryan Newton.
  [2008.06.27]

Version2:
  This greatly simplifies the code in bgsub.ws by factoring out repetitive portions.

Version3:
  This version represents the factored code in bgsub2, additionally
  modified to use integers rather than floating point numbers for
  histogram tallies.
    ALSO, Version3 abstracts the 3D array/matrix interface so that we
    can swap in multiple implementations.

Version4:
  This version refactors the code to abstract the image transforms so
  that they can be applied to the image in a data-parallel (tiled
  "patches") manner.

TODO:

Note:
[2008.10.31] takes ~60ms collection time out of 2.4s under boehm.

 */

include "stdlib.ws"
include "opencv.ws"
include "r100/r100.ws"

fullpath_in = GETENV("REGIMENTD") ++ "/apps/vision_ucla/input/FeederStation_2007-06-26_14-00-03.000";
//fullpath_in = GETENV("REGIMENTD") ++ "/apps/vision_ucla/input/hamster";
fullpath_out = GETENV("REGIMENTD") ++ "/apps/vision_ucla/processed/";

outfmt = "bmp"

LIVE = GETENV("LIVE") != ""

// Maybe I should make this builtin?
WSDEBUG = true

//====================================================================================================
/// Types and Constants:

type Color = Uint8;
type RawImage = Array Color; // Without the width/height metadata.  RGB interleaved.

// These images look just like rowmajor matrices.
//include "matrix-nested.ws"
include "matrix-rowmajor.ws"

type Image = (RawImage * Int * Int); // With width/height (cols/rows)
//type Image = Matrix Color; // With width/height (cols/rows)

type RGB = (Color * Color * Color);

// Application type defs:
type Inexact = Double; // Float or Double

abs  =  absD
ceil = ceilD 
sqrt = sqrtD // Need type classes!

DEBUG = true;

include "helpers.ws"
//include "patches.ws"
include "parmatrix.ws"

fun tryenv(var,default) {
  str = GETENV(var);
  if str == ""
  then default
  else stringToInt(str);
}

settings = (
		"/data/birdmotion/JR_webcam/FeederStation_2007-06-26_14-00-03.000/",  // Filename
		"../processed/FeederStation_2007-06-26_14-00-03.000/bhatta_default/", // OutLoc
		(396 :: Int), // BgStartFrame
		(0  :: Int),  // StartFrame
		(20 :: Int),  // NumBgFrames
		//tryenv("NUMBGFRAMES", 20),  // NumBgFrames
		(100 :: Int), // NumFgFrames

		(1   :: Int),    //BgStep
		(1   :: Int),    //FgStep
		(128 :: Color), // Threshold
										
		(3 :: Int),   // nChannels - RGB image
		
		//false    // useHSV
	      );
	
bhattasettings = (
		(16 :: Int),   // NumBins1
		(2  :: Int),   // NumBins2
		(2  :: Int),   // NumBins3
		(30 :: Int),   // SizePatch
		(0  :: HistElt), // Alpha (bg update)
		false // useBgUpdateMask 
	);


// TEMP:
// Unpack the settings, ugly because we don't have records yet --rrn	
let (NumBins1, NumBins2, NumBins3, SizePatch, Alpha, useBgUpdateMask) = bhattasettings;
let (Filename, OutLoc, BgStartFrame, FgStartFrame, 
     NumBgFrames, NumFgFrames, BgStep, FgStep, Threshold,
     //rows, cols, 
     nChannels) = settings;

//====================================================================================================
// These hooks allow us to go back and forth between the floating point version and the integer.

// rrn: note, it seems like the histograms could keep growing during the updateBg phase...
//type HistElt = Uint16;
type HistElt = Int;
sampleWeight1 :: HistElt = 1
sampleWeight2 :: HistElt = 1

/*
type HistElt = Inexact;
// To reduce divisions.  Adjust weight so that a pixel's histogram will be normalized after all frames are received.
sampleWeight1 = 1 / Inexact! (SizePatch * SizePatch * NumBgFrames);
sampleWeight2 = (Inexact! 1.0) / (SizePatch * SizePatch).gint;	
*/


//====================================================================================================

// A histogram for the viscinity around a pixel:
type PixelHist = Array3D HistElt;

 // To reduce divisions.  Used to take a pixel value and calculate the histogram bin it falls in.
 inv_sizeBins1 :: Inexact = 1 / ceil(256 / Inexact! NumBins1);
 inv_sizeBins2 :: Inexact = 1 / ceil(256 / Inexact! NumBins2);
 inv_sizeBins3 :: Inexact = 1 / ceil(256 / Inexact! NumBins3); // NOTE, if I replace gint with Inexact! I get a typechecking problem.

//	double inv_nPixels = 1.f/((double) (bSettings->SizePatch*bSettings->SizePatch*bSettings->SizePatch*bSettings->SizePatch*settings->NumBgFrames));

 // nPixels = the number of pixels in two patches in all the background frames.
 nPixels = (SizePatch * SizePatch * SizePatch * SizePatch * NumBgFrames);
 inv_nPixels = 1 / Inexact! nPixels;

 halfPatch :: Int = SizePatch / 2;

// Actually, workers is this bhattaWorkers squared:
bhattaWorkers = {
  n = if GETENV("NUMCPUS") == ""
      then 4
      else stringToInt(GETENV("NUMCPUS"));
  Int! sqrtF(Float! n);
}

_ = {
  println$ "Some metaprogram-time values: \n";
  println$ "Number of Workers = "++ bhattaWorkers * bhattaWorkers ++"\n";

  println$ "  inv_sizeBins: "++(inv_sizeBins1, inv_sizeBins2, inv_sizeBins3);
  println$ "  sampleWeight1,2: "++ (sampleWeight1, sampleWeight2);
  println$ "";

  println$ "  nPixels: "++ (nPixels);
  println$ "  inv_nPixels: "++ (inv_nPixels);
  }

//====================================================================================================
// Factoring pieces of the below functions into these helpers:

// When we are near the edge of the image, patches may go off the
// edge, we reflect them back on themselves.
fun boundit(x,range) {
  if x < 0 then 0-x-1 else
  if x >= range then 2*range-1-x else x;
};

// Update the correct bin within a pixel's histogram, indexed by R/G/B.
hist_update :: (Color, Color, Color, PixelHist, HistElt -> HistElt) -> ();
fun hist_update(r,g,b, hist, fn) {
  // figure out which bin
  binB = Int! (Inexact! b * inv_sizeBins1);
  binG = Int! (Inexact! g * inv_sizeBins2);
  binR = Int! (Inexact! r * inv_sizeBins3);
  // apply transform to histogram  
  using Array3D;
  set(hist, binB, binG, binR, fn(get(hist, binB, binG, binR)));
}

// Do the first patch in an image, fill in the histogram using all the color values.
fun initPatch(r,c, rows, cols, patchbuf, image, sampleWeight) {
  // clear patch histogram:
  Array3D:fill(patchbuf, 0);
  roEnd = r - halfPatch + SizePatch;  // end of patch
  coEnd = c - halfPatch + SizePatch;  // end of patch
  for ro = r-halfPatch to roEnd-1 { // cover the row
      roi = boundit(ro,rows);
      for co = c-halfPatch to coEnd-1 { // cover the col
	  coi = boundit(co,cols);	  
	  i = (roi * cols + coi) * 3;   // get the pixel location
	  hist_update(image[i+2], image[i+1], image[i], patchbuf, 
	              (+ sampleWeight));
	}
    }
}

// This runs down a column of a patch, reading color values and
// applying a transform to the histogram (not the image).
fun zipDownward(rowIndex, rows,cols, tempHist, image)
  fun(columnIndex, fn) {
    coi = boundit(columnIndex,cols);
    for ro = rowIndex - halfPatch to rowIndex-halfPatch + SizePatch - 1 {
	roi = boundit(ro, rows);
	i = (roi * cols + coi) * 3;	  
	hist_update(image[i+2], image[i+1], image[i], tempHist, fn);
    }
  }

// Shift a patch one pixel right, update the histogram incrementally.
shift_patch :: (Int, Int, Int, Int, PixelHist, RawImage, HistElt) -> ();
fun shift_patch(r,c, rows,cols, hist, image, sampleWeight) {
  zippy = zipDownward(r, rows,cols, hist, image);
  // subtract left col       
  co = c - halfPatch - 1;
  if true
  then zippy(co, fun(x)     x - sampleWeight)
  else zippy(co, fun(x) max(x - sampleWeight, 0));
  // add right col
  co = c - halfPatch + SizePatch - 1;
  zippy(co, fun(x) x + sampleWeight);
}

fun add_into3D(dst,src) Array3D:map_inplace2(dst, src, (+));

// This just walks over a matrix and gives you both the row/column
// index, and the flat array index.
fun loop2D(rows,cols, fn) {
  index = Mutable:ref(0);
  for r = 0 to rows-1 {
    for c = 0 to cols-1 {
      fn(r,c, index);
      index += 1;
  }}}

// TODO: Replace this with a call into our actual matrix library:
fun matrix_foreachi(mat, rows,cols, fn) 
  loop2D(rows,cols, fun(r,c,i) fn(r,c,mat[i]))

// This converts interleaved rgb values into tuples.
// Under WSC2 it should be a noop, could take advantage of that as a performance hack.
image_to_matrix :: Image -> Matrix RGB; 
fun image_to_matrix((arr,wid,hgt)) {
  using Matrix;
  mat = create(hgt,wid, (0,0,0));
  wid3 = wid*3;
  // We are keeping it in "row-major" form, so x--j & y--i
  
  Matrix:build(hgt,wid, fun(i,j) {
          ind = i*wid3 + 3*j;
	  (arr[ind+2], arr[ind+1], arr[ind])
        });
 /*for i = 0 to hgt-1 {
    offset = i*wid3;
    j = 0;
    while j < wid {      
      ind = offset + 3*j;
      set(mat,i,j, (arr[ind+2], arr[ind+1], arr[ind]));
      j += 3;
    };    
    };
  mat  
*/
}

matrix_to_image :: Matrix RGB -> Image;
fun matrix_to_image(mat) {
  using Matrix;
  let (r,c) = mat.dims;
  //arr = Array:makeUNSAFE(3*r*c);
  arr = Array:make(3*r*c, 0);
  ind = 0;
  Matrix:foreachi(fun(i,j,(r,g,b)) {
    ind = (i*c + j) * 3;
    //if (r,g,b) == (0,0,0)  then print("?");
    arr[ind+2] := r;
    arr[ind+1] := g;
    arr[ind]   := b;
  }, mat);
  (arr,c,r)
}


//====================================================================================================
// Here's the new, pixel-level data parallel version.

// In its first incarnation it will not be able to take advantage of incremental computation of histograms.


populatePixHist :: (PixelHist, Int, (Int, Int) -> RGB) -> ();
fun populatePixHist(hist, sampleWeight, nbrhood) {
  //Array3D:fill(hist, 0);
  for i = 0-halfPatch to halfPatch-1 {
    for j = 0-halfPatch to halfPatch-1 {
      let (r,g,b) = nbrhood(i,j);
      hist_update(r,g,b, hist, (+ sampleWeight));
    }
  }
}

// Compare a pixels surrounds to its historical surroundings and compute a diff and a mask.

estimateFgPix :: (PixelHist, PixelHist, (Int, Int) -> RGB) -> (Uint8 * Uint8);
fun estimateFgPix(tempHist, bgHist, nbrhood) {
  // Compute a histogram for the current frame.
  populatePixHist(tempHist, sampleWeight2, nbrhood);
  // compare histograms
  diff = Array3D:fold2(tempHist, bgHist, 0,
  	               fun(acc,px,bg) acc + sqrt(Inexact! (px * bg) * inv_nPixels));
  // renormalize diff so that 255 = very diff, 0 = same
  diffImage = Uint8! (255 - Int! (diff * 255)); // create result image
  mask = if diffImage > Threshold then 255 else 0; // Inefficient...
  (diffImage, mask)
}

// This will create a stream of mask images.
bhattaPixKern :: Stream (Bool * Matrix RGB) -> Stream (Matrix (Color * Color));
bhattaPixKern = {
 fun box(v) Array:make(1,v);
 fun unbox(b) b[0];
 fun set(b,x) b[0] := x;
 tagged_pixel_kernel_with_neighborhood(bhattaWorkers,bhattaWorkers, halfPatch,
   // This is the work function at each pixel.
   fun(bgEstimateMode, bghist,px,nbrhood) {
     // This alloc could be lifted:
     tempHist = Array3D:make(NumBins1, NumBins2, NumBins3, 0);
     if bgEstimateMode  then {
       populatePixHist(bghist, sampleWeight1, nbrhood);
       (128,0) // This is not nice, but we must output something.
     } else {
       estimateFgPix(tempHist, bghist, nbrhood); // (diff,mask)
     }
   },
   // This initializes the per-pixel state:
   fun(i,j) Array3D:make(NumBins1, NumBins2, NumBins3, 0))
}

// This controls when we are in background profiling vs. background
// subtraction mode.  It sends a flag downstream to the components
// that do the actual work.
fun tagWithMode(strm) {
  iterate mat in strm {
    state {
            bgEstimateMode = true; 
            FrameIndex = BgStartFrame;
            bgStaleCounter = 0;
            stopFrame = BgStartFrame + NumBgFrames * BgStep;
          }
    if FrameIndex == BgStartFrame then {
      println$ "Output location: "++OutLoc;
      println$ "Settings: ";
      println$ " # of bins:             "++ NumBins1 ++","++ NumBins2 ++","++ NumBins3;
      println$ " Size of patch:         "++ SizePatch;
      println$ " # of Bg Frames:        "++ NumBgFrames;
      println$ " Threshold:             "++ Threshold;
      println$ " Alpha:                 "++ Alpha;
      println$ "Image rows/cols: "++ Matrix:dims(mat);
      println$ "  Allocating global arrays...";
      println("Building background model...");      
    };
    if bgEstimateMode then {
	  FrameIndex += 1;
          if FrameIndex == stopFrame then {
            bgEstimateMode := false;
            FrameIndex := FgStartFrame;
	    //println("\nFinished background model, extracting foreground.\n");
            stopFrame := FgStartFrame + NumFgFrames * FgStep;
	  };
    } else { 
      //if (FrameIndex == FgStartFrame)  then  println$ "Calling estimate... frame size "++ Matrix:dims(mat);
      FrameIndex += FgStep;
    };
     emit (bgEstimateMode, mat)
  }
}



//====================================================================================================
//   Main Stream Program
//====================================================================================================

// This is all the nonsense about loading files and scaling images.

// This is an ugly separation right now.  The frame index stream
// drives the reading of files, but the Bhatta kernel below must stay
// synchronized with this input stream (in terms of switching between
// background frames and foreground frames).
//
// TODO: this index stream should simply emit a union type tagging elements as Bg or Fg.
index_stream :: Stream Int;
index_stream = iterate _ in timer(2) {
  state { bgcnt = 0;
          bgind = BgStartFrame;
          fgcnt = 0;
          fgind = FgStartFrame;
        }
  assert_eq("only works for bgstep 1 at the moment", BgStep, 1);
  assert_eq("only works for fgstep 1 at the moment", FgStep, 1);

  if bgcnt < NumBgFrames then {    
    emit bgind;
    bgcnt += 1;
    bgind += 1;
  } else {
    emit fgind;
    fgcnt += 1;
    fgind += 1;
  }
}

filenames :: Stream String;
filenames = iterate ind in index_stream {
  state { nametable = Array:null }
  if nametable == Array:null then {
    nametable := scandir(fullpath_in);
  };
  if ind >= Array:length(nametable) 
  then wserror(" Tried to index file "++ind++" within directory -- doesn't exist!");
  emit fullpath_in ++ "/" ++ nametable[ind];
}

fun dump_files(strm) 
  iterate (ind, (frame,cols,rows), diffImage, mask) in strm {
    using Array;
    ws_writeImage(fullpath_out++"/Fg_"++ind++"."++outfmt, frame, cols, rows, 3);    
    
    fun get_dims(len,chans) {
      // Hack, we assume the aspect ratio is the same as the frame, but the size may not be.
      orig = Double! (cols*rows);
      aspect = Double! cols / Double! rows;
      c = sqrtD(Double! (len / chans) * aspect);
      r = c / aspect;
      (Int! c, Int! r)
    };
    if diffImage.length > 0 then { 
      let (c,r) = get_dims(diffImage.length, 3);
      ws_writeImage(fullpath_out++"/Diff_"++ind++"."++outfmt, diffImage, c, r, 1); ()
     };
    if mask.length > 0 then {
      let (c,r) = get_dims(mask.length, 1);
      ws_writeImage(fullpath_out++"/Mask_"++ind++"."++outfmt, mask, c, r, 1); () 
    };
    emit ();
  }

simple_dump :: Stream Image -> Stream ();
fun simple_dump(strm) 
  iterate (frame,cols,rows) in strm {
    state { ind = 0 }    
    ws_writeImage("./"++ind++".jpg", frame, cols, rows, 3);
    ind += 1;
    emit ();
  }

dump_matrix :: Stream (Matrix Color) -> Stream ();
fun dump_matrix(strm) 
  iterate mat in strm {
    state { ind = 0 }    
    using Matrix;
    let (rows,cols) = mat.dims;
    frame = mat.toArray;
    ws_writeImage("./"++ind++".jpg", frame, cols, rows, 3);
    ind += 1;
    emit ();
  }

// Select out the FG image and display that:
fun my_display(strm) 
     display_to_screen(smap(fun((_, (fg,c,r), diff, mask)) (fg,c,r), strm))


// Half both dimensions:
// Throw out 3 pixels out of each square of 4.
fun squisher(strm) 
  iterate (arr,cols,rows) in strm {
    using Array;
    halfrow = rows/2;
    halfcol = cols/2;
    new = makeUNSAFE(halfrow * halfcol * 3);
    for c = 0 to halfcol-1 {
    for r = 0 to halfrow-1 {
       ind0 = (r*cols*2 + c*2) * 3;
       ind1 = (r*halfcol + c) * 3;
       new[ind1+0] := arr[ind0+0];
       new[ind1+1] := arr[ind0+1];
       new[ind1+2] := arr[ind0+2];
     }
    };
    emit (new, halfcol, halfrow);
  }

// Double both dimensions:
unsquisher :: Stream OutputBundle -> Stream OutputBundle;
fun unsquisher(strm) {
  fun blowup(arr,cols,rows) {

    println("Blowing up from size "++cols++" by "++rows);

    using Array;
    dubcol = cols*2;
    //new :: Array Color = makeUNSAFE(dubcol * rows * 6); 
    new = make(dubcol * rows * 6, 0); 
    for c = 0 to cols-1 {
    for r = 0 to rows-1 {
       i0 = (r*cols + c) * 3;
       blue  = arr[i0+0];
       green = arr[i0+1];
       red   = arr[i0+2];

       i1 = (r*dubcol*2 + c*2) * 3;
       new[i1+0] := blue;
       new[i1+1] := green;
       new[i1+2] := red;
       new[i1+3] := blue;
       new[i1+4] := green;
       new[i1+5] := red;

       i2 = i1 + (dubcol*3);
       new[i2+0] := blue;
       new[i2+1] := green;
       new[i2+2] := red;
       new[i2+3] := blue;
       new[i2+4] := green;
       new[i2+5] := red;
     }
    };
    new
  };
  iterate (ind, (arr,cols,rows), diffImage, mask) in strm {
    emit (ind, (blowup(arr,cols,rows), cols*2, rows*2), 
          blowup(diffImage,cols,rows), 
	  mask);
  }
}



//====================================================================================================

type OutputBundle = (Int * Image * RawImage * RawImage);

// Main stream script:

input_imgs :: Stream Image;
output_imgs :: Stream OutputBundle -> Stream ();

input_imgs  = if LIVE then front_camera else smap(ws_readImage, filenames)
output_imgs = if LIVE then my_display else dump_files;


// Sadly, without Chez we can't currently call C functions and read the image files in at profile time.
fakeFrames = iterate _ in timer$3 {
  using Array;
  //let (cols,rows) = (320, 240);
  let (cols,rows) = (32, 24);
  //let (cols,rows) = (2, 6);

  arr = build(rows * cols * 3, fun(i) (1::Color));
  emit (arr, cols, rows);
}


last_time = 0;

//main = simple_dump $ Curry:smap(fun(mat) ) $ kern $ mats
diffsAndMasks = 
   bhattaPixKern $
   tagWithMode $ 
   Curry:smap(image_to_matrix) $
   input_imgs

main = 
      simple_dump $ 
      Curry:smap(fun(x) { t = realtime(); println("Time elapsed: "++t-last_time); last_time := t; x}) $
      Curry:smap(matrix_to_image) $
      Curry:smap(fun(m) Matrix:map(fun((x,y)) (x,y,0), m)) $
      diffsAndMasks

      //Curry:smap(fun(_) ()) $ 
      //dump_matrix $ 
      //Curry:smap(printmat$"\n\n ** Restitched ") $
           
      //Curry:smap(inspect) $       
      //pxkern $ 
      //kern $
      //mats

      //fakeFrames
