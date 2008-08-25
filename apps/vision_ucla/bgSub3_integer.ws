
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

TODO:


 */

include "stdlib.ws"
include "opencv.ws"
include "r100/r100.ws"

fullpath_in = GETENV("REGIMENTD") ++ "/apps/vision_ucla/input/FeederStation_2007-06-26_14-00-03.000";
//fullpath_in = GETENV("REGIMENTD") ++ "/apps/vision_ucla/input/hamster";
fullpath_out = GETENV("REGIMENTD") ++ "/apps/vision_ucla/processed/";

outfmt = "bmp"

LIVE = false;

//====================================================================================================
/// Types and Constants:

type Color = Uint8;
type RawImage = Array Color; // Without the width/height metadata.
type Image = (RawImage * Int * Int); // With width/height (cols/rows)

// Application type defs:
type Inexact = Double; // Float or Double

abs  =  absD
ceil = ceilD 
sqrt = sqrtD // Need type classes!

DEBUG = true;

include "helpers.ws"

settings = (
		"/data/birdmotion/JR_webcam/FeederStation_2007-06-26_14-00-03.000/",  // Filename
		"../processed/FeederStation_2007-06-26_14-00-03.000/bhatta_default/", // OutLoc
		(396 :: Int), // BgStartFrame
		(0  :: Int),  // StartFrame
		(20 :: Int),  // NumBgFrames
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


_ = {
  println$ "Some metaprogram-time values: \n";
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

// I tried this to make the sliding patch cheaper... but I did not get
// it right (output was not the same).  Also, it wasn't faster.  The
// bottleneck must be from accessing the histograms, not the images.
// Actually, it was slower.
/*
// This runs down a column of a patch, reading color values and
// applying a transform to the histogram (not the image).
fun zipAcross(columnIndex, rows,cols, tempHist, image)
  fun(rowIndex, fn) {
    roi = boundit(rowIndex,cols);
    // Run across in-grain:
    for co = columnIndex - halfPatch to columnIndex-halfPatch + SizePatch - 1 {
	coi = boundit(co, rows);
	i = (roi * cols + coi) * 3;
	hist_update(image[i+2], image[i+1], image[i], tempHist, fn);
    }
  }

// Shift a patch one pixel right, update the histogram incrementally.
fun downshift_patch(r,c, rows,cols, hist, image, sampleWeight) {
  zippy = zipAcross(c, rows,cols, hist, image);
  // subtract upper row
  ro = r - halfPatch - 1;
  zippy(ro, fun(x) max(x - sampleWeight, 0));			
  // add lower row
  ro = r - halfPatch + SizePatch - 1;
  zippy(ro, fun(x) x + sampleWeight);
}

fun loop2D_unnatural_order(rows,cols, fn) {
  for c = 0 to cols-1 {
   index = Mutable:ref(c);
   for r = 0 to rows-1 {
      fn(r,c, index);
      index += cols;
  }}}

fun matrix_foreachi_unnatural_order(mat, rows,cols, fn) 
  loop2D_unnatural_order(rows,cols, fun(r,c,i) fn(r,c,mat[i]))
*/

//====================================================================================================

// Builds background model histograms for each pixel.  
// Additions to the histograms are scaled according the assumption that 
// it will receive NumBgFrames # of images.
populateBg :: (PixelHist, Array PixelHist, Image) -> ();
fun populateBg(tempHist, bgHist, (image,cols,rows)) {
  using Array; using Mutable;
  assert_eq("Image must be the right size:",length(image), rows * cols * 3);

  // Patches are centered around the pixel.  [p.x p.y]-[halfPatch halfPatch] gives the upperleft corner of the patch.				

  // Histograms are for each pixel (patch).  First create a histogram of the left most pixel in a row.
  // Then the next pixel's histogram in the row is calculated by:
  //   1. removing pixels in the left most col of the previous patch from the histogram and 
  //   2. adding pixels in the right most col of the current pixel's patch to the histogram	

  // This makes a strong assumption about the order the matrix is traversed.
  // It's not representation-independent in that sense.
  matrix_foreachi(bgHist, rows,cols, 
    fun(r,c, bgHist_rc) {
      if c==0 then  initPatch(r,c, rows,cols, tempHist, image, sampleWeight1)
      else        shift_patch(r,c, rows,cols, tempHist, image, sampleWeight1);
      add_into3D(bgHist_rc, tempHist);   // copy temp histogram to left most patch
    })
}


//====================================================================================================

// Estimate foregound pixel from "image" and the background model. 
// "diffImage" visualizes the difference as measured by the bhattacharyya distance between the current image's
// pixel and the background model pixel.
// "mask" is an image where white pixels represent the foreground and black pixels represents the background
// according to Threshold

estimateFg :: (PixelHist, Array PixelHist, Image, RawImage, RawImage) -> ();
fun estimateFg(tempHist, bgHist, (image,cols,rows), diffImage, mask) {
   using Array; using Mutable;
   fill(mask, 0); // clear mask image

   // as in the populateBg(), we compute the histogram by subtracting/adding cols	
   loop2D(rows,cols,      fun(r,c, index) {
      if c==0 then  initPatch(r,0, rows,cols, tempHist, image, sampleWeight2)
      else        shift_patch(r,c, rows,cols, tempHist, image, sampleWeight2);

      // compare histograms
      diff = Array3D:fold2(tempHist, bgHist[index], 0,  
                           //fun(acc,px,bg) acc + sqrt(Inexact! px * Inexact! bg)
			   fun(acc,px,bg) acc + sqrt(Inexact! (px * bg) * inv_nPixels)
			   );
      // renormalize diff so that 255 = very diff, 0 = same
      diffImage[index] := Uint8! (255 - Int! (diff * 255)); // create result image
      // Inefficient:
      mask[index] := if diffImage[index] > Threshold then 255 else 0;         
    })
}

//====================================================================================================


// update background
// Two types happen:
// If a mask is not given, all pixels are updated.
// If a mask is given, only pixels in the diffImage that have a lower value than 
//    Threshold (are background) are updated.
// 
// degrade the background model by scaling each bin by 1-bSettings->Alpha
// add new pixel values scaled so that the resulting background model will sum to 1.
updateBg :: (PixelHist, Array PixelHist, Image, RawImage) -> ();
fun updateBg(tempHist, bgHist, (image,cols,rows), mask) 
  if Alpha == 0 then () else {
    using Array; using Mutable;
    if mask == null then println$ "Mask not given: updating everything";
    Array3D:fill(tempHist, 0);
	
    // iterate through all pixel's histograms
    // rescale the histogram only if there is a mask given
    // Modifies bgHist:
    for i = 0 to rows * cols - 1 {
      // if no mask provided, update all pixels
      // if mask is provided and the pixel in the mask indicates background, update
      // NOTE! RRN: THIS CODE IS CURRENTLY UNTESTED::
      if mask == null || mask[i] == 0 then {
	sum :: Ref Inexact = ref$ 0;
	Array3D:map_inplace(bgHist[i],
          fun(bh) {
	    sum += Inexact! bh;
	    bh * (1 - Alpha);
          });
	// make sure histogram still sums up correctly.
	//if sum - (1-Alpha) > (Inexact! 0.00001) // 10e-5
	//then wserror$ "ERROR1: bgHist is not normalized properly: sum = " ++ sum;
	()
      }
    };

    loop2D(rows,cols,      fun(r,c, index) {
      if c==0 then  initPatch(r,0, rows,cols, tempHist, image, sampleWeight2)
      else        shift_patch(r,c, rows,cols, tempHist, image, sampleWeight2);
	// we compute each histogram regardless of the mask 
	// under the assumption that the foreground is smaller than the background
	// it seemed easier to do this than a hybrid histogram computation that skipped
	// pixels.

      // update if indicated to do so
      // NOTE! RRN: THIS CODE IS CURRENTLY UNTESTED::
      if mask == null || mask[index] == 0 then {
	 sum = ref$ 0;
	 using Array3D;
	 iter( bgHist[index], fun(cb,cg,cr) {  // or map2_inplace3D
	     set(bgHist[index], cb,cg,cr, (Alpha * get(tempHist,      cb,cg,cr)) +
	     	                                   get(bgHist[index], cb,cg,cr));
	    // Naughty!  This needn't use inexact.
	    sum += Inexact! get(bgHist[index], cb,cg,cr);
	 });
	 if abs(sum - 1) > (Inexact! 0.00001)
	 then wserror$ "ERROR2: bgHist not normalized properly: sum  = "++ sum;
      }
    })
  }


//====================================================================================================
//   Complete Bhatta function
//====================================================================================================

type OutputBundle = (Int * Image * RawImage * RawImage);

bhatta :: Stream Image -> Stream OutputBundle;
fun bhatta(video) {
  println("Bhattacharyya Differencing.");
    
  //SHELL("mkdir ");

  using Array;
  //bghist = Mutable:ref$ null; // Same error.

  iterate (frame,cols,rows) in video {
    state { 
            bgEstimateMode = true; 
            FrameIndex = BgStartFrame;
            bgStaleCounter = 0;
            stopFrame = BgStartFrame + NumBgFrames * BgStep;
	    
	    // Here is the main storage:
            bghist    = null;

	    // create temporary patch to store working histogram 
	    temppatch = Array3D:null;
	    mask      = null;  // Just one channel.
	    diffImage = null;  // All three channels.
          }

    if bghist == null then {
      println$ "Output location: "++OutLoc;
      println$ "Settings: ";
      println$ " # of bins:             "++ NumBins1 ++","++ NumBins2 ++","++ NumBins3;
      println$ " Size of patch:         "++ SizePatch;
      println$ " # of Bg Frames:        "++ NumBgFrames;
      println$ " Threshold:             "++ Threshold;
      println$ " Alpha:                 "++ Alpha;

      println$ "Image rows/cols: "++ rows ++", "++ cols;
      println$ "  Allocating global arrays...";
      bghist := build(rows*cols, fun (_) Array3D:make(NumBins1, NumBins2, NumBins3, 0));
      temppatch := Array3D:make(NumBins1, NumBins2, NumBins3, 0);

      mask        := make(rows * cols, 0);
      diffImage   := make(rows * cols * nChannels, 0);
      //ImageBuffer := make(rows * cols * nChannels, 0);

      println("Building background model...");      
    };

    if bgEstimateMode then {
	  // Get input frame
	  //InputStream->GetFrame(FrameIndex,ImageBuffer);
	  // add frame to the background
          st = clock();
	  populateBg(temppatch, bghist, (frame,cols,rows));

	  //println$ "MAX hist "++      Array:fold(fun(best, hist) Array3D:fold(hist, best, max), 0, bghist);

	  en = clock();
	  println$ "Computation time for populateBg(): " ++ (en - st);

	  FrameIndex += 1;

          if FrameIndex == stopFrame then {
            bgEstimateMode := false;
            FrameIndex := FgStartFrame;
	    println("\nFinished background model, extracting foreground.\n");
            stopFrame := FgStartFrame + NumFgFrames * FgStep;

	    //println$ "SUM hist "++ Array:fold(fun(sum, hist) Array3D:fold(hist, sum, (+)), 0, bghist);
	  };

	  //emit (0, (frame,cols,rows), Array:null, Array:null);
    } else { 

      if (FrameIndex == FgStartFrame)  then 
      //println$ "Calling estimate... ImageBuffer size "++ Array:length(ImageBuffer);
        println$ "Calling estimate... frame size "++ Array:length(frame);

      st = clock();
      estimateFg(temppatch, bghist, (frame,cols,rows), diffImage, mask);
      en = clock();
      println$ "Computation time for estimateFg(): "++ en-st;    

      println$ "SUM diffimage "++ Array:fold((+),0,diffImage);

      // Not yet:
      bgStaleCounter += 1;
      if bgStaleCounter == BgStep then {
	println$ "Updating bg";
	updateBg(temppatch, bghist, (frame,cols,rows), 
	         if useBgUpdateMask then mask else null);
	bgStaleCounter := 0;
      };

      FrameIndex += FgStep;
      
      // This is a tad naughty... We destructively update frame by the mask:
      for i = 0 to mask.length - 1 {         
	  if mask[i] == 0 then {
	    i3 = i*3;
	    // Inefficient to do divisions! Hopefully gcc makes it a right shift.
            frame[i3+0] := frame[i3+0] / 2;
            frame[i3+1] := frame[i3+1] / 2;
            frame[i3+2] := frame[i3+2] / 2;
	  }
      };

      emit (FrameIndex - FgStep, (frame,cols,rows), diffImage, mask);
    };
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
index_stream = iterate _ in timer(10) {
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

// Select out the FG image and display that:
fun my_display(strm) 
     display_to_screen(smap(fun((_, (fg,c,r), diff, mask)) (fg,c,r), strm))

fun nilbhatta(strm)
 iterate frame in strm {
  state { cnt = 0 }
  emit (cnt, frame, Array:null, Array:null);
  cnt += 1;
 }

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

// Main stream script:

input_imgs :: Stream Image;
output_imgs :: Stream OutputBundle -> Stream ();

input_imgs  = if LIVE then front_camera else smap(ws_readImage, filenames)
output_imgs = if LIVE then my_display else dump_files;

main = 
       output_imgs
/*      $ unsquisher */
/*      $ unsquisher */
     //$ nilbhatta
     $ bhatta
/*      $ squisher */
/*      $ squisher */
     $ input_imgs;


