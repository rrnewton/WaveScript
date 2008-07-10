
/* 
  Teresa Ko's background subtraction code, ported from C++ to WS by Ryan Newton.
  [2008.06.27]

TODO:
Flip blue and RED in the image pixel ordering.

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
type Array4D t = Array (Array (Array (Array t))); 
type Array3D t = Array (Array (Array t)); 

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
		(128 :: Double), // Threshold
										
		(3 :: Int),   // nChannels - RGB image
		
		//false    // useHSV
	      );
	
bhattasettings = (
		(16 :: Int),   // NumBins1
		(2  :: Int),   // NumBins2
		(2  :: Int),   // NumBins3
		(30 :: Int),   // SizePatch
		(0  :: Inexact), // Alpha (bg update)
		false // useBgUpdateMask 
	);


// TEMP:
// Unpack the settings, ugly because we don't have records yet --rrn	
let (NumBins1, NumBins2, NumBins3, SizePatch, Alpha, useBgUpdateMask) = bhattasettings;
let (Filename, OutLoc, BgStartFrame, FgStartFrame, 
     NumBgFrames, NumFgFrames, BgStep, FgStep, Threshold,
     //rows, cols, 
     nChannels) = settings;

 // To reduce divisions.  Used to take a pixel value and calculate the histogram bin it falls in.
 inv_sizeBins1 :: Inexact = 1 / ceil(256 / NumBins1.gint);
 inv_sizeBins2 :: Inexact = 1 / ceil(256 / NumBins2.gint);
 inv_sizeBins3 :: Inexact = 1 / ceil(256 / NumBins3.gint); // NOTE, if I replace gint with Inexact! I get a typechecking problem.
 // Patches are centered around the pixel.  [p.x p.y]-[halfPatch halfPatch] gives the upperleft corner of the patch.				
 halfPatch :: Int = SizePatch / 2;

//====================================================================================================
// Factoring pieces of the below functions into these helpers:

fun bounds(x,range) {
  if x < 0 then 0-x-1 else
  if x >= range then 2*range-1-x else x;
};

//====================================================================================================

// Builds background model histograms for each pixel.  
// Additions to the histograms are scaled according the assumption that 
// it will receive settings->NumBgFrames # of images.
populateBg :: (Array4D Inexact, Image) -> ();
fun populateBg(bgHist, (image,cols,rows)) {

  assert_eq("Image must be the right size:",Array:length(image), rows * cols * 3);

  // Patches are centered around the pixel.  [p.x p.y]-[offset offset] gives the upperleft corner of the patch.				

  ERG = NumBins3;
  EHH = NumBins3.gint;

  // To reduce divisions.  Adjust weight so that a pixel's histogram will be normalized after all frames are received.
  sampleWeight = 1 / gint(SizePatch * SizePatch * NumBgFrames);
  	
  // Histograms are for each pixel by creating create a histogram of the left most pixel in a row.
  // Then the next pixel's histogram in the row is calculated by:
  //   1. removing pixels in the left most col of the previous patch from the histogram and 
  //   2. adding pixels in the right most col of the current pixel's patch to the histogram
	
  // create temp hist to store working histogram
  using Array; using Mutable;

  k = ref$ 0; // current pixel index

  tempHist = build(NumBins1, fun(r)
              build(NumBins2, fun(b)
	       make(NumBins3, (0 :: Inexact))));

  for r = 0 to rows-1 { 
    // clear temp patch
    fill3D(tempHist, 0);  
				
    // create the left most pixel's histogram from scratch
    c :: Int = 0;
    roEnd = r - halfPatch + SizePatch;  // end of patch
    coEnd = c - halfPatch + SizePatch;  // end of patch

    for ro = r-halfPatch to roEnd-1 { // cover the row
      roi = bounds(ro,rows);
      for co = c-halfPatch to coEnd-1 { // cover the col
	coi = bounds(co,cols);
        // get the pixel location
        i = (roi * cols + coi) * 3;  
	
        // figure out which bin
        binB = Int! (Inexact! image[i  ] * inv_sizeBins1);
        binG = Int! (Inexact! image[i+1] * inv_sizeBins2);
	binR = Int! (Inexact! image[i+2] * inv_sizeBins3);
        // add to temporary histogram	
        tempHist[binB][binG][binR] += sampleWeight;
      }
    };

    // copy temp histogram to left most patch
    for cb = 0 to NumBins1-1 {
     for cg = 0 to NumBins2-1 {
       for cr = 0 to NumBins3-1 {
         bgHist[k][cb][cg][cr] += tempHist[cb][cg][cr];
    }}};
		
    // increment pixel index
    k += 1;

    // compute the top row of histograms
    for c = 1 to cols-1 {
	// subtract left col
	co = c - halfPatch - 1;
	coi = bounds(co,cols);
	for ro = r - halfPatch to r-halfPatch + SizePatch - 1 {
	  roi = bounds(ro, rows);
	  i = (roi * cols + coi) * 3;	  

	  binB = Int! (Inexact! image[i+0] * inv_sizeBins1);
	  binG = Int! (Inexact! image[i+1] * inv_sizeBins2);
	  binR = Int! (Inexact! image[i+2] * inv_sizeBins3);	  
	  
	  //varbar = (binB, binG, binR, 0.0 - sampleWeight);
	  tempHist[binB][binG][binR] := tempHist[binB][binG][binR] + 0 - sampleWeight;
          //tempHist[binB][binG][binR] += 0.0 - sampleWeight;
	  if (tempHist[binB][binG][binR] < 0) then {
	    tempHist[binB][binG][binR] := 0;	    
	    //wserror $ "error: underflow";
	  };	  
	};
			
	// add right col
	co = c - halfPatch + SizePatch - 1;
	coi = bounds(co,cols);
	for ro = r-halfPatch to r-halfPatch + SizePatch - 1 {
	  roi = bounds(ro,rows);
	  i = (roi * cols + coi) * 3;

	  binB = Int! (Inexact! image[i  ] * inv_sizeBins1);
	  binG = Int! (Inexact! image[i+1] * inv_sizeBins2);
	  binR = Int! (Inexact! image[i+2] * inv_sizeBins3);
	  tempHist[binB][binG][binR] += sampleWeight;
	};

	// copy over			
	for cb = 0 to NumBins1-1 {
	for cg = 0 to NumBins2-1 {
        for cr = 0 to NumBins3-1 {	  
	  bgHist[k][cb][cg][cr] += tempHist[cb][cg][cr];
	}}};

	// increment pixel index
	k += 1;
    };
  } // End mega for-loop
}

//====================================================================================================



// Estimate foregound pixel from "image" and the background model. 
// "diffImage" visualizes the difference as measured by the bhattacharyya distance between the current image's
// pixel and the background model pixel.
// "mask" is an image where white pixels represent the foreground and black pixels represents the background
// according to settings->Threshold

estimateFg :: (Array3D Inexact, Array4D Inexact, Image, RawImage, RawImage) -> ();
fun estimateFg(pixelHist, bgHist, (image,cols,rows), diffImage, mask) {

   (image :: RawImage); // [2008.07.01] Having a typechecking difficulty right now.

   // To reduce divisions.  Adjust weight so that a pixel's histogram will be normalized.
   sampleWeight = (Inexact! 1.0) / (SizePatch * SizePatch).gint;	
   nPixels =  rows * cols; 
		
   // clear mask image
   using Array; using Mutable;
   fill(mask, 0);

   // as in the populateBg(), we compute the histogram by subtracting/adding cols	
   for r = 0 to rows-1 {
       c :: Int = 0;
       pIndex = r * cols + c;
		
       // for the first pixel
       // clear patch histogram
       fill3D(pixelHist, 0);

       // compute patch
       roEnd = r - halfPatch + SizePatch;
       coEnd = c - halfPatch + SizePatch;
       for ro = r - halfPatch to roEnd-1 {
	 roi = bounds(ro,rows);
	 for co = c-halfPatch to coEnd -1 {
	   coi = bounds(co,cols);
	   i = (roi * cols + coi) * 3;
	   binB = Int! (Inexact! image[i+0] * inv_sizeBins1);
	   binG = Int! (Inexact! image[i+1] * inv_sizeBins2);
	   binR = Int! (Inexact! image[i+2] * inv_sizeBins3);
	   pixelHist[binB][binG][binR] += sampleWeight;	   				
	 }
       };

       // compare histograms
       diff :: Ref Inexact = ref$ 0;
       Array:foreach2_3D(pixelHist, bgHist[pIndex],
	                 fun(pix,bg) diff += sqrt(pix * bg));
       
       // renormalize diff so that 255 = very diff, 0 = same
       // create result images
       diffImage[pIndex] := Uint8! (255 - Int! (diff * 255));

       // Inefficient:
       mask[pIndex] := if Double! diffImage[pIndex] > Double! Threshold then 255 else 0;
                       
       // iterate through the rest of the row
       for c = 1 to cols-1 {
         pIndex = r * cols + c;
			
	 //remove left col
	 co = c-halfPatch-1;
	 coi = bounds(co,cols);
	 for ro = r-halfPatch to r - halfPatch + SizePatch-1 {
	     roi = bounds(ro,rows);
	     i = (roi * cols + coi) * 3;
	     binB = Int! (Inexact! image[i+0] * inv_sizeBins1);
	     binG = Int! (Inexact! image[i+1] * inv_sizeBins2);
	     binR = Int! (Inexact! image[i+2] * inv_sizeBins3);
				
	     pixelHist[binB][binG][binR] += 0-sampleWeight;
	     if (pixelHist[binB][binG][binR] < 0) then {
		 pixelHist[binB][binG][binR] := 0;
		 //cout << "error: underflow "  << sampleWeight << " " <<  pixelHist[binB][binG][binR] << endl;
	     }
	 };		
			
	 // add right col
	 co = c-halfPatch + SizePatch-1;
	 coi = bounds(co,cols);
	 for ro = r-halfPatch to r-halfPatch+SizePatch-1 {
	     roi = bounds(ro,rows);
	     i = (roi * cols + coi) * 3;
	     binB = Int! (Inexact! image[i+0] * inv_sizeBins1);
	     binG = Int! (Inexact! image[i+1] * inv_sizeBins2);
	     binR = Int! (Inexact! image[i+2] * inv_sizeBins3);
	     pixelHist[binB][binG][binR] += sampleWeight;
	 };

	 // compare histograms
	 diff = ref$ 0;
	 Array:foreach2_3D(pixelHist, bgHist[pIndex], 
                           fun(px,bg) diff += sqrt(px * bg));	 

	 // create result images		
	 diffImage[pIndex] := Uint8! (255 - Int! (diff * 255));
	 mask[pIndex] := if Double! diffImage[pIndex] > Threshold then 255 else 0;	 
       }
   }
}


//====================================================================================================



// update background
// Two types happen:
// If a mask is not given, all pixels are updated.
// If a mask is given, only pixels in the diffImage that have a lower value than 
//    settings->Threshold (are background) are updated.
// 
// degrade the background model by scaling each bin by 1-bSettings->Alpha
// add new pixel values scaled so that the resulting background model will sum to 1.
updateBg :: (Array4D Inexact, Image, RawImage) -> ();
fun updateBg(bgHist, (image,cols,rows), mask) 
  if Alpha == 0 then () else {
    using Array; using Mutable;
    if mask == null then println$ "Mask not given: updating everything";

    k = ref$ 0;
	
    // iterate through all pixel's histograms
    // rescale the histogram only if there is a mask given
    for i = 0 to rows * cols - 1 {

      // if no mask provided, update all pixels
      // if mask is provided and the pixel in the mask indicates background, update
      if mask == null || mask[i] == 0 then {
	sum :: Ref Inexact = ref$ 0;

	Array:map3D_inplace(bgHist[i],
          fun(bh) {
	    //sum += bh;
	    bh //bh * (1 - Alpha);
          });

	println$ "What does this do?? "++(1 / 100000);
	
	// make sure histogram still sums up correctly.
	if sum - (1-Alpha) > (1 / 100000) // 10e-5
	then wserror$ "ERROR1: bgHist is not normalized properly: sum = " ++ sum;
      }
    };

    incAmount :: Inexact = 1 / (SizePatch * SizePatch).gint;
    nPixels =  rows * cols; 
    
    tempHist :: Array3D Inexact = make3D(NumBins1, NumBins2, NumBins3, 0);		

    for r = 0 to rows-1 { 
	// clear temp patch
	fill3D(tempHist, 0);  
					
	// first create a patch
	c :: Int = 0;
	roEnd = r - halfPatch + SizePatch;
	coEnd = c - halfPatch + SizePatch;
	for ro = r-halfPatch to roEnd-1 {
	  roi = bounds(ro,rows);
	  for co = c-halfPatch to coEnd-1 {
            coi = bounds(co,cols);
	    i = (roi * cols + coi) * 3;
	    binB = Int! (Inexact! image[i+0] * inv_sizeBins1);
	    binG = Int! (Inexact! image[i+1] * inv_sizeBins2);
	    binR = Int! (Inexact! image[i+2] * inv_sizeBins3);
	    tempHist[binB][binG][binR] += incAmount;
	  }
	};
		
	// update bg hist if indicated to do so
	if mask == null || mask[k] == 0 then {
	  sum = ref$ 0;
          Array:iter3D( bgHist, fun(cb,cg,cr) {  // or map2_inplace3D
	    bgHist[k][cb][cg][cr] += Alpha * tempHist[cb][cg][cr];
            sum += bgHist[k][cb][cg][cr];
	  });

	  if abs(sum - 1) > (1 / 100000)
	  then wserror$ "ERROR2: bgHist not normalized properly: sum  = "++ sum;
        };
		
	// increment pixel value
	k += 1;

	// iterate through the rest of the row
	// we compute each histogram regardless of the mask 
	// under the assumption that the foreground is smaller than the background
	// it seemed easier to do this than a hybrid histogram computation that skipped
	// pixels.
	for c = 1 to cols-1 {

	  // subtract left col
	  co = c-halfPatch-1;
	  coi = bounds(co,cols);
	  for ro = r-halfPatch to r - halfPatch + SizePatch - 1 {
	    roi = bounds(ro,rows);
	    i = (roi * cols + coi) * 3;
	    binB = Int! (Inexact! image[i+0] * inv_sizeBins1);
	    binG = Int! (Inexact! image[i+1] * inv_sizeBins2);
	    binR = Int! (Inexact! image[i+2] * inv_sizeBins3);
	    if (tempHist[binB][binG][binR] < 0) then wserror$ "underflow";
				
	    tempHist[binB][binG][binR] += 0-incAmount;
	  };

	  // add right col
	  co = c-halfPatch + SizePatch-1;
	  coi = bounds(co,cols);
	  for ro = r-halfPatch to r-halfPatch + SizePatch-1 {
  	    roi = bounds(ro,rows);
	    i = (roi * cols + coi) * 3;
	    binB = Int! (Inexact! image[i+0] * inv_sizeBins1);
	    binG = Int! (Inexact! image[i+1] * inv_sizeBins2);
	    binR = Int! (Inexact! image[i+2] * inv_sizeBins3);
	    tempHist[binB][binG][binR] += incAmount;
	  };
						
	  // only update background if indicated to do so
	  if mask == null || mask[k] == 0 then {
	    sum = ref$ 0;

            Array:iter3D( bgHist, fun(cb,cg,cr) {  // or map2_inplace3D
	      bgHist[k][cb][cg][cr] += Alpha * tempHist[cb][cg][cr];
              sum += bgHist[k][cb][cg][cr];
	    });
	    	    
	    if abs(sum-1) > (1 / 100000) 
	    then wserror$ "ERROR: bgHist not normalized properly: sum  = "++sum;
	  };
			
	  //update pixel position
	  k+=1;
	}
    }
  }


//====================================================================================================

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

// Main Bhatta function:

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
	    pixelhist = null;
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
      bghist := make4D(rows*cols, NumBins1, NumBins2, NumBins3, 0);
      pixelhist := make3D(NumBins1, NumBins2, NumBins3, 0);
      mask := make(rows * cols, 0);
      diffImage   := make(rows * cols * nChannels, 0);
      //ImageBuffer := make(rows * cols * nChannels, 0);

      println("Building background model...");      
    };

    if bgEstimateMode then {
	  // Get input frame
	  //InputStream->GetFrame(FrameIndex,ImageBuffer);
	  // add frame to the background
          st = clock();
	  populateBg(bghist, (frame,cols,rows));
	  en = clock();
	  println$ "Computation time for populateBg(): " ++ (en - st);

	  FrameIndex += 1;

          if FrameIndex == stopFrame then {
            bgEstimateMode := false;
            FrameIndex := FgStartFrame;
	    println("\nFinished background model, extracting foreground.\n");
            stopFrame := FgStartFrame + NumFgFrames * FgStep;
	  };

	  //emit (0, (frame,cols,rows), Array:null, Array:null);
    } else { 

      if (FrameIndex == FgStartFrame)
      then 

      //println$ "Calling estimate... ImageBuffer size "++ Array:length(ImageBuffer);
      println$ "Calling estimate... frame size "++ Array:length(frame);

      st = clock();
      estimateFg(pixelhist, bghist, (frame,cols,rows), diffImage, mask);
      en = clock();
      println$ "Computation time for estimateFg(): "++ en-st;    

      // Not yet:
      bgStaleCounter += 1;
      if bgStaleCounter == BgStep then {
	println$ "Updating bg";
	if useBgUpdateMask	then updateBg(bghist, (frame,cols,rows), mask)
	else updateBg(bghist, (frame,cols,rows), null);
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
