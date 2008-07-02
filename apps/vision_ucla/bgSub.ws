
/* 
  Teresa Ko's background subtraction code, ported from C++ to WS by Ryan Newton.
  [2008.06.27]

TODO:

Flip blue and RED in the image pixel ordering.

 */

include "stdlib.ws"


//====================================================================================================
/// Types and Constants:


type Color = Uint8;
type Image = Array Color;
type Array4D t = Array (Array (Array (Array t))); 
type Array3D t = Array (Array (Array t)); 

type Inexact = Float; // Or Double
abs = absF

DEBUG = true;

type Settings = (
        String * // ImLoc[500];
	String * // OutLoc[500];	
	Int * // BgStartFrame;
	Int * // StartFrame;	
	Int * // NumBgFrames;
	Int * // NumFrames;	

	Int * // BgStep;
	Int * // FgStep;	
	Double * // Threshold;		

	Int * // rows;
	Int * // cols;
	Int * // nChannels;
	Bool  // useHSV;
     );	

settings :: Settings = (
		"/data/birdmotion/JR_webcam/FeederStation_2007-06-26_14-00-03.000/", // Filename
		"../processed/FeederStation_2007-06-26_14-00-03.000/bhatta_default/", // OutLoc
		0,//396, // BgStartFrame
		0,	 // StartFrame
		5,//20, // NumBgFrames
		5,//100, // NumFgFrames

		1, //BgStep
		1, //FgStep
		128, // Threshold
										
		240,	// rows
		320,	// cols
		1,	// nChannels // NOT USED YET????
		
		true    // useBgModel		
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
let (Filename, OutLoc, BgStartFrame, StartFrame, 
     NumBgFrames, NumFg, BgStep, FgStep, Threshold,
     rows, cols, nChannels, useBgModel) = settings;

//====================================================================================================
/// General helpers:

fun fill3D(arr, val) {
  for i = 0 to Array:length(arr)-1 {
   for j = 0 to Array:length(arr[i])-1 {
       Array:fill(arr[i][j], val);
     }};
}

// Assumes they're of the same dimension.  If not, you'll get an
// unhelpful out of bounds error.
fun Array:foreach2_3D(arr1, arr2, fn) {
  using Array;
   if DEBUG then assert_eq("foreach2_3D: length mismatch", arr1.length, arr2.length);
  for i = 0 to length(arr1)-1 {
    if DEBUG then assert_eq("foreach2_3D: inner length mismatch", arr1[i].length, arr2[i].length);
   for j = 0 to length(arr1[i])-1 { 
     if DEBUG then assert_eq("foreach2_3D: inner inner length mismatch", arr1[i][j].length, arr2[i][j].length);
    for k = 0 to length(arr1[i][j])-1 {
      fn(arr1[i][j][k], arr2[i][j][k])
  }}};
}

fun Array:map3D_inplace(arr, fn) {
  for i = 0 to Array:length(arr)       - 1 {
  for j = 0 to Array:length(arr[i])    - 1 {
  for k = 0 to Array:length(arr[i][j]) - 1 {
     arr[i][j][k] := fn(arr[i][j][k]);
  }}}
}

fun Array:iter3D(arr, fn) {
  for i = 0 to Array:length(arr)       - 1 {
  for j = 0 to Array:length(arr[i])    - 1 {
  for k = 0 to Array:length(arr[i][j]) - 1 {
    fn(i,j,k)
  }}}  
}

fun Array:make3D(i,j,k, val) {
  using Array;
  build(i, fun(_)
  build(j, fun(_)
   make(k, val)));
}

fun Array:make4D(i,j,k,l, val) {
  using Array;
  build(i, fun(_) 
  build(j, fun(_)
  build(k, fun(_)
   make(l, val))));
}


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
populateBg :: (Array4D Float, Image) -> ();
fun populateBg(bgHist, image) {

  assert_eq("Image must be the right size:",Array:length(image), rows * cols * 3);

  // Patches are centered around the pixel.  [p.x p.y]-[offset offset] gives the upperleft corner of the patch.				

  ERG = NumBins3;
  EHH = NumBins3.gint;

  offset = SizePatch / 2;
  // To reduce divisions.  Used to take a pixel value and calculate the histogram bin it falls in.
  inv_sizeBins1 = 1 / ceilF(256 / NumBins1.gint); 
  inv_sizeBins2 = 1 / ceilF(256 / NumBins2.gint);
  inv_sizeBins3 = 1 / ceilF(256 / NumBins3.gint);
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
		make(NumBins3, 0.0)));	
  for r = 0 to rows-1 { 
    // clear temp patch
    fill3D(tempHist, 0);  
				
    // create the left most pixel's histogram from scratch
    c :: Int = 0;
    roEnd = r - offset + SizePatch;  // end of patch
    coEnd = c - offset + SizePatch;  // end of patch

    for ro = r-offset to roEnd-1 { // cover the row
      roi = if ro < 0 then 0-ro-1 else 
            if ro >= rows then 2 * rows-1-ro else ro;
      for co = c-offset to coEnd-1 { // cover the col
        coi = if co < 0 then 0-co-1 else
              if co >= cols then 2 * cols-1-co else co;
        // get the pixel location
        i = (roi * cols + coi) * 3;  
	
        // figure out which bin
        binB = f2i$ Float! image[i  ] * inv_sizeBins1;
        binG = f2i$ Float! image[i+1] * inv_sizeBins2;
        binR = f2i$ Float! image[i+2] * inv_sizeBins3;
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
	co = c - offset - 1;
	coi = if co < 0 then 0 - co - 1 else 
    	      if co >= cols then 2 * cols - 1 - co else co;
	for ro = r - offset to offset + SizePatch - 1 {
	  roi = if ro < 0 then 0-ro-1 else 
 	        if ro >= rows then 2 * rows - 1 - ro else ro;
	  i = (roi * cols + coi) * 3;	  

	  binB = floatToInt(Float! image[i+0] * inv_sizeBins1);
	  binG = floatToInt(Float! image[i+1] * inv_sizeBins2);
	  binR = floatToInt(Float! image[i+2] * inv_sizeBins3);	  
	  
	  //varbar = (binB, binG, binR, 0.0 - sampleWeight);
	  tempHist[binB][binG][binR] := tempHist[binB][binG][binR] + 0.0 - sampleWeight;
          //tempHist[binB][binG][binR] += 0.0 - sampleWeight;
	  if (tempHist[binB][binG][binR] < 0.0) then {
	    tempHist[binB][binG][binR] := 0.0;
	    //wserror $ "error: underflow";
	  };
	};
			
	// add right col
	co = c - offset + SizePatch - 1;
	coi = if co < 0 then 0-co-1 else
              if co >= cols then 2 * cols-1-co else co;
	for ro = r-offset to r-offset + SizePatch - 1 {
	  roi = if ro < 0 then 0-ro-1 else
                if ro >= rows then 2 * rows-1-ro else ro;

	  i = (roi * cols + coi) * 3;

	  binB = f2i (Float! image[i  ] * inv_sizeBins1);
	  binG = f2i (Float! image[i+1] * inv_sizeBins2);
	  binR = f2i (Float! image[i+2] * inv_sizeBins3);
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

estimateFg :: (Array3D Inexact, Array4D Inexact, Image, Image, Image) -> a;
fun estimateFg(pixelHist, bgHist, image, diffImage, mask) {

  println$"IMAGE LENGTH: "++ Array:length(image);

  (image :: Image); // [2008.07.01] Having a typechecking difficulty right now.

   // Patches are centered around the pixel.  [p.x p.y]-[offset offset] gives the upperleft corner of the patch.				
   offset :: Int = SizePatch / 2;
   // To reduce divisions.  Used to take a pixel value and calculate the histogram bin it falls in.
   inv_sizeBins1 :: Inexact = 1 / ceilF(256 / NumBins1.gint);
   inv_sizeBins2 :: Inexact = 1 / ceilF(256 / NumBins2.gint);
   inv_sizeBins3 :: Inexact = 1 / ceilF(256 / NumBins3.gint); // NOTE, if I replace gint with Inexact! I get a typechecking problem.

   // To reduce divisions.  Adjust weight so that a pixel's histogram will be normalized.
   sampleWeight = 1.0 / (SizePatch * SizePatch).gint;	
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
       roEnd = r - offset + SizePatch;
       coEnd = c - offset + SizePatch;
       for ro = r - offset to roEnd-1 {
	 roi = if ro < 0 then 0-ro-1 else
               if ro >= rows then 2*rows-1-ro else ro;
	 for co = c-offset to coEnd -1 {
	   coi = if co < 0 then 0-co-1 else
                 if co >= cols then 2*cols-1-co else co;
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
	                 fun(pix,bg) diff += sqrtF(pix * bg));
       
       // renormalize diff so that 255 = very diff, 0 = same
       // create result images
       diffImage[pIndex] := Uint8! (255 - (diff * 255));
       mask[pIndex] := if Double! diffImage[pIndex] > Threshold then 255 else 0;
                       
       // iterate through the rest of the row
       for c = 1 to cols-1 {
         pIndex = r * cols + c;
			
	 //remove left col
	 co = c-offset-1;
	 coi = if co < 0 then 0-co-1 else 
               if co >= cols then 2*cols-1-co else co;
	 for ro = r-offset to r - offset + SizePatch-1 {
	     roi = if ro < 0 then 0-ro-1 else 
                   if ro >= rows then 2*rows-1-ro else ro;
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
	 co = c-offset + SizePatch-1;
	 coi = if co < 0 then 0-co-1 else 
               if co >= cols then 2*cols-1-co else co;
	 for ro = r-offset to r-offset+SizePatch-1 {
	     roi = if ro < 0 then 0-ro-1 else
                   if ro >= rows then 2*rows-1-ro  else ro;
	     i = (roi * cols + coi) * 3;
	     binB = Int! (Inexact! image[i+0] * inv_sizeBins1);
	     binG = Int! (Inexact! image[i+1] * inv_sizeBins2);
	     binR = Int! (Inexact! image[i+2] * inv_sizeBins3);
	     pixelHist[binB][binG][binR] += sampleWeight;
	 };

	 // compare histograms
	 diff = ref$ 0;
	 Array:foreach2_3D(pixelHist, bgHist[pIndex], 
                           fun(px,bg) diff += sqrtF(px * bg));	 

	 // create result images		
	 diffImage[pIndex] := Uint8! (255 - (diff * 255));
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
updateBg :: (Array4D Inexact, Image, Image) -> ();
fun updateBg(bgHist, image, mask) 
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
    offset = SizePatch / 2; 
    inv_sizeBins1 = 1 / ceilF(256 / Inexact! NumBins1);
    inv_sizeBins2 = 1 / ceilF(256 / Inexact! NumBins2);
    inv_sizeBins3 = 1 / ceilF(256 / Inexact! NumBins3);
    
    tempHist :: Array3D Inexact = make3D(NumBins1, NumBins2, NumBins3, 0);		

    for r = 0 to rows-1 { 
	// clear temp patch
	fill3D(tempHist, 0);  
					
	// first create a patch
	c :: Int = 0;
	roEnd = r - offset + SizePatch;
	coEnd = c - offset + SizePatch;
	for ro = r-offset to roEnd-1 {
	  roi = if ro < 0 then 0-ro-1 else
                if ro >= rows then 2*rows-1-ro else ro;
	  for co = c-offset to coEnd-1 {
	    coi = if co < 0 then 0-co-1 else 
                  if co >= cols then 2*cols-1-co else co; //(co+settings->cols)%settings->cols;
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
	  co = c-offset-1;
	  coi = if co < 0 then 0-co-1 else 
                if co >= cols then 2 * cols-1-co else co;
	  for ro = r-offset to r - offset + SizePatch - 1 {
	    roi = if ro < 0 then 0-ro-1 else
		  if ro >= rows then 2*rows-1-ro else ro;
	    i = (roi * cols + coi) * 3;
	    binB = Int! (Inexact! image[i+0] * inv_sizeBins1);
	    binG = Int! (Inexact! image[i+1] * inv_sizeBins2);
	    binR = Int! (Inexact! image[i+2] * inv_sizeBins3);
	    if (tempHist[binB][binG][binR] < 0) then wserror$ "underflow";
				
	    tempHist[binB][binG][binR] += 0-incAmount;
	  };

	  // add right col
	  co = c-offset + SizePatch-1;
	  coi = if co < 0 then 0-co-1 else 
                if co >= cols then 2*cols-1-co else co;
	  for ro = r-offset to r-offset + SizePatch-1 {
	    roi = if ro < 0 then 0-ro-1 else
	          if ro >= rows then 2*rows-1-ro else ro;
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

// Main stream script:

fakeFrames = iterate _ in timer$3 {
  using Array;

  //arr :: Array Uint8 = build(rows, build(cols, 0));
  //arr = build(rows, make(cols, 0));
  arr = make(rows * cols * 3, 0);
  emit arr;
}


main = {
  println("Bhattacharyya Differencing.");
    
  //SHELL("mkdir ");

  stopFrame = BgStartFrame + NumBgFrames;

  using Array;
  //bghist = Mutable:ref$ null; // Same error.

  iterate frame in fakeFrames {
    state { FrameIndex = 0;
            bgCount = 0;
            //p = Bhatta(settings);
            bghist    = null; 
	    pixelhist = null;
	    mask      = null;
	    diffImage = null;
	    ImageBuffer = null;
          }
    println("Frame # "++ FrameIndex);
    if (FrameIndex == 0) then { 
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
      ImageBuffer := make(rows * cols * nChannels, 0);

      println("Building background model...");
      
      // initializes storage for current image's histograms
      
    };
    if (FrameIndex < stopFrame) then {
	  // Get input frame
	  //InputStream->GetFrame(FrameIndex,ImageBuffer);
	  // add frame to the background
  	  //print(".");
          st = clock();
	  populateBg(bghist, frame);
	  en = clock();
	  println$ "Computation time for populateBg(): " ++ (en - st);

	  FrameIndex += 1;

	  emit bghist[0][0][0];
    } else {
      if (FrameIndex == stopFrame)
      then println("\nFinished background model, extracting foreground.\n");      
      print(".");

      println$ "Calling estimate... ImageBuffer size "++ Array:length(ImageBuffer);

      st = clock();
      estimateFg(pixelhist, bghist, ImageBuffer, diffImage, mask);
      en = clock();
      println$ "Computation time for estimateFg(): "++ en-st;    
      bgCount+=1;

      if bgCount == BgStep then {
	println$ "Updating bg";
	if useBgUpdateMask 
	then updateBg(bghist, ImageBuffer, mask)
	else updateBg(bghist, ImageBuffer, null);
	bgCount := 0;
      };

      FrameIndex += FgStep;

      emit bghist[0][0][0];
    };
  }
}

