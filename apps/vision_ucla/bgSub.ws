
/* 
  Teresa Ko's background subtraction code, ported from C++ to WS by Ryan Newton.
  [2008.06.27]

TODO:

Flip blue and RED in the image pixel ordering.

 */

include "stdlib.ws"

type Settings = (
        String * // ImLoc[500];
	String * // OutLoc[500];	
	Int * // BgStartFrame;
	Int * // StartFrame;	
	Int * // NumBgFrames;
	Int * // NumFrames;	

	Int * // BgStep;
	Int * // Step;	
	Double * // Threshold;		

	Int * // rows;
	Int * // cols;
	Int * // nChannels;
	Bool  // useHSV;
     );	

settings :: Settings = (
		"/data/birdmotion/JR_webcam/FeederStation_2007-06-26_14-00-03.000/", // Filename
		"../processed/FeederStation_2007-06-26_14-00-03.000/bhatta_default/", // OutLoc
		396, // BgStartFrame
		0,	 // StartFrame
		5,//20, // NumBgFrames
		5,//100, // NumFgFrames

		1, //BgStep
		1, //FgStep
		128, // Threshold
										
		240,	// rows
		320,	// cols
		0,	// nChannels // NOT USED YET????
		
		true    // useBgModel		
	      );
	
bhattasettings = (
	        (16 :: Int),   // NumBins1
		(2  :: Int),   // NumBins2
		(2  :: Int),   // NumBins3
		(30 :: Int),   // SizePatch
		(0  :: Double) // Alpha (bg update)
	);


// TEMP:
// Unpack the settings, ugly because we don't have records yet --rrn	
let (NumBins1, NumBins2, NumBins3, SizePatch, Alpha) = bhattasettings;
let (Filename, OutLoc, BgStartFrame, StartFrame, 
     NumBgFrames, NumFg, BgStep, FgStep, Threshold,
     rows, cols, nChannels, useBgModel) = settings;

type Color = Uint8;
type Image = Array Color;
type Array4D t = Array (Array (Array (Array t))); 
type Array3D t = Array (Array (Array t)); 

type Inexact = Float; // Or Double

fun fill3D(arr, val) {
  for i = 0 to Array:length(arr)-1 {
   for j = 0 to Array:length(arr[i])-1 {
       Array:fill(arr[i][j], 0);
     }};
}

DEBUG = true;

//================================================================================

// Builds background model histograms for each pixel.  
// Additions to the histograms are scaled according the assumption that 
// it will receive settings->NumBgFrames # of images.
populateBg :: (Array4D Float, Image) -> ();
fun populateBg(bgHist, image) {

  assert_eq("Image must be the right size:",Array:length(image), rows * cols * 3);

  // Patches are centered around the pixel.  [p.x p.y]-[offset offset] gives the upperleft corner of the patch.				

  offset = SizePatch / 2;
  // To reduce divisions.  Used to take a pixel value and calculate the histogram bin it falls in.
  inv_sizeBins1 = 1 / ceilF(256 / NumBins1.gint); 
  inv_sizeBins2 = 1 / ceilF(256 / NumBins2.gint);
  inv_sizeBins3 = 1 / ceilF(256 / NumBins3.gint);
  // To reduce divisions.  Adjust weight so that a pixel's histogram will be normalized after all frames are received.
  sampleWeight = 1 / gint(SizePatch * SizePatch * NumBgFrames);
  
  //nPixels =  rows * cols; 	
  //	int i = 0, k=0;  // k = current pixel index
  //int binB, binG, binR;
  //int r,c,ro,co,roi,coi;
	
	
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
    c = 0;
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



      fun bounds(x,range) {
	 if x < 0 then 0-x-1 else
         if x >= range then 2*range-1-x else x;
       };

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


// Estimate foregound pixel from "image" and the background model. 
// "diffImage" visualizes the difference as measured by the bhattacharyya distance between the current image's
// pixel and the background model pixel.
// "mask" is an image where white pixels represent the foreground and black pixels represents the background
// according to settings->Threshold

estimateFg :: (Array3D Inexact, Array4D Inexact, Image, Image, Image) -> a;
fun estimateFg(pixelHist, bgHist, image, diffImage, mask) {

   // Patches are centered around the pixel.  [p.x p.y]-[offset offset] gives the upperleft corner of the patch.				
   offset :: Int = SizePatch / 2;
   // To reduce divisions.  Used to take a pixel value and calculate the histogram bin it falls in.
   inv_sizeBins1 = 1.0 / ceilF(256 / Inexact! NumBins1);
   inv_sizeBins2 = 1.0 / ceilF(256 / Inexact! NumBins2);
      inv_sizeBins3 = 1.0 / ceilF(256 / Inexact! NumBins3);

   // To reduce divisions.  Adjust weight so that a pixel's histogram will be normalized.
   sampleWeight = 1.0 / (SizePatch * SizePatch).gint;	
   nPixels =  rows * cols; 
		
   // clear mask image
   using Array; using Mutable;
   fill(mask, 0);

   // as in the populateBg(), we compute the histogram by subtracting/adding cols	

   for r = 0 to rows-1 {
       c=0;
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
                           fun(px,bg) sqrtF(px * bg));	 

	 // create result images		
	 diffImage[pIndex] := Uint8! (255 - (diff * 255));
	 mask[pIndex] := if Double! diffImage[pIndex] > Threshold then 255 else 0;	 
       }
   }
}


//====================================================================================================


//void Bhatta::estimateFg(unsigned char* image, unsigned char* diffImage,unsigned char* mask ) {

//void Bhatta::updateBg(unsigned char* image,unsigned char* diffImage,unsigned char* mask )


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
    state { cnt = 0;
            //p = Bhatta(settings);
            bghist = null; // ACK - compiler bug
          }
    if (cnt == 0) then { 
      println$ "Output location: "++OutLoc;
      println$ "Settings: ";
      println$ " # of bins:             "++ NumBins1 ++","++ NumBins2 ++","++ NumBins3;
      println$ " Size of patch:         "++ SizePatch;
      println$ " # of Bg Frames:        "++ NumBgFrames;
      println$ " Threshold:             "++ Threshold;
      println$ " Alpha:                 "++ Alpha;

      println$ "Image rows/cols: "++ rows ++", "++ cols;

      println("Building background model...");
      bghist := build(rows*cols, fun(_) 
		build(NumBins1,  fun(_)
                build(NumBins2,  fun(_)
		 make(NumBins3, 0)
	        )));
    };
    if (cnt < stopFrame) then {
	  // Get input frame
	  //InputStream->GetFrame(FrameIndex,ImageBuffer);
	  // add frame to the background
  	  //print(".");
          st = clock();
	  populateBg(bghist, frame);
	  en = clock();
	  println$ "Computation time for populateBg(): " ++ (en - st);

	  emit bghist[0][0][0];
    } else {
      if (cnt == stopFrame)
      then println("\nFinished background model, extracting foreground.\n");      
      print(".");

      //st = clock();
      //estimateFg(pixelHist, bghist, ImageBuffer, diffImage, mask);
      //en = clock();
      //println$ "Computation time for estimateFg(): "++ en-st;
      //bgCount++;
      
      emit bghist[0][0][0];
    };
    cnt += 1;
  }
}

