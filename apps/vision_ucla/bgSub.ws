
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
		20, // NumBg
		100, // NumFg

		1, //BgStep
		1, //FgStep
		128, // Threshold
										
		240,	// rows
		320,	// cols
		0,	// nChannels // NOT USED YET????
		
		true    // useBgModel		
	      );


/*
void InputReader::Initialize(Settings *controls) 
{
 	pControls = controls;
	imLoc = pControls->ImLoc;
	outLoc = pControls->OutLoc;
	int n;
  n = scandir(imLoc, &ep, &select, alphasort);
  if (n < 0)
      cout << "Error: scandir" << endl;
	else
		nFiles = n; 
	cout << "\t" << pControls->ImLoc << endl << "\t# of Files: " << nFiles << endl;
	char filename[500];
	strcpy(filename,imLoc);
	strcat(filename, ep[0]->d_name);
	im = cvvLoadImage(filename);	
	
	pControls->rows = im->height;
	pControls->cols = im->width;
	pControls->nChannels = im->nChannels;
	
	cvReleaseImage(&im);
}
*/
	
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


/*

typedef struct {
	int NumBins1;
	int NumBins2;
	int NumBins3;
	int MaxSizePatch;
	double Regularizer;
	double Alpha;				// background update amount 
	bool useBgUpdateMask; 
} BhattaOptSettings;


typedef struct {
	int NumBins1;
	int NumBins2;
	int NumBins3;
	int SizePatch;
	double Alpha;				// background update amount 
	bool useBgUpdateMask; 
	bool normalized;
} EMDSettings;

typedef struct {
	int SizeBinsX;
	int SizeBinsY;
	int NumBins1;
	int NumBins2;
	int NumBins3;
	int NumFgFrames;
	double Alpha; // uniform fg percentage
	bool useFgUpdate;
	bool useMRF;
	
	int NumBinsX;
	int NumBinsY;
} SheikhSettings;


typedef struct {
	int NumEigenValues;
} OliverSettings;
	

typedef struct {
	double Alpha; // amount of shadow
} ElgammalSettings;
*/




type Color = Uint8;
type Image = Array Color;
type Array4D t = Array (Array (Array (Array t))); 

// Builds background model histograms for each pixel.  
// Additions to the histograms are scaled according the assumption that 
// it will receive settings->NumBgFrames # of images.

populateBg :: (Array4D Float, Image) -> ();
fun populateBg(bgHist, image) {

  println$ "POPULATING, rows "++rows++"\n";

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

  println$ "K INITIALIZED TO: "++k;

  tempHist = build(NumBins1, fun(r)
              build(NumBins2, fun(b)
		make(NumBins3, 0.0)));	
  for r = 0 to rows-1 { 

    println$" r "++r++" of rows "++rows;

    // clear temp patch
    for cb = 0 to NumBins1-1 {
     for cg = 0 to NumBins2-1 {
       Array:fill(tempHist[cb][cg], 0);
     }};
				
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

    println$" finished covering row ";
		
    // copy temp histogram to left most patch
    for cb = 0 to NumBins1-1 {
     for cg = 0 to NumBins2-1 {
       for cr = 0 to NumBins3-1 {
         bgHist[k][cb][cg][cr] += tempHist[cb][cg][cr];
    }}};
		
    println$" copied to leftmost patch OUTER INCR k, before: "++k;

    // increment pixel index
    k += 1;

    // compute the top row of histograms
    for c = 1 to cols-1 {

        println$ "starting column loop: r "++r++" c "++c;
		
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

	println$ "  Finished inner row loop.." ;
			
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

	println$ "  Finished inner column loop.. k "++k ;
			
	// copy over			
	for cb = 0 to NumBins1-1 {
	for cg = 0 to NumBins2-1 {
        for cr = 0 to NumBins3-1 {	  
	  //if k > 76560 then println$ "k "++ k ++" cb "++ cb ++" cg "++ cg ++" cr "++ cr;
	  bgHist[k][cb][cg][cr] += tempHist[cb][cg][cr];
	}}};

	// increment pixel index
	k += 1;

        println$ "  Finished inner copy loop.." ;
    };

    println$" computed top row";

  } // End mega for-loop
}

//void Bhatta::estimateFg(unsigned char* image, unsigned char* diffImage,unsigned char* mask ) {

//void Bhatta::updateBg(unsigned char* image,unsigned char* diffImage,unsigned char* mask )


fakeFrames = iterate _ in timer$3 {
  using Array;

  //arr :: Array Uint8 = build(rows, build(cols, 0));
  //arr = build(rows, make(cols, 0));
  arr = make(rows * cols, 0);
  emit arr;
}


main = {
  println("Bhattacharyya Differencing.");
    
  //SHELL("mkdir ");

  stopFrame = 10;

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
          st = clock();
	  populateBg(bghist, frame);
	  en = clock();
	  println$ "Computation time for populateBg(): " ++ (en - st);
    } else {
      if (cnt == stopFrame)
      then println("Finished background model, extracting foreground.\n");
      
    };
    cnt += 1;
  }
}

/*


int main(int argc,char *argv[])
{
		
	// parse input
	get_args(argc, argv, &settings, &bs);


	/////////////////////////////////////////////////////////////////////////////////////
	// intialize input stream
	cout << "Initializing InputReader" << endl;
	InputReader * InputStream=new InputReader();
	InputStream->Initialize(&settings);

	// initialize memory buffers..
	cout << "Allocating memory" << endl;
	unsigned int imagesize = settings.rows*settings.cols;
	unsigned char * ImageBuffer = new unsigned char [imagesize*settings.nChannels];
	unsigned char* diffImage = new unsigned char[imagesize*settings.nChannels];
	unsigned char* mask = new unsigned char[imagesize];
	/*
	unsigned char * FGImage = new unsigned char [imagesize*(settings.useColor? 3 : 1)];
	unsigned int nMasks = 2*(1+(settings.useFgUpdate? 1:0)+(settings.useMRF? 1:0));
	cout << "# of masks = " << nMasks << endl;
	unsigned char **DisplayBuffers = new unsigned char * [nMasks];
	for (int i  = 0; i < nMasks; i++) 
		DisplayBuffers[i] = new unsigned char [imagesize];
	// DisplayBuffers[0] = Mask of FG using uniform FG model
	// DisplayBuffers[1] = Likelihood Ratio of FG using uniform FG model
	// DisplayBuffers[2] = Mask of FG using adaptive FG model
	// DisplayBuffers[3] = Likelihood Ratio of FG using adaptive FG model
	// DisplayBuffers[4] = Mask of FG using MRF
	// DisplayBuffers[5] = Likelihood Ratio of FG using MRF
	*/
	/////////////////////////////////////////////////////////////////////////////////////
	// setup Model
	cout << "Setting up Model" << endl;
	Bhatta *p = new Bhatta(&settings, &bs);

	/////////////////////////////////////////////////////////////////////////////////////
	// setup background
	cout << "Building background model" << endl;
	int FrameIndex  = settings.BgStartFrame;
	int stopFrame = settings.BgStartFrame+settings.NumBgFrames;
	
	while (FrameIndex < stopFrame) {
	  // Get input frame
	  InputStream->GetFrame(FrameIndex,ImageBuffer);
	  // add frame to the background
	  time (&start);
	  p->populateBg(ImageBuffer);
	  time (&end);
	  cout << "Computation time for populateBg(): " << difftime(end,start) << endl;
	  FrameIndex++;
	}



	/////////////////////////////////////////////////////////////////////////////////////
	// compute fg        
	cout << "Extracting foreground" << endl;
	int bgCount = 0;
	FrameIndex  = settings.StartFrame;
	 stopFrame = settings.StartFrame+settings.Step*settings.NumFrames;
	while (FrameIndex < stopFrame) {
		InputStream->GetFrame(FrameIndex,ImageBuffer);
		time (&start);
		p->estimateFg(ImageBuffer, diffImage, mask );
		time (&end);
		cout << "Computation time for estimateFg(): " << difftime(end,start) << endl;
		bgCount++;
		if (bgCount == settings.BgStep) {
			cout << "Updating bg" << endl;
			if (bs.useBgUpdateMask) 
				p->updateBg(ImageBuffer, diffImage, mask);
			else 
				p->updateBg(ImageBuffer, diffImage, 0);
			bgCount = 0;
		}

		InputStream->SaveResults(0,diffImage, "Diff_");
		InputStream->SaveResults(0, mask, "Mask_");
		InputStream->SaveResults(ImageBuffer, mask, "Fg_");
		FrameIndex +=settings.Step;
	} 

	return 0;
}

*/
