
/* 
  Teresa Ko's background subtraction code, ported from C++ to WS by Ryan Newton.
  [2008.06.27]

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
										
		0,	// rows
		0,	// cols
		0,	// nChannels
		
		true    // useBgModel		
	      );
	
bhattasettings = (
	        (16 :: Int),   // NumBins1
		(2  :: Int),   // NumBins2
		(2  :: Int),   // NumBins3
		(30 :: Int),   // SizePatch
		(0  :: Double) // Alpha (bg update)
	);


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






// Builds background model histograms for each pixel.  
// Additions to the histograms are scaled according the assumption that 
// it will receive settings->NumBgFrames # of images.
fun populateBg(image) {

  // Unpack the settings, ugly because we don't have records yet --rrn	
  let (NumBins1, NumBins2, NumBins3, SizePatch, Alpha) = bhattasettings;
  let (Filename, OutLoc, BgStartFrame, StartFrame, 
       NumBgFrames, NumFg, BgStep, FgStep, Threshold,
       rows, cols, nChannels, useBgModel) = settings;

  // Patches are centered around the pixel.  [p.x p.y]-[offset offset] gives the upperleft corner of the patch.				

  offset = SizePatch / 2;
  // To reduce divisions.  Used to take a pixel value and calculate the histogram bin it falls in.
  inv_sizeBins1 = 1.0 / ceilF(256 / NumBins1.gint); 
  inv_sizeBins2 = 1.0 / ceilF(256 / NumBins2.gint);
  inv_sizeBins3 = 1.0 / ceilF(256 / NumBins3.gint);
  // To reduce divisions.  Adjust weight so that a pixel's histogram will be normalized after all frames are received.
  sampleWeight = 1.0 / gint(SizePatch * SizePatch * NumBgFrames);
  
  nPixels =  rows * cols; 	
  //	int i = 0, k=0;  // k = current pixel index
  //int binB, binG, binR;
  //int r,c,ro,co,roi,coi;
	
	
  // Histograms are for each pixel by creating create a histogram of the left most pixel in a row.
  // Then the next pixel's histogram in the row is calculated by:
  //   1. removing pixels in the left most col of the previous patch from the histogram and 
  //   2. adding pixels in the right most col of the current pixel's patch to the histogram
	
  // create temp hist to store working histogram
  using Array;

  tempHist = build(NumBins1, fun(r)
              build(NumBins2, fun(b)
		make(NumBins3, 0.0)));
	
  for r = 0 to rows-1 { 
				
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
      binB = floatToInt$ image[i]   * inv_sizeBins1;
      binG = floatToInt$ image[i+1] * inv_sizeBins2;
      binR = floatToInt$ image[i+2] * inv_sizeBins3;
      // add to temporary histogram
      tempHist[binB][binG][binR] += sampleWeight;
      }
    }
    
    /*		
		// copy temp histogram to left most patch
		for (int cb = 0; cb < bSettings->NumBins1; cb++)
			for (int cg = 0; cg < bSettings->NumBins2; cg++)
				for (int cr = 0; cr < bSettings->NumBins3; cr++)
					bgHist[k][cb][cg][cr] += tempHist[cb][cg][cr];
		
		// increment pixel index
		k++;
		
		// compute the top row of histograms
		for (c = 1; c < settings->cols; c++) {
		
			// subtract left col
			co = c-offset-1;
			coi = (co < 0 ? -co-1 : (co >= settings->cols ? 2*settings->cols-1-co :co));
			for (ro = r-offset; ro < r-offset+bSettings->SizePatch; ro++) {
				roi = (ro < 0 ? -ro-1 : (ro >= settings->rows ? 2*settings->rows-1-ro :ro));
				i = (roi*settings->cols+coi)*3;
				binB = (int) (image[i]*inv_sizeBins1);
				binG = (int) (image[i+1]*inv_sizeBins2);
				binR = (int) (image[i+2]*inv_sizeBins3);
				if (tempHist[binB][binG][binR] < 0) cout << "error: underflow" << endl;
				
				tempHist[binB][binG][binR] -= sampleWeight;
			}
			
			// add right col
			co = c-offset+bSettings->SizePatch-1;
		  coi = (co < 0 ? -co-1 : (co >= settings->cols ? 2*settings->cols-1-co :co));
			for (ro = r-offset; ro < r-offset+bSettings->SizePatch; ro++) {
				roi = (ro < 0 ? -ro-1 : (ro >= settings->rows ? 2*settings->rows-1-ro :ro));
				i = (roi*settings->cols+coi)*3;
				binB = (int) (image[i]*inv_sizeBins1);
				binG = (int) (image[i+1]*inv_sizeBins2);
				binR = (int) (image[i+2]*inv_sizeBins3);
				tempHist[binB][binG][binR] += sampleWeight;
			}
			
			// copy over			
			for (int cb = 0; cb < bSettings->NumBins1; cb++)
				for (int cg = 0; cg < bSettings->NumBins2; cg++)
					for (int cr = 0; cr < bSettings->NumBins3; cr++)
						bgHist[k][cb][cg][cr] += tempHist[cb][cg][cr];
			
			// increment pixel index
			k++;
		}
*/	

	}

}
/*

void Bhatta::populateBg(unsigned char* image) {
	


}
 */


//void Bhatta::estimateFg(unsigned char* image, unsigned char* diffImage,unsigned char* mask ) {

//void Bhatta::updateBg(unsigned char* image,unsigned char* diffImage,unsigned char* mask )



main = {
  println("Bhattacharyya Differencing.");


  
  //SHELL("mkdir ");

  stopFrame = 10;

  iterate frame in timer$3 {
    state { cnt = 0;
            //p = Bhatta(settings);
          }
    if (cnt == 0)
    then println("Building background model...");
    if (cnt < stopFrame) then {

	  // Get input frame
	  //InputStream->GetFrame(FrameIndex,ImageBuffer);
	  // add frame to the background
          st = clock();
	  //populateBg(ImageBuffer);
	  //p->populateBg(ImageBuffer);
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

	cout << settings.OutLoc << endl;
	if (mkdir(settings.OutLoc, 0777) == -1)  // Create the directory
	  std::cout << "Directory already exists." << endl;
	cout << "Settings: " << endl;
	cout << " # of bins:             " << bs.NumBins1 << "," <<  bs.NumBins2 << ","  << bs.NumBins3 << endl;
	cout << " Size of patch:         " << bs.SizePatch << endl;
	cout << " # of Bg Frames:        " << settings.NumBgFrames << endl;
	cout << " Threshold:             " << settings.Threshold << endl;
	cout << " Alpha:                 " << bs.Alpha << endl;

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
