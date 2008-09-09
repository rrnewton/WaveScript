


#include <stdio.h>
#include <dirent.h>
#include <opencv/cv.h>
#include <opencv/highgui.h>

#include "ws.h"

// Return type aping the structure of a WS tuple.
struct ws_readImage_return {
  uint8_t* fld1;
  int fld2;
  int fld3;
};

// returns a WS object -- an array of color values.
//uint8_t* 
IplImage* ws_readImage_load(const char* filename, int* dims) {
  IplImage* im;
  im = cvvLoadImage(filename);
  if (!im) { 
    printf("Tried to load <%s>\n", filename);
    fflush(stdout);
    wserror_fun("opencv_wrapper ws_readImage failed to load file.");
    exit(1);
  }
  dims[0] = im->height;
  dims[1] = im->width;
  dims[2] = im->nChannels;
  return im; 	
}

void ws_readImage_fill(IplImage* im, char* wsimg)
{
  memcpy(wsimg, im->imageData, im->imageSize);
  cvReleaseImage(&im);
}

//  part1 :: (String, Array Int) -> Pointer "IplImage*" = foreign("ws_readImage_load", cv_files);
//  part2 :: (Pointer "IplImage*", Array Char) -> ()    = foreign("ws_readImage_fill", cv_files);


ws_bool_t ws_writeImage(const char* filename, uint8_t* img, int width, int height, int nChannels) {
  IplImage* fgImage;

  //int nChannels = 3; // RGB by default.

  fgImage = cvCreateImage(cvSize(width,height), IPL_DEPTH_8U, nChannels);
  memcpy(fgImage->imageData, img, width * height * nChannels);
  //if (pControls->useHSV) cvCvtColor(fgImage, fgImage, CV_HSV2BGR);

  // The documentation does not even say if this is an error code!

  int err = cvSaveImage(filename, fgImage);
  /*
  if (err) {
    printf("cvSaveImage Failed with error code %d, when saving to filename %s\n", err, filename);
    wserror_fun("cvSaveImage returned non-zero error code");
  }
*/

  cvReleaseImage(&fgImage);   

  //printf("Wrote file %s\n", filename);

  return 0;
}
