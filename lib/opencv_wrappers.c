


#include <stdio.h>
#include <dirent.h>
#include <opencv/cv.h>
#include <opencv/highgui.h>

// Return type aping the structure of a WS tuple.
struct ws_readImage_return {
  uint8_t* fld1;
  int fld2;
  int fld3;
};

// returns a WS object -- an array of color values.
//uint8_t* 
void*
 ws_readImage(const char* filename) {
  IplImage* im;
  //printf("Executing readimage %s\n", filename);
  im = cvvLoadImage(filename);
  //printf("Read image %d\n", im);
  	
  //if (!im) wserror_fun(sprintf("opencv_wrapper ws_readImage failed to load file: %s", filename));
  if (!im) wserror_fun("opencv_wrapper ws_readImage failed to load file: %s");

  int rows = im->height;
  int cols = im->width;
  int nChannels = im->nChannels;
  	
  //printf("   Height/width/channels: %d %d %d\n", rows, cols, nChannels);

  char* wsimg = WSARRAYALLOC(im->imageSize, uint8_t);
  memcpy(wsimg, im->imageData, im->imageSize);
  cvReleaseImage(&im);

  //printf("   Copied to WS and released...\n");
  
  //return wsimg;
  //struct ws_readImage_return tuple = {wsimg, cols, rows};
  struct ws_readImage_return* tuple = malloc(sizeof(struct ws_readImage_return));
  tuple->fld1 = wsimg;
  tuple->fld2 = cols;
  tuple->fld3 = rows;
  return tuple;
}


ws_bool_t ws_writeImage(const char* filename, uint8_t* img, int width, int height, int nChannels) {
  IplImage* fgImage;

  //int nChannels = 3; // RGB by default.
		
  fgImage = cvCreateImage(cvSize(width,height), IPL_DEPTH_8U, nChannels);
  memcpy(fgImage->imageData, img, width * height * nChannels);
  //if (pControls->useHSV) cvCvtColor(fgImage, fgImage, CV_HSV2BGR);

  cvSaveImage(filename, fgImage);
  cvReleaseImage(&fgImage);   

  //printf("Wrote file %s\n", filename);

  return 0;
}
