#ifndef CAPTURE_H
#define CAPTURE_H

//-----Libraries-----
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <linux/videodev2.h>
#include <unistd.h>
#include <signal.h>
#include <time.h>
#include <getopt.h>
#include <errno.h>
#include <sys/mman.h> 
#include <assert.h>


#define CLEAR(x) memset (&(x), 0, sizeof (x))

//Function for errors
void errno_exit (const char *s);
//Function in place of iotcl
int xioctl (int fd, int request, void *arg);


//Type Booleen
//typedef enum { FALSE=0, TRUE=1 } BOOLEEN;


//Struct for buffer
typedef struct {
	void *start;
	size_t length;
} cam_buffer;


//Header of bmp file
typedef struct {
	unsigned int BMPHeaderLenght;
      	unsigned int ReservedBit;
      	unsigned int DataOffset;
   
      	unsigned int DataHeaderLenght;
      	unsigned int width;
      	unsigned int height;
      	unsigned short Planes;
      	unsigned short BPP;
   
      	unsigned int Compression;
   
      	unsigned int ImageSize;
   
      	unsigned int ResX;
      	unsigned int ResY;
   
      	unsigned int ColorUsed;
      	unsigned int ColorImportant;
}bmp_header;


//Struct for the parameter
typedef struct {
	int width;
	int height;
	int framerate;
	int brightness;
   	int contrast;
	int saturation;
	int exposure;
   	int white_balance;
} list_param;


//Struct for the current image
typedef struct {
	int number;
	int sec;
	int frame;
	unsigned char* data;
} current_image;	


//Struct for the camera
typedef struct {
	int fd;
	char* name;

	int palette;
	int data_size;

	list_param param;	
	
	int capturing;
	unsigned int i_buffers;
	cam_buffer* buffers;
	
	bmp_header header;
	current_image image;

} cam_infos;

cam_infos camera;


//Other functions
void init();
void image_capture();
void de_init();

//Entry point 1
void wsentry1(int*);
//Entry point 2
void wsentry2(void*);


#endif
