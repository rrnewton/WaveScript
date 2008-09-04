#ifndef CAMERA_H
#define CAMERA_H

//---------------------Libraries--------------------
#include <stdint.h>
#include <stdio.h>
#include <math.h> 
#include <stdlib.h>
#include <fcntl.h>             
#include <errno.h>
#include <sys/ioctl.h>
#include <linux/videodev2.h>
#include <string.h>
#include <sys/mman.h> 
#include <assert.h>
#include <pthread.h>
#include <time.h>
#include <unistd.h>
#include <signal.h>


#define CLEAR(x) memset (&(x), 0, sizeof (x))

//device name
char * device_name;
//fd represents the device
int fd;

int capturing;

//struct for buffer
struct {
   void *start;
   size_t length;
} *buffers;
//number of buffers
unsigned int n_buffers;

//To know the format
int palette;

//size of data (change with format)
int size_data_rgb24;
int size_data_yuv420;
int size_data_yuyv;

//Conf options
   int width;
   int height;
   int framerate;
   int brightness;
   int contrast;
   int saturation;
   int hue;
   int exposure;
   int white_balance;
   int gain;
   float gama;

//To count the framerate
time_t timestamp;
struct tm * t;
int sec;
int frame;

void errno_exit (const char *s);
int xioctl (int fd, int request, void *arg);

int read_frame(int);
void mainloop();
void start_capturing();
void stop_capturing();
void init();
void init_mmap();
void change_control();

void wssink(int*);

//Entry point th send the return value
void wsentry1(int*);
//Entry point to send the data
void wsentry2(char*);


#endif




