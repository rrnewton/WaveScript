/**
 *  Simple Camera driver:  Copy data from a camera and send it to the frame buffer
 *  From Andrew Christian
 */

#ifndef _CAM_H
#define _CAM_H

// Video for linux Two header file:
#include <linux/videodev2.h>

struct screen_info {
  int fd;
  int width;   /* Current width    */
  int height;  /* Current height   */
  int actual_width;
  int actual_height;
  int rotate;  /* Current rotation */
  int bpp; /* bit per pixel */
  unsigned short *fb;
  int fb_size;
};

#define PARAM_GAIN        0x0001
#define PARAM_EXPOSURE    0x0002
#define PARAM_MODE        0x0004
#define PARAM_ORIENTATION 0x0008
#define PARAM_FPS         0x0010

struct params {
  int flags;
  int gain;
  int exposure;
  int mode;
  int orientation;
  int red;
  int green;
  int blue;
  int fps;
};

/* This struct will be extended in the private sections */

struct camera_info {
  int fd;
  void (*init)( struct camera_info *camera, struct screen_info *screen, struct params *params );
  void (*start)( struct camera_info *camera );
  void (*stop)( struct camera_info *camera );
  void (*preprocess)( struct camera_info *camera, struct screen_info *screen, void *data, int length );
  //void (*preprocess)( struct camera_info *camera, void *data, int length );
  //void (*postprocess)( struct camera_info *camera, struct screen_info *screen );
  void (*postprocess)( struct camera_info *camera, unsigned short* dest);
};

extern void  set_control( int fd, int id, int value );
extern int   get_control( int fd, int id );
extern char *decode_pixelformat( unsigned int format );


#endif /* _CAM_H */
