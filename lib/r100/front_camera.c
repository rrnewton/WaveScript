/**
 *  Code specific for a VS6451 camera
 *  From Andrew Christian
 */

#include <stdlib.h>
#include <fcntl.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>

#include <linux/fs.h>
#include <linux/fb.h>

#include "cam.h"


// This is essentially a sub-class of camera_info.
struct vs6451_camera {
  struct camera_info info;

  unsigned short *scratch;      // Space for storing temporary data
  int             pixel_order;  // Pixel information (per frame)
  int             width;
  int             height;
};

/**
 * We assume test mode is at PRIVATE_BASE +6
 * 0 = normal
 * 1 = solid color 
 * 2 = 100% color bars
 * 3 = fade to gray color bars
 * 4 = PN9 (pseudo random)
 */

/**
 * Parse the Embedded data line format
 */

struct image_data {
  int model_id;
  int revision_number;
  int manufacturer_id;
  int smia_version;
  int frame_count;
  int pixel_order;
  int pixel_depth;
};

static void
image_data_set( struct image_data *idata, unsigned short cci, unsigned char data )
{
  switch (cci) {
  case 0x0000:
    idata->model_id = data << 8;
    break;
  case 0x0001:
    idata->model_id |= data;
    break;

  case 0x0002:
    idata->revision_number = data;
    break;

  case 0x0003:
    idata->manufacturer_id = data;
    break;

  case 0x0004:
    idata->smia_version = data;
    break;
  case 0x0005:
    idata->frame_count = data;
    break;
  case 0x0006:
    idata->pixel_order = data;
    break;
  case 0x000c:
    idata->pixel_depth = data;
    break;
  }
}

static void
parse_edlf( unsigned char *start, int bytes, struct image_data *idata )
{
  unsigned char *s   = start;
  unsigned char *end = start + bytes;
  unsigned short cci = 0;

  //  printf("Parsing EDLF\n");
  if ( *s++ != 0x0a )   // Check for valid starting byte
    return;

  while (1) {
    unsigned char tag, data;

    /* Extract the tag */
    if ((s-start+1) % 5 == 0)
      s++;
    if ( s >= end )
      return;
    tag = *s++;

    /* Extract the data byte */
    if ((s-start+1) % 5 == 0)
      s++;
    if ( s >= end )
      return;
    data = *s++;

    //    printf("Tag 0x%02x Data 0x%02x\n", tag, data );

    switch (tag) {
    case 0x07:
      //      printf("End of data\n");
      return;
    case 0xaa:
      cci = (cci & 0xff) | (((unsigned short) data) << 8);
      break;
    case 0xa5:
      cci = (cci & 0xff00) | data;
      break;
    case 0x5a:
      //      printf("CCI 0x%04x 0x%02x %d\n", cci, data, data );
      image_data_set( idata, cci, data );
      cci += 1;
      break;
    case 0x55:
      if ( data != 0x07 ) {
	printf("Invalid NULL data byte 0x%02x\n", data);
	return;
      }
      cci += 1;
      break;
    default:
      printf("Illegal tag 0x%02x\n", tag);
      return;
    }
  }
}

/** 
 * Unpack the bayer data into a sparsely populated scratch buffer.
 * We assume that the width and height are multiples of four.
 *
 * Raw10 format:
 *   Pixel 1 (9:2)
 *   Pixel 2 (9:2)
 *   Pixel 3 (9:2)
 *   Pixel 4 (9:2)
 *   Pixel 4 (1:0) Pixel 3 (1:0) Pixel 2 (1:0) Pixel 1 (1:0)
 */


static int
unpack_raw10( void *source, unsigned short *dest, int width, int height )
{
  int i,j;
  unsigned char  *in  = source;
  unsigned short *out = dest;
  struct image_data idata;

  memset( dest, 0, 2 * width * height );
  memset( &idata, 0, sizeof(idata) );
  
  for ( j = 0 ; j < height ; j++ ) {
    for ( i = 0 ; i < width ; i += 4 ) {
      *out++ = (((unsigned short) in[0]) << 2) | ((in[4]) & 0x03);
      *out++ = (((unsigned short) in[1]) << 2) | ((in[4] >> 2) & 0x03);
      *out++ = (((unsigned short) in[2]) << 2) | ((in[4] >> 4) & 0x03);
      *out++ = (((unsigned short) in[3]) << 2) | ((in[4] >> 6) & 0x03);
      in += 5;
    }
  }

  parse_edlf( source, width * 5 / 4, &idata );
  parse_edlf( source + width * 5 / 4, width * 5 / 4, &idata );

  return idata.pixel_order;
  
  /*
  unsigned char *s = source;
  for ( j = 0 ; j < 10 ; j++ ) {
    for ( i = 0 ; i < 40 ; i++ ) 
      printf("%02x ", s[i]);
    printf("\n");
    s += width * 5 / 4;
  }
  printf("\n");
  */

  /*
  for ( j = 0 ; j < 8 ; j++ ) {
    for ( i = 0 ; i < 8 ; i++ ) 
      printf("%4d ", scratch[i + j*width]);
    printf("\n");
  }
  printf("\n");
  */
}

/**
 * Color map gain table.
 * We map from an 8 bit color value to an appropriately shifted
 * entry in the RGB16 space
 */

static unsigned short red_gain[256];
static unsigned short green_gain[256];
static unsigned short blue_gain[256];

inline unsigned short clip( double x) { if (x<0) return 0; if (x>255) return 255; return x; }

static void
init_color_conversion( double red, double green, double blue )
{
  int i;

  for ( i = 0 ; i < 256 ; i++ ) {
    red_gain[i]   = (clip(red *i) & 0xf8) << 8;
    green_gain[i] = (clip(green*i) & 0xfc) << 3;
    blue_gain[i]  = clip(blue*i) >> 3;
  }
}

// rrn: Wow, unhygenic macros:

/* This version packs 10 bit values */
/*
#define PACK16(_r,_g,_b) \
  do { \
    *d++ = (((_r) & 0x3c0) << 6) | (((_g) & 0x3f0) << 1 ) | (((_b) >> 5)); \
    s++; \
  } while (0)
*/

/* This version uses the color map */
#define PACK16(_r,_g,_b) \
  do { \
    *d++ = red_gain[(_r) >> 2] | green_gain[(_g) >> 2] | blue_gain[(_b)>>2]; \
    s++; \
  } while (0)

#define PACK16_R(_b,_g,_r) \
  do { \
    *d++ = red_gain[(_r) >> 2] | green_gain[(_g) >> 2] | blue_gain[(_b)>>2]; \
    s++; \
  } while (0)

/**
 * Given a standard Bayer pattern of GR/BG, pack the array of pixel
 * values in to a 16bit RGB value
 */

static void
GRBG_pack_16RGB( unsigned short *source, unsigned short *dest, int width, int height )
{
  unsigned short *s = source;
  unsigned short *d = dest;
  int i, j;

  /* Top-left corner */
  PACK16( s[1], s[0], s[width] );

  /* Top row */
  for ( i = 1 ; i < width - 1 ; i += 2 ) {
    PACK16( s[0],  (s[-1] + s[width] + s[1]) / 3,  (s[width-1] + s[width+1]) / 2 );
    PACK16( (s[-1] + s[1]) / 2,  s[0], s[width] );
  }

  /* Top-right corner */
  PACK16( s[0], (s[-1] + s[width]) / 2, s[-1+width] );

  /* Middle rows */
  for ( j = 1 ; j < height - 1 ; j += 2 ) {
    /* Left side (starting BLUE) */
    PACK16( (s[1-width] + s[1+width]) / 2, (s[-width] + s[1] + s[width]) / 3, s[0] );

    /* Mid rows */
    for ( i = 1 ; i < width - 1 ; i += 2 ) {
      PACK16( (s[-width] + s[width]) / 2,  s[0], (s[1] + s[-1]) / 2);
      PACK16( (s[1-width] + s[1+width] + s[-1-width] + s[-1+width]) / 4,
	      (s[width] + s[-width] + s[1] + s[-1]) / 4,
	      s[0] );
    }

    /* Right side */
    PACK16( (s[width] + s[-width]) / 2, s[0], s[-1] );

    /* Left side, starting GREEN */
    PACK16( s[1], s[0], (s[-width] + s[width]) / 2);

    /* Mid rows */
    for ( i = 1 ; i < width - 1 ; i += 2 ) {
      PACK16( s[0], (s[width] + s[-width] + s[1] + s[-1] ) / 4,
	      (s[1-width] + s[1+width] + s[-1-width] + s[-1+width] ) / 4);
      PACK16( (s[-1] + s[1]) / 2, s[0], ( s[width] + s[-width] ) / 2);
    }

    /* Right side */
    PACK16( s[0], (s[width] + s[-width] + s[-1]) / 3, 
	    ( s[-1-width] + s[-1+width]) / 2 );
  }

  /* Bottom-left corner */
  PACK16( s[-width+1], (s[-width] + s[1]) / 2, s[0] );

  /* Bottom row */
  for ( i = 1 ; i < width - 1 ; i += 2 ) {
    PACK16( s[-width], s[0], (s[-1] + s[1]) / 2);
    PACK16( (s[-width-1] + s[-width+1]) / 2, (s[-1] + s[1] + s[-width]) / 3, s[0] );
  }

  /* Bottom-right corner */
  PACK16( s[-width], s[0], s[-1] );
}


/**
 * Given a standard Bayer pattern of GB/RG, pack the array of pixel
 * values in to a 16bit RGB value
 */

static void
GBRG_pack_16RGB( unsigned short *source, unsigned short *dest, int width, int height )
{
  unsigned short *s = source;
  unsigned short *d = dest;
  int i, j;

  /* Top-left corner */
  PACK16_R( s[1], s[0], s[width] );

  /* Top row */
  for ( i = 1 ; i < width - 1 ; i += 2 ) {
    PACK16_R( s[0],  (s[-1] + s[width] + s[1]) / 3,  (s[width-1] + s[width+1]) / 2 );
    PACK16_R( (s[-1] + s[1]) / 2,  s[0], s[width] );
  }

  /* Top-right corner */
  PACK16_R( s[0], (s[-1] + s[width]) / 2, s[-1+width] );

  /* Middle rows */
  for ( j = 1 ; j < height - 1 ; j += 2 ) {
    /* Left side (starting BLUE) */
    PACK16_R( (s[1-width] + s[1+width]) / 2, (s[-width] + s[1] + s[width]) / 3, s[0] );

    /* Mid rows */
    for ( i = 1 ; i < width - 1 ; i += 2 ) {
      PACK16_R( (s[-width] + s[width]) / 2,  s[0], (s[1] + s[-1]) / 2);
      PACK16_R( (s[1-width] + s[1+width] + s[-1-width] + s[-1+width]) / 4,
	      (s[width] + s[-width] + s[1] + s[-1]) / 4,
	      s[0] );
    }

    /* Right side */
    PACK16_R( (s[width] + s[-width]) / 2, s[0], s[-1] );

    /* Left side, starting GREEN */
    PACK16_R( s[1], s[0], (s[-width] + s[width]) / 2);

    /* Mid rows */
    for ( i = 1 ; i < width - 1 ; i += 2 ) {
      PACK16_R( s[0], (s[width] + s[-width] + s[1] + s[-1] ) / 4,
	      (s[1-width] + s[1+width] + s[-1-width] + s[-1+width] ) / 4);
      PACK16_R( (s[-1] + s[1]) / 2, s[0], ( s[width] + s[-width] ) / 2);
    }

    /* Right side */
    PACK16_R( s[0], (s[width] + s[-width] + s[-1]) / 3, 
	    ( s[-1-width] + s[-1+width]) / 2 );
  }

  /* Bottom-left corner */
  PACK16_R( s[-width+1], (s[-width] + s[1]) / 2, s[0] );

  /* Bottom row */
  for ( i = 1 ; i < width - 1 ; i += 2 ) {
    PACK16_R( s[-width], s[0], (s[-1] + s[1]) / 2);
    PACK16_R( (s[-width-1] + s[-width+1]) / 2, (s[-1] + s[1] + s[-width]) / 3, s[0] );
  }

  /* Bottom-right corner */
  PACK16_R( s[-width], s[0], s[-1] );
}


/**
 * Given a fliped/mirrored Bayer pattern of BG/GR, pack the array of pixel
 * values in to a 16bit RGB value
 */

static void
BGGR_pack_16RGB( unsigned short *source, unsigned short *dest, int width, int height )
{
  unsigned short *s = source;
  unsigned short *d = dest;
  int i, j;

  /* Top-left corner */
  PACK16( s[width+1], (s[1]+s[width])/2, s[0] );

  /* Top row */
  for ( i = 1 ; i < width - 1 ; i += 2 ) {
    PACK16( s[width], s[0], (s[-1]+s[1])/2);
    PACK16( (s[width-1]+s[width+1])/2, (s[-1]+s[1]+s[width])/3, s[0]);
  }

  /* Top-right corner */
  PACK16( s[width], s[-1], s[0] );

  /* Middle rows */
  for ( j = 1 ; j < height - 1 ; j += 2 ) {
    /* Left side (starting GREEN) */
    PACK16( s[1], s[0], (s[width]+s[-width]) / 2 );

    /* Mid rows */
    for ( i = 1 ; i < width - 1 ; i += 2 ) {
      PACK16( s[0], (s[width] + s[-width] + s[1] + s[-1]) / 4,
	      (s[1-width] + s[1+width] + s[-1-width] + s[-1+width]) / 4);
      PACK16( (s[-1]+s[1]) / 2, s[0], (s[width]+s[-width]) / 2);
    }

    /* Right side */
    PACK16( s[0], (s[-1] + s[-width] + s[width]) / 3, (s[width-1]+s[-width-1]) / 2);

    /* Left side, starting BLUE */
    PACK16( (s[1-width]+s[1+width]) / 2, (s[-width]+s[1]+s[width])/3, s[0]);

    /* Mid rows */
    for ( i = 1 ; i < width - 1 ; i += 2 ) {
      PACK16( (s[width]+s[-width])/2, s[0], (s[-1]+s[1])/2);

      PACK16( (s[1-width] + s[1+width] + s[-1-width] + s[-1+width] ) / 4,
	      (s[width] + s[-width] + s[1] + s[-1] ) / 4, s[0] );
    }

    /* Right side */
    PACK16( (s[width]+s[-width])/2, s[0], s[-1] );
  }

  /* Bottom-left corner */
  PACK16( s[1], s[0], s[-width] );

  /* Bottom row */
  for ( i = 1 ; i < width - 1 ; i += 2 ) {
    PACK16( s[0], (s[-1] + s[1] + s[-width]) / 3, (s[-width-1] + s[-width+1]) / 2);
    PACK16( (s[-1] + s[1]) / 2, s[0], s[-width]);
  }

  /* Bottom-right corner */
  PACK16( s[0], (s[-1]+s[1]) / 2, s[-width -1]);
}


/**
 * Given a fliped/mirrored Bayer pattern of RG/GB, pack the array of pixel
 * values in to a 16bit RGB value
 */

static void
RGGB_pack_16RGB( unsigned short *source, unsigned short *dest, int width, int height )
{
  unsigned short *s = source;
  unsigned short *d = dest;
  int i, j;

  /* Top-left corner */
  PACK16_R( s[width+1], (s[1]+s[width])/2, s[0] );

  /* Top row */
  for ( i = 1 ; i < width - 1 ; i += 2 ) {
    PACK16_R( s[width], s[0], (s[-1]+s[1])/2);
    PACK16_R( (s[width-1]+s[width+1])/2, (s[-1]+s[1]+s[width])/3, s[0]);
  }

  /* Top-right corner */
  PACK16_R( s[width], s[-1], s[0] );

  /* Middle rows */
  for ( j = 1 ; j < height - 1 ; j += 2 ) {
    /* Left side (starting GREEN) */
    PACK16_R( s[1], s[0], (s[width]+s[-width]) / 2 );

    /* Mid rows */
    for ( i = 1 ; i < width - 1 ; i += 2 ) {
      PACK16_R( s[0], (s[width] + s[-width] + s[1] + s[-1]) / 4,
	      (s[1-width] + s[1+width] + s[-1-width] + s[-1+width]) / 4);
      PACK16_R( (s[-1]+s[1]) / 2, s[0], (s[width]+s[-width]) / 2);
    }

    /* Right side */
    PACK16_R( s[0], (s[-1] + s[-width] + s[width]) / 3, (s[width-1]+s[-width-1]) / 2);

    /* Left side, starting BLUE */
    PACK16_R( (s[1-width]+s[1+width]) / 2, (s[-width]+s[1]+s[width])/3, s[0]);

    /* Mid rows */
    for ( i = 1 ; i < width - 1 ; i += 2 ) {
      PACK16_R( (s[width]+s[-width])/2, s[0], (s[-1]+s[1])/2);

      PACK16_R( (s[1-width] + s[1+width] + s[-1-width] + s[-1+width] ) / 4,
	      (s[width] + s[-width] + s[1] + s[-1] ) / 4, s[0] );
    }

    /* Right side */
    PACK16_R( (s[width]+s[-width])/2, s[0], s[-1] );
  }

  /* Bottom-left corner */
  PACK16_R( s[1], s[0], s[-width] );

  /* Bottom row */
  for ( i = 1 ; i < width - 1 ; i += 2 ) {
    PACK16_R( s[0], (s[-1] + s[1] + s[-width]) / 3, (s[-width-1] + s[-width+1]) / 2);
    PACK16_R( (s[-1] + s[1]) / 2, s[0], s[-width]);
  }

  /* Bottom-right corner */
  PACK16_R( s[0], (s[-1]+s[1]) / 2, s[-width -1]);
}

static void (*unpackers[])(unsigned short *, unsigned short *, int, int) = {
  GRBG_pack_16RGB,
  RGGB_pack_16RGB,
  BGGR_pack_16RGB,
  GBRG_pack_16RGB
};


// This undoes the bit-packing on the raw input.
static void 
vs6451_preprocess( struct camera_info   *caminfo,
		   struct screen_info   *screen,
		   void                 *data, 
		   int                   length )
{
  struct vs6451_camera *camera = (struct vs6451_camera *) caminfo;
  camera->pixel_order = unpack_raw10( data, camera->scratch, camera->width, camera->height );
}

// This undoes the bayer filter.
static void 
vs6451_postprocess( struct camera_info *caminfo,
                    unsigned short* dest)
{
  struct vs6451_camera *camera = (struct vs6451_camera *) caminfo;
  (unpackers[camera->pixel_order])( camera->scratch, 
                                    dest,
				    camera->width, 
				    camera->height );
}


static void
init_format( struct vs6451_camera *camera, 
	     int                   screen_width, 
	     int                   screen_height )
{
  struct v4l2_format format;
  
  memset( &format, 0, sizeof(format));
  format.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;

  if ( ioctl( camera->info.fd, VIDIOC_G_FMT, &format )) {
    perror("VIDIOC_G_FMT");
    exit(1);
  }

  format.fmt.pix.width  = screen_width;
  format.fmt.pix.height = screen_height;

  if ( ioctl( camera->info.fd, VIDIOC_S_FMT, &format )) {
    perror("VIDIOC_S_FMT");
    exit(1);
  }

  camera->width  = format.fmt.pix.width;
  camera->height = format.fmt.pix.height;

  // Making it a little bigger to hold the unpacked 24bit per RGB pixel representation
  // rrn: allocating WS side instead.
  camera->scratch = malloc( camera->width * camera->height * 3 );
  if ( !camera->scratch ) {
    perror("malloc");
    exit(1);
  }
}

#define DEF_GAIN     14
#define DEF_EXPOSURE 300
#define DEF_ORIENT   0
#define DEF_MODE     0


static void 
init_params( struct vs6451_camera *camera, 
	     struct params        *param )
{
  int fd = camera->info.fd;

  set_control( fd, V4L2_CID_GAIN,     ( param->flags & PARAM_GAIN ? param->gain : DEF_GAIN ));
  set_control( fd, V4L2_CID_EXPOSURE, ( param->flags & PARAM_EXPOSURE ? param->exposure : DEF_EXPOSURE ));

  switch ( (param->flags & PARAM_ORIENTATION ? param->orientation : DEF_ORIENT) ) { 
  case 0:
    set_control( fd, V4L2_CID_HFLIP, 0 );
    set_control( fd, V4L2_CID_VFLIP, 0 );
    break;
  case 1:
    set_control( fd, V4L2_CID_HFLIP, 1 );
    set_control( fd, V4L2_CID_VFLIP, 0 );
    break;
  case 2:
    set_control( fd, V4L2_CID_HFLIP, 1 );
    set_control( fd, V4L2_CID_VFLIP, 1 );
    break;
  case 3:
    set_control( fd, V4L2_CID_HFLIP, 0 );
    set_control( fd, V4L2_CID_VFLIP, 1 );
    break;
  }

  set_control( fd, V4L2_CID_PRIVATE_BASE + 6, ( param->flags & PARAM_MODE ? param->mode : DEF_MODE) );

  if ( (param->flags & PARAM_MODE) && param->mode ) {
    set_control( fd, V4L2_CID_PRIVATE_BASE + 7, param->red );
    set_control( fd, V4L2_CID_PRIVATE_BASE + 8, param->green );
    set_control( fd, V4L2_CID_PRIVATE_BASE + 9, param->blue );
    set_control( fd, V4L2_CID_PRIVATE_BASE + 10, param->green );
  }
}


static void 
vs6451_init( struct camera_info   *caminfo,
	     struct screen_info   *screen, 
	     struct params        *params )
{
  struct vs6451_camera *camera = (struct vs6451_camera *) caminfo;

  init_color_conversion( 0.8, 0.8, 2.2 );
  init_format( camera, screen->width, screen->height );
  init_params( camera, params );
}

static void
vs6451_start( struct camera_info *caminfo )
{
}

static void
vs6451_stop( struct camera_info *caminfo )
{
}

//struct camera_info *
struct vs6451_camera *
vs6451_new( int fd )
{
  struct vs6451_camera *c = malloc( sizeof(struct vs6451_camera));
  if ( !c ) {
    perror("malloc");
    exit(1);
  }

  c->info.fd          = fd;
  c->info.init        = vs6451_init;
  c->info.start       = vs6451_start;
  c->info.stop        = vs6451_stop;
  c->info.preprocess  = vs6451_preprocess;
  c->info.postprocess = vs6451_postprocess;

  //return (struct camera_info *) c;
  return  c;
}


//====================================================================================================//
//====================================================================================================//

/**
 *  Camera Test:  Copy data from a camera and send it to the frame buffer
 */

/* #include <stdlib.h> */
/* #include <fcntl.h> */
/* #include <stdio.h> */
/* #include <errno.h> */
/* #include <string.h> */
/* #include <unistd.h> */

/* #include <sys/types.h> */
/* #include <sys/stat.h> */
/* #include <sys/mman.h> */

//#include <linux/fb.h>
/* #include <linux/fs.h> */
/* #include "camtest.h" */

//extern struct camera_info *vs6451_new( int fd );
//extern struct camera_info *goldeneye_new( int fd );

void
init_screen( const char *device, struct screen_info *screen )
{
  struct fb_var_screeninfo v;

  screen->fd = open( device, O_RDWR );
  if (screen->fd < 0) {
    printf("Unable to open frame buffer device /dev/fb0\n");
    exit(1);
  }

  if (ioctl (screen->fd, FBIOGET_VSCREENINFO, &v)) {
    perror("Unable to FBIOGET_VSCREENINFO");
    exit(1);
  }

  if ( v.rotate != 0 && v.rotate != 180 ) {
    screen->actual_width  = v.yres;
    screen->actual_height = v.xres;
  } else {
    screen->actual_width  = v.xres;
    screen->actual_height = v.yres;
  }

  screen->width   = v.xres;
  screen->height  = v.yres;
  screen->rotate  = v.rotate;
  screen->bpp     = v.bits_per_pixel;
  screen->fb_size = (v.yres * v.xres * v.bits_per_pixel) / 8;
  screen->fb      = mmap( (void *)0, screen->fb_size, PROT_READ | PROT_WRITE, MAP_SHARED, screen->fd, 0);
  
  if (screen->fb == MAP_FAILED) {
    perror("Unable to MAP memory");
    exit(1);
  }

  printf("Screen info: width=%d height=%d bpp=%d fb_size=%d\n", 
	 screen->width, screen->height, screen->bpp, screen->fb_size );
}

void
set_control( int fd, int id, int value )
{
  struct v4l2_control   control;

  control.id    = id;
  control.value = value;
  printf("%s: id=0x%lx value=0x%x\n", __func__, id, value );

  if ( ioctl( fd, VIDIOC_S_CTRL, &control )) {
    perror( "VIDIOC_S_CTRL" );
    exit(1);
  }
}

int
get_control( int fd, int id )
{
  struct v4l2_control   control;

  control.id    = id;
  control.value = 0;
  //  printf("%s: id=0x%lx\n", __func__, id );

  if ( ioctl( fd, VIDIOC_G_CTRL, &control )) {
    perror( "VIDIOC_G_CTRL" );
    exit(1);
  }
  
  return control.value;
}


char *
decode_pixelformat( unsigned int format )
{
  static char buf[5];
  buf[0] = format & 0xff;
  buf[1] = (format & 0xff00) >> 8;
  buf[2] = (format & 0xff0000) >> 16;
  buf[3] = (format & 0xff000000) >> 24;
  buf[4] = 0;
  return buf;
}

//struct camera_info *
struct vs6451_camera *
init_camera( char *device )
{
  struct v4l2_input    input;
  struct v4l2_standard standard;
  struct v4l2_fmtdesc  desc;
  struct v4l2_capability cap;
  int index;
  int fd;

  fd = open( device, O_RDWR );
  if ( fd < 0 ) {
    perror("Unable to open video device");
    exit(1);
  }

  if ( ioctl( fd, VIDIOC_QUERYCAP, &cap )) {
    perror("Unable to ioctl camera\n");
    exit(1);
  }

  printf("Video driver='%s' card='%s' bus_info='%s' version=%d capabilities=0x%lx\n", 
	 cap.driver, cap.card, cap.bus_info,
	 cap.version, cap.capabilities );

  if ( !(cap.capabilities & V4L2_CAP_VIDEO_CAPTURE) ) {
    printf("Not a capture device\n");
    exit(1);
  }

  if (!(cap.capabilities & V4L2_CAP_STREAMING)) {
    printf("Doesn't support streaming\n");
    exit(1);
  }

  memset( &input, 0, sizeof(input) );
  input.index = 0;
  while ( !ioctl( fd, VIDIOC_ENUMINPUT, &input)) {
    printf("Input %d name='%s', type=%d, audioset=0x%lx, tuner=0x%lx std=0x%llx status=0x%lx\n",
	   index, input.name, input.type, input.audioset, input.tuner, input.std, input.status );
    input.index += 1;

    if ( input.type != V4L2_INPUT_TYPE_CAMERA ) {
      printf("Not a camera\n");
      exit(1);
    }
  }
  
  memset (&standard, 0, sizeof (standard));

  while ( !ioctl(fd, VIDIOC_ENUMSTD, &standard) ) {
    if (standard.id & input.std)
      printf(" %s\n", standard.name);
    standard.index++;
  }

  memset( &desc, 0, sizeof( desc ));
  desc.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  while ( !ioctl( fd, VIDIOC_ENUM_FMT, &desc )) {
    printf("Format %d: type=%d, flags=0x%lx, description='%s', pixelformat='%s'\n",
	   desc.index, desc.type, desc.flags, desc.description, 
	   decode_pixelformat( desc.pixelformat));
    desc.index += 1;

    switch (desc.pixelformat) {
      //    case V4L2_PIX_FMT_RGB565:  // Goldeneye
      //      return goldeneye_new( fd );
    case v4l2_fourcc('B','A','1','0'):
      return vs6451_new( fd );
    }
  }

  return NULL;
}


void
enqueue( int fd, unsigned int index )
{
  struct v4l2_buffer buffer;

  memset( &buffer, 0, sizeof(buffer));
  buffer.type   = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  buffer.memory = V4L2_MEMORY_MMAP;
  buffer.index  = index;

  //  printf("Enqueuing type=%d memory=%d index=%u\n", buffer.type, buffer.memory, buffer.index );
  if ( ioctl( fd, VIDIOC_QBUF, &buffer )) {
    perror("VIDIOC_QBUF");
    exit(1);
  }
}

unsigned int
dequeue( int fd )
{
  struct v4l2_buffer buffer;

  memset( &buffer, 0, sizeof(buffer));
  buffer.type   = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  buffer.memory = V4L2_MEMORY_MMAP;

  if ( ioctl( fd, VIDIOC_DQBUF, &buffer )) {
    perror("VIDIOC_DQBUF");
    exit(1);
  }

  return buffer.index;
}
  
struct {
  void *start;
  size_t length;
} *buffers;


static void
init_memory_map( struct camera_info *camera )
{
  unsigned int i;
  struct v4l2_requestbuffers reqbuf;

  /*
  printf("Width=%d height=%d pixelformat='%s' field=%d bytesperline=%d sizeimage=%d colorspace=%d\n",
	 camera->format.fmt.pix.width, 
	 camera->format.fmt.pix.height, 
	 decode_pixelformat( camera->format.fmt.pix.pixelformat),
	 camera->format.fmt.pix.field, 
	 camera->format.fmt.pix.bytesperline, 
	 camera->format.fmt.pix.sizeimage,
	 camera->format.fmt.pix.colorspace );
  */
  /* Set up streaming I/O */

  memset( &reqbuf, 0, sizeof(reqbuf));
  reqbuf.type   = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  reqbuf.memory = V4L2_MEMORY_MMAP;
  reqbuf.count  = 3;

  if ( ioctl(camera->fd, VIDIOC_REQBUFS, &reqbuf )) {
    perror("VIDIOC_REQBUFS");
    exit(1);
  }

  printf("Requested buffer count=%d\n", reqbuf.count);

  buffers = calloc( reqbuf.count, sizeof(*buffers));
  for ( i = 0 ; i < reqbuf.count ; i++ ) {
    struct v4l2_buffer buffer;

    memset( &buffer, 0, sizeof(buffer));
    buffer.type   = reqbuf.type;
    buffer.memory = V4L2_MEMORY_MMAP;
    buffer.index  = i;
    
    if ( ioctl( camera->fd, VIDIOC_QUERYBUF, &buffer )) {
      perror("VIDIOC_QUERYBUF");
      exit(1);
    }

    buffers[i].length = buffer.length;   /* Used in munmap */
    buffers[i].start  = mmap( NULL, buffer.length,
			      PROT_READ | PROT_WRITE,
			      MAP_SHARED,
			      camera->fd, buffer.m.offset );

    if ( buffers[i].start == MAP_FAILED ) {
      perror("mmap");
      exit(1);
    }
  }

  /* Enqueue the buffers */
  for ( i = 0 ; i < reqbuf.count ; i++ )
    enqueue( camera->fd, i );

  /* Start the stream */
  int arg = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  if ( ioctl( camera->fd, VIDIOC_STREAMON, &arg )) {
    perror("VIDIOC_STREAMON");
    exit(1);
  }
}


static void
set_fps( int fd, int fps )
{
  struct v4l2_streamparm parm;

  parm.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  parm.parm.capture.timeperframe.numerator = 1;
  parm.parm.capture.timeperframe.denominator = fps;
  
  if ( ioctl( fd, VIDIOC_S_PARM, &parm )) {
    perror("VIDIOC_S_PARM");
    exit(1);
  }
}


//====================================================================================================//
//====================================================================================================//

// rrn: Hooks for WS:

// Making these global for WS.
struct screen_info ws_screen;
//struct camera_info *ws_camera;
struct vs6451_camera *ws_camera;


int wsinit( int argc, char *argv[] )  {
  // Need to assert this for our WS code to cooperate wrt data reps:
  // assert(sizeof(unsigned short) == 2);

  struct params params;
  int opt;

  struct camera_info* cam;

  params.flags = 0;
  params.red = 1023;
  params.blue = 1023;
  params.green = 1023;

  init_screen( "/dev/fb0", &ws_screen );
  ws_camera = init_camera( "/dev/video0" );
  if ( !ws_camera ) {
    printf("Unable to identify this camera\n");
    exit(1);
  }
  cam = (struct camera_info*) ws_camera;

  (cam->init)( cam, &ws_screen, &params );
  if ( params.flags & PARAM_FPS ) 
    set_fps( cam->fd, params.fps );

  init_memory_map( cam );
  (cam->start)( cam );

  printf("Camera initialized...\n");
  fflush(stdout);
}

// This is the entrypoint into WS:
//extern void ws_camera_hookup( unsigned short* );
extern void ws_camera_hookup( ws_unit_t );

int wsmain( int argc, char *argv[] )  {
 
  struct camera_info* cam = (struct camera_info*) ws_camera;
  int i, pix = ws_camera->width * ws_camera->height;
  unsigned short* tmp = malloc( pix * sizeof(unsigned short) );

  //unsigned char* unpacked = malloc( bufsize );
  unsigned char* unpacked;

  printf("wsmain starting...\n");
  fflush(stdout);

  while (1)  { 
    // Video 4 Linux gives us the frames when it has them:
    unsigned int index = dequeue( cam->fd );
    // But the frames are raw, need to convert.
    (cam->preprocess)( cam, &ws_screen, buffers[index].start, buffers[index].length );
    //(camera->preprocess)( camera, buffers[index].start, buffers[index].length );
    // This must tell v4l that it can reuse the buffer?
    enqueue( cam->fd, index );

    // Now we undo the bayer filter into a temporary.:
    //(cam->postprocess)( cam, ws_screen.fb );
    //(cam->postprocess)( cam, ws_camera->scratch );
    (cam->postprocess)( cam, tmp );

    // This is inefficient, now we unpack one more step and make an array of chars.
    unpacked = (unsigned char*)ws_camera->scratch;    

    // If in place, need to do this from the back end so as not to overwrite data.    
    // Not in place currently:
    for (i=0; i<pix; i++) {
      //printf("unpacking %p index %d\n", unpacked, i);
      
      // Unpack into the HIGH bits.
      *unpacked++ = (tmp[i] >> (5 + 6 - 3)) & (255 - 7) ; // Red
      *unpacked++ = (tmp[i] >> (5 - 2))     & (255 - 3) ; // Green
      *unpacked++ = (tmp[i] << 3)                       ; // Blue

      // Unpack into the LOW bits:
      //*unpacked++ = (tmp[i] >> 11) & 31;
      //*unpacked++ = (tmp[i] >> 5)  & 63;
      //*unpacked++ = (tmp[i] )          ;
    }

    // Drive a tick to WS to let it know that the frame is ready.
    ws_camera_hookup(0);
  }
}

// rrn: This is a mess because I'm trying to do a minimal hack to Andy's code.
// This function takes a buffer as argument rather than implicitly
// using the globally registered (set_cam_scratch) buffer.
void display_to_screen(unsigned char* buf) {
  struct camera_info* cam = (struct camera_info*) ws_camera;
  unsigned short* tmp_bak = ws_camera->scratch;
  int i, j, pix = ws_camera->width * ws_camera->height;
  unsigned short* scratch;  

  printf("Putting frame to screen..\n");
  fflush(stdout);

  ws_camera->scratch = (unsigned short*) buf;
  scratch = (unsigned short*) buf;

  // Pack it tighter -- IN PLACE TRANSFORM:
  j = 0;
  for (i=0; i<pix; i++) {
    // When converting the 24 bit color rep, we use the 5 or 6 HIGH bits of each byte.
    // Pack from the HIGH bits:
    // This requires clearing the low bits of the each 8-bit pixel before repacking.
    scratch[i] = ((unsigned short)(buf[j]   & (255 - 7)))  << (5 + 6 - 3)
               | ((unsigned short)(buf[j+1] & (255 - 3)))  << (5     - 2)
               | ((unsigned short) buf[j+2])               >> 3;
    // Pack from the LOW bits:
    //scratch[i] = ((unsigned short)buf[j])   << 11
    //           | ((unsigned short)buf[j+1]) << 5
    //           | ((unsigned short)buf[j+2]) ;
    j+=3;
  }

  // put the frame to screen:
  memcpy(ws_screen.fb, ws_camera->scratch, pix * sizeof(unsigned short));

  // Put things back where they were.
  ws_camera->scratch = tmp_bak;
}

int cam_width () { return ws_camera->width; }
int cam_height() { return ws_camera->height; }
void set_cam_scratch(unsigned short* buf) { ws_camera->scratch = buf; }
//void set_cam_scratch()

void ws_front_cam_getframe() {  
  
}

//int main( int argc, char *argv[] )  {}
