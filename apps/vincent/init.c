#include "process.h"


//-----------------------------------------------
//---------------------init_mmap-----------------
//-----------------------------------------------
//To init the mmap and the buffers

void init_mmap(){
   
	struct v4l2_requestbuffers reqbuf;
   
	//requesting buffers
    printf("Requesting the buffer ...\n\n");
    CLEAR(reqbuf);
    reqbuf.count = 4;
    reqbuf.type	= V4L2_BUF_TYPE_VIDEO_CAPTURE;
    reqbuf.memory	= V4L2_MEMORY_MMAP;
   
    if (-1 == xioctl (fd, VIDIOC_REQBUFS, &reqbuf)) {
       errno_exit ("VIDIOC_REQBUFS");
    } 
           
    if (reqbuf.count < 2) {
       fprintf (stderr, "Insufficient buffer memory on %s\n",device_name);
       exit (-1);
    }
      
   
   
   
    //mmap buffers
	printf("Creating the buffers ... \n\n");
      
    buffers = calloc (reqbuf.count, sizeof (*buffers));
    if (!buffers) {
       fprintf (stderr, "Out of memory\n");
       exit (-1);
    }
   
	for (n_buffers = 0; n_buffers < reqbuf.count; ++n_buffers) {
       struct v4l2_buffer buf;
         
       CLEAR(buf);
         
       buf.type	= V4L2_BUF_TYPE_VIDEO_CAPTURE;
       buf.memory	= V4L2_MEMORY_MMAP;
       buf.index	= n_buffers;
      	
       if (-1 == xioctl (fd, VIDIOC_QUERYBUF, &buf))
          errno_exit ("VIDIOC_QUERYBUF");
      	
       buffers[n_buffers].length = buf.length;
       buffers[n_buffers].start = mmap (NULL, buf.length, PROT_READ | PROT_WRITE, MAP_SHARED, fd, buf.m.offset);
         
       if (-1 == xioctl (fd, VIDIOC_QBUF, &buf))
          errno_exit ("VIDIOC_QBUF");        
    }  
}





//---------------------------------------------
//--------------------init---------------------
//--------------------------------------------
//To init the device : format, standard, capabilities, ...

void init(){
   
	struct v4l2_capability cap;
    struct v4l2_format fmt;
   
    //capabilities
    if (-1 == xioctl (fd, VIDIOC_QUERYCAP, &cap)) {
       if (EINVAL == errno) {
          fprintf (stderr, "%s is no V4L2 device\n",device_name);
          exit (-1);
       } 
       else {
          errno_exit ("VIDIOC_QUERYCAP");
       }
    }
    
     
    printf ("Driver: %s\n", cap.driver);
    printf ("Card: %s\n\n", cap.card);
    printf ("Capabilities:\n");
    if (cap.capabilities & V4L2_CAP_VIDEO_CAPTURE)    printf ("capture\n");
    if (cap.capabilities &V4L2_CAP_STREAMING)    printf ("streaming\n");
    if (cap.capabilities & V4L2_CAP_READWRITE)   printf ("READ/WRITE\n\n");
     
      
   
    //setting the format
    printf("Setting the format ...\n");
    CLEAR(fmt);
    fmt.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    fmt.fmt.pix.width = width;
    printf("width: %d\n", fmt.fmt.pix.width);
    fmt.fmt.pix.height = height;
    printf("height: %d\n", fmt.fmt.pix.height);
    //fmt.fmt.pix.pixelformat = V4L2_PIX_FMT_YUYV;
    if(strcasecmp((char *)cap.driver,"SentechUSB")==0){
       palette=1;
       fmt.fmt.pix.pixelformat	= V4L2_PIX_FMT_BGR24;
    }
    else if(strcasecmp((char *)cap.driver,"pwc")==0){
       palette=2;
       fmt.fmt.pix.pixelformat	= V4L2_PIX_FMT_YUV420;
    }
    else if(strcasecmp((char *)cap.driver,"uvcvideo")==0){
       palette=3;
       fmt.fmt.pix.pixelformat = V4L2_PIX_FMT_YUYV;
    }
   
    fmt.fmt.pix.field	= V4L2_FIELD_NONE;
    //fmt.fmt.pix.bytesperline	= fmt.fmt.pix.width*2;
      
    if (-1 == xioctl (fd, VIDIOC_S_FMT, &fmt))
       errno_exit ("VIDIOC_S_FMT");
        
    size_data_rgb24=3*height*width;
    size_data_yuv420=1.5*height*width;
    size_data_yuyv=2*height*width;    

    init_mmap();
}

