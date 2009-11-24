#include "capture.h"


void init() {

	struct v4l2_capability cap;
    	struct v4l2_format fmt;
	struct v4l2_requestbuffers reqbuf;


	//Requesting the capabilities
	if (-1 == xioctl (camera.fd, VIDIOC_QUERYCAP, &cap)) {
         	if (EINVAL == errno) {
            		fprintf (stderr, "%s is no V4L2 device\n",camera.name);
            		exit (-1);
         	} 
        	else errno_exit ("VIDIOC_QUERYCAP");
      	}
      
      	printf ("Driver: %s\n", cap.driver);
      	printf ("Card: %s\n\n", cap.card);
      	printf ("Capabilities:\n");
      	if (cap.capabilities & V4L2_CAP_VIDEO_CAPTURE) 		printf ("capture\n");
      	if (cap.capabilities &V4L2_CAP_STREAMING)  	 	printf ("streaming\n");
      	if (cap.capabilities & V4L2_CAP_READWRITE)  		printf ("READ/WRITE\n");	
	


	//setting the format
      	printf("\nSetting the format:\n");
      	CLEAR(fmt);
      	fmt.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
      	fmt.fmt.pix.width = camera.param.width;
      	printf("width: %d\n", fmt.fmt.pix.width);
      	fmt.fmt.pix.height = camera.param.height;
      	printf("height: %d\n", fmt.fmt.pix.height);
   	//fmt.fmt.pix.pixelformat = V4L2_PIX_FMT_YUYV;
      	if(strcasecmp((char *)cap.driver,"SentechUSB")==0){
         	camera.palette=1;
         	fmt.fmt.pix.pixelformat	= V4L2_PIX_FMT_BGR24;
		camera.data_size = 3*camera.param.width*camera.param.height;
      	}
      	else if(strcasecmp((char *)cap.driver,"pwc")==0){
         	camera.palette=2;
         	fmt.fmt.pix.pixelformat	= V4L2_PIX_FMT_YUV420;
		camera.data_size = 1.5*camera.param.width*camera.param.height;
      	}
      	else if(strcasecmp((char *)cap.driver,"uvcvideo")==0){
         	camera.palette=3;
         	fmt.fmt.pix.pixelformat = V4L2_PIX_FMT_YUYV;
		camera.data_size = 2*camera.param.width*camera.param.height;
      	}
   
      	fmt.fmt.pix.field = V4L2_FIELD_NONE;
      
      	if (-1 == xioctl (camera.fd, VIDIOC_S_FMT, &fmt))
         	errno_exit ("VIDIOC_S_FMT");



	//requesting buffers
      	printf("\n\nRequesting the buffer\n\n");
      	CLEAR(reqbuf);
      	reqbuf.count = 4;
      	reqbuf.type	= V4L2_BUF_TYPE_VIDEO_CAPTURE;
      	reqbuf.memory	= V4L2_MEMORY_MMAP;
   
      	if (-1 == xioctl (camera.fd, VIDIOC_REQBUFS, &reqbuf))
	         errno_exit ("VIDIOC_REQBUFS");
           
      	if (reqbuf.count < 2) {
         	fprintf (stderr, "Insufficient buffer memory on %s\n",camera.name);
         	exit (-1);
      	}
      

   
   	//mmap buffers
      	printf("Creating the buffers.\n\n");
      
      	camera.buffers = (cam_buffer*) calloc (reqbuf.count, sizeof(cam_buffer));
      	if (!camera.buffers) {
         	fprintf (stderr, "Out of memory\n");
         	exit (-1);
      	}
   
      	for (camera.i_buffers = 0; camera.i_buffers < reqbuf.count; ++camera.i_buffers) {
        	struct v4l2_buffer buf;
         
         	CLEAR(buf);
         
         	buf.type	= V4L2_BUF_TYPE_VIDEO_CAPTURE;
         	buf.memory	= V4L2_MEMORY_MMAP;
         	buf.index	= camera.i_buffers;
      	
         	if (-1 == xioctl (camera.fd, VIDIOC_QUERYBUF, &buf))
            		errno_exit ("VIDIOC_QUERYBUF");
      	
         	camera.buffers[camera.i_buffers].length = buf.length;
         	camera.buffers[camera.i_buffers].start = mmap (NULL, buf.length, PROT_READ | PROT_WRITE, MAP_SHARED, camera.fd, buf.m.offset);
         
         	if (-1 == xioctl (camera.fd, VIDIOC_QBUF, &buf))
            		errno_exit ("VIDIOC_QBUF");
      	} 

	//Change controls
	//change_control();
}


