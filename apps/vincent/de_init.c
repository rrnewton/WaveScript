#include "capture.h"

void de_init(){

	enum v4l2_buf_type type;
      	unsigned int i;
   
      	camera.capturing=FALSE;
     
	//stopping the capture
      	printf("Stop capturing\n\n");
      	type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
      	if (xioctl (camera.fd, VIDIOC_STREAMOFF, &type) == -1)
		errno_exit("VIDIOC_STREAMOFF");
   

   	//reseting buffer
      	printf("Reseting the buffers\n\n");
      	for (i = 0; i < camera.i_buffers; i++) {
         	struct v4l2_buffer buf;
         
         	CLEAR(buf);
         	buf.type	= V4L2_BUF_TYPE_VIDEO_CAPTURE;
         	buf.memory	= V4L2_MEMORY_MMAP;
         	buf.index	= i;
         	if (xioctl (camera.fd, VIDIOC_QUERYBUF, &buf) == -1)
         		errno_exit("VIDIOC_QUERYBUF");
         	munmap (camera.buffers[i].start, camera.buffers[i].length);
      	}
      	printf("Closing the device\n\n");
      	close(camera.fd);
      	exit(1);
}

