#include "capture.h"


void image_capture() {

	enum v4l2_buf_type type;
	struct v4l2_buffer buf;
	fd_set fds;
        struct timeval tv, current;
        int r;
	time_t timestamp;
   	struct tm * t;
   	
	//Start capturing
	printf("Start capturing\n\n");

      	type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
      	if (-1 == xioctl (camera.fd, VIDIOC_STREAMON, &type))
         	errno_exit ("VIDIOC_STREAMON");

      	camera.capturing=TRUE;

	//Initialization of current image
	camera.image.number=0;
	camera.image.sec=0;
	camera.image.frame=0;
	camera.image.data=(unsigned char*)malloc(sizeof(unsigned char)*camera.data_size);

	//To get the actual time
	gettimeofday(&current, NULL);

	while(camera.capturing) {
		for (;;) {
			FD_ZERO(&fds);
        		FD_SET(camera.fd, &fds);

        		tv.tv_sec = 2;
        		tv.tv_usec = 0;

			r = select (camera.fd + 1, &fds, NULL, NULL, &tv);
         	
            		if (-1 == r) {
               			if (EINTR == errno)
                  			continue;
            
               			errno_exit ("select");
           		}
         
            		if (0 == r)
               			errno_exit ("select");

			CLEAR (buf);
   
      			buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
      			buf.memory = V4L2_MEMORY_MMAP;
   
      			if (-1 == xioctl (camera.fd, VIDIOC_DQBUF, &buf)) 
         			if(errno==EAGAIN){}
				else errno_exit("VIDIOC_DQBUF");
			else {
				assert(buf.index < camera.i_buffers);
			
				//if the image corresponds to a previous capture (stored in the buffer of the camera)
				if(buf.timestamp.tv_sec<current.tv_sec){
					if (-1 == xioctl (camera.fd, VIDIOC_QBUF, &buf))
         					errno_exit ("VIDIOC_QBUF");
					break;
				}
			


				//count the framerate
      				timestamp = time(NULL);
      				t = gmtime(&timestamp);
      
      				if (t->tm_sec!=camera.image.sec){
         				camera.image.sec=t->tm_sec;
         				printf("---- Fps : %d, Image : %d\n", camera.image.frame, camera.image.number); 
         				camera.image.frame=0;
      				}
      				if(t->tm_sec==camera.image.sec)
         				camera.image.frame++;        

				//Save the data
				wsentry2(camera.buffers[buf.index].start);
				
				if (-1 == xioctl (camera.fd, VIDIOC_QBUF, &buf))
         				errno_exit ("VIDIOC_QBUF");
				camera.image.number++;
				break;

				
			}
		}
	}
}


