#include "process.h"


//-----------------------------------------------
//---------------------read_frame---------------
//----------------------------------------------
//To read a frame during the capture
 
int read_frame(int file_number){
   
    struct v4l2_buffer buf;
    FILE *f;
   
    CLEAR (buf);
   
	buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    buf.memory = V4L2_MEMORY_MMAP;
   
    if (-1 == xioctl (fd, VIDIOC_DQBUF, &buf)) 
       switch (errno) {
          case EAGAIN:
             return 0;
         
          case EIO:
                     
          default:
             errno_exit ("VIDIOC_DQBUF");
       }
      
   
    assert (buf.index < n_buffers);
    

  //count the framerate
    timestamp = time(NULL);
    t = gmtime(&timestamp);
      
    if (t->tm_sec!=sec){
       sec=t->tm_sec;
       printf("Fps :            %d,   image : %d\n", frame, file_number); 
       frame=0;
    }
    if(t->tm_sec==sec)
       frame++;        

 
    //write the header 
    if(palette==1) {
	void *data=(void *)malloc(sizeof(void)*size_data_rgb24);
        memcpy(data,buffers[buf.index].start,size_data_rgb24);
	sprintf(data+size_data_rgb24+1,"%d", file_number);
	sprintf(data+size_data_rgb24+2,"%d", sec);
	wsentry2(data);
    } else if(palette==2){
	void *data=(void *)malloc(sizeof(void)*size_data_yuv420);
        memcpy(data,buffers[buf.index].start,size_data_yuv420);
	sprintf(data+size_data_yuv420+1,"%d", file_number);
	sprintf(data+size_data_yuv420+2,"%d", sec);
	wsentry2(data);
    } else if(palette==3){
 	void *data=(void *)malloc(sizeof(void)*size_data_yuyv);
        memcpy(data,buffers[buf.index].start,size_data_yuyv);
	sprintf(data+size_data_yuyv+1,"%d", file_number);
	sprintf(data+size_data_yuyv+2,"%d", sec);
	wsentry2(data);
   }
      
    if (-1 == xioctl (fd, VIDIOC_QBUF, &buf))
       errno_exit ("VIDIOC_QBUF");
   
   
    return 1;
} 
  
  
  
  
  
//-----------------------------------------------
//-----------------------mainloop----------------
//-----------------------------------------------
//To do during the capture

void mainloop (){
    
    unsigned int count; 
    sec=0;
    frame=0; 
    count = 0;
   	
	while (capturing==1) {
       	for (;;) {
          fd_set fds;
          struct timeval tv;
          int r;
        
          FD_ZERO (&fds);
          FD_SET (fd, &fds);
         
          /* Timeout. */
          tv.tv_sec = 2;
          tv.tv_usec = 0;
            
          r = select (fd + 1, &fds, NULL, NULL, &tv);
         
          if (-1 == r) {
             if (EINTR == errno)
                continue;
           
             errno_exit ("select");
          }
         
          if (0 == r) {
             errno_exit ("select");
          }
          if (read_frame(count))
             break;
       	/* EAGAIN - continue select loop. */
       	}
       	//To have around 30fps
       	if(framerate==60)
          usleep(30200);
         
       	count++;
       	//break;		// one time
	}
}

//----------------------------------------------------------
//------------------------start_capturing-------------------	
//----------------------------------------------------------
//To start the capture
	
void start_capturing(){	
   
	enum v4l2_buf_type type;
   	
      	type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
   
      	if (-1 == xioctl (fd, VIDIOC_STREAMON, &type))
        	 errno_exit ("VIDIOC_STREAMON");
            
      	capturing=1;
}



//----------------------------------------------------	
//------------------stop_capturing--------------------
//----------------------------------------------------
//To stop the capture and reset the buffers

void stop_capturing(){
   
	enum v4l2_buf_type type;
        unsigned int i;
   
        capturing=0;
     
        printf("Stop capturing ...\n\n");
        type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
        ioctl (fd, VIDIOC_STREAMOFF, &type);
   
        //reseting buffer
        printf("Reseting the buffers\n\n");
        for (i = 0; i < n_buffers; i++) {
        	struct v4l2_buffer buf;
         
        	CLEAR(buf);
        	buf.type	= V4L2_BUF_TYPE_VIDEO_CAPTURE;
        	buf.memory= V4L2_MEMORY_MMAP;
        	buf.index	= i;
        	if (xioctl (fd, VIDIOC_QUERYBUF, &buf) < 0) {
        		errno_exit ("VIDIOC_QUERYBUF");
            		exit(-1);
        	}
        	munmap (buffers[i].start, buffers[i].length);
        }
        printf("Closing the device ...\n");
        close(fd);
        exit(1);
}
