#include "process.h"



//------------------------------------------------------	
//--------------------change_control--------------------
//------------------------------------------------------
//To change a control thanks to an option in the conf file
	
    void change_control(){
   
      struct v4l2_queryctrl queryctrl;
      struct v4l2_control control;
      
      printf("Setting the parameters:\n");
   	//brightness
      printf("Brightness : %d\n", brightness);
      CLEAR(queryctrl);
      queryctrl.id = V4L2_CID_BRIGHTNESS;
      
      if (-1 == xioctl (fd, VIDIOC_QUERYCTRL, &queryctrl)) {
         if (errno != EINVAL) 
            errno_exit ("VIDIOC_QUERYCTR");      
         else 
            printf ("V4L2_CID_BRIGHTNESS is not supported\n");
      } 
      else if (queryctrl.flags & V4L2_CTRL_FLAG_DISABLED) {
         printf ("V4L2_CID_BRIGHTNESS is not supported\n");
      } 
      else {
         CLEAR(control);
         control.id = V4L2_CID_BRIGHTNESS;
         control.value = brightness;
         if (-1 == xioctl (fd, VIDIOC_S_CTRL, &control)) {
            errno_exit ("VIDIOC_S_CTRL");
         }
      }
   
   
      
   	//saturation
      printf("Saturation : %d\n", saturation);
      CLEAR(queryctrl);
      queryctrl.id = V4L2_CID_SATURATION;
      
      if (-1 == xioctl (fd, VIDIOC_QUERYCTRL, &queryctrl)) {
         if (errno != EINVAL) 
            errno_exit ("VIDIOC_QUERYCTR");      
         else 
            printf ("V4L2_CID_SATURATION is not supported\n");
      } 
      else if (queryctrl.flags & V4L2_CTRL_FLAG_DISABLED) {
         printf ("V4L2_CID_SATURATION is not supported\n");
      } 
      else {
         CLEAR(control);
         control.id = V4L2_CID_SATURATION;
         control.value = saturation;
         if (-1 == xioctl (fd, VIDIOC_S_CTRL, &control)) {
            errno_exit ("VIDIOC_S_CTRL");
         }
      }
      
   	
      
      //gain
      if(palette!=2){
         printf("Gain : %d\n", gain);
         CLEAR(queryctrl);
         queryctrl.id = V4L2_CID_GAIN;
         
         if (-1 == xioctl (fd, VIDIOC_QUERYCTRL, &queryctrl)) {
            if (errno != EINVAL) 
               errno_exit ("VIDIOC_QUERYCTR");      
            else 
               printf ("V4L2_CID_GAIN is not supported\n");
         } 
         else if (queryctrl.flags & V4L2_CTRL_FLAG_DISABLED) {
            printf ("V4L2_CID_GAIN is not supported\n");
         } 
         else {
            CLEAR(control);
            control.id = V4L2_CID_GAIN;
            control.value = gain;
            if (-1 == xioctl (fd, VIDIOC_S_CTRL, &control)) {
               errno_exit ("VIDIOC_S_CTRL");
            }
         }
      }
   
      
   	//exposure
      printf("Exposure : %d\n\n", exposure);
      CLEAR(queryctrl);
      queryctrl.id = V4L2_CID_EXPOSURE;
        // printf("aa : %d", queryctrl.default_value);
      if (-1 == xioctl (fd, VIDIOC_QUERYCTRL, &queryctrl)) {
         if (errno != EINVAL) 
            errno_exit ("VIDIOC_QUERYCTR");      
         else 
            printf ("V4L2_CID_EXPOSURE is not supported\n");
      } 
      else if (queryctrl.flags & V4L2_CTRL_FLAG_DISABLED) {
         printf ("V4L2_CID_EXPOSURE is not supported\n");
      } 
      else {
         CLEAR(control);
         control.id = V4L2_CID_EXPOSURE;
         control.value = exposure;
         if (-1 == xioctl (fd, VIDIOC_S_CTRL, &control)) {
            errno_exit ("VIDIOC_S_CTRL");
         }
      }
   
   }
