#include "capture.h"
#include "change_control.h"


void change_control(){

        struct v4l2_queryctrl queryctrl;
        struct v4l2_control control;
      
      	printf("Setting the parameters:\n");
   	
	      //brightness
      	printf("Brightness : %d\n", camera.param.brightness);
      	CLEAR(queryctrl);
      	queryctrl.id = V4L2_CID_BRIGHTNESS;
      
      	if (-1 == xioctl (camera.fd, VIDIOC_QUERYCTRL, &queryctrl)) {
         	if (errno != EINVAL) 
            		errno_exit ("VIDIOC_QUERYCTR");      
         	else 
            		printf ("V4L2_CID_BRIGHTNESS is not supported\n");
      	} else if (queryctrl.flags & V4L2_CTRL_FLAG_DISABLED)
         	printf ("V4L2_CID_BRIGHTNESS is not supported\n");
        else {
         	CLEAR(control);
         	control.id = V4L2_CID_BRIGHTNESS;
         	control.value = camera.param.brightness;
         	if (-1 == xioctl (camera.fd, VIDIOC_S_CTRL, &control))
            		errno_exit ("VIDIOC_S_CTRL");
      	}

        //contrast
      	printf("Contrast : %d\n", camera.param.contrast);
      	CLEAR(queryctrl);
      	queryctrl.id = V4L2_CID_CONTRAST;
      
      	if (-1 == xioctl (camera.fd, VIDIOC_QUERYCTRL, &queryctrl)) {
         	if (errno != EINVAL) 
            		errno_exit ("VIDIOC_QUERYCTR");      
         	else 
            		printf ("V4L2_CID_CONTRAST is not supported\n");
      	} else if (queryctrl.flags & V4L2_CTRL_FLAG_DISABLED)
         	printf ("V4L2_CID_CONTRAST is not supported\n");
        else {
         	CLEAR(control);
         	control.id = V4L2_CID_CONTRAST;
         	control.value = camera.param.contrast;
         	if (-1 == xioctl (camera.fd, VIDIOC_S_CTRL, &control))
            		errno_exit ("VIDIOC_S_CTRL");
      	}

        //saturation
      	printf("Saturation : %d\n", camera.param.saturation);
      	CLEAR(queryctrl);
      	queryctrl.id = V4L2_CID_SATURATION;
      
      	if (-1 == xioctl (camera.fd, VIDIOC_QUERYCTRL, &queryctrl)) {
         	if (errno != EINVAL) 
            		errno_exit ("VIDIOC_QUERYCTR");      
         	else 
            		printf ("V4L2_CID_SATURATION is not supported\n");
      	} else if (queryctrl.flags & V4L2_CTRL_FLAG_DISABLED)
         	printf ("V4L2_CID_SATURATION is not supported\n");
        else {
         	CLEAR(control);
         	control.id = V4L2_CID_SATURATION;
         	control.value = camera.param.saturation;
         	if (-1 == xioctl (camera.fd, VIDIOC_S_CTRL, &control))
            		errno_exit ("VIDIOC_S_CTRL");
      	}

        //exposure
      	printf("Exposure : %d\n", camera.param.exposure);
      	CLEAR(queryctrl);
      	queryctrl.id = V4L2_CID_EXPOSURE;
      
      	if (-1 == xioctl (camera.fd, VIDIOC_QUERYCTRL, &queryctrl)) {
         	if (errno != EINVAL) 
            		errno_exit ("VIDIOC_QUERYCTR");      
         	else 
            		printf ("V4L2_CID_EXPOSURE is not supported\n");
      	} else if (queryctrl.flags & V4L2_CTRL_FLAG_DISABLED)
         	printf ("V4L2_CID_EXPOSURE is not supported\n");
        else {
         	CLEAR(control);
         	control.id = V4L2_CID_EXPOSURE;
         	control.value = camera.param.exposure;
         	if (-1 == xioctl (camera.fd, VIDIOC_S_CTRL, &control))
            		errno_exit ("VIDIOC_S_CTRL");
      	}

        //white balance
        //Not added for the moment
}
