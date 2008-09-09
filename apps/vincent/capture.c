#include "capture.h"

#include "init.c"
#include "image_capture.c"
#include "de_init.c"

//In case of error
void wserror(const char *p){
	printf("WSERROR: %s\n", p);
}


//To have the value in the C file
void wssink(int *value){
	camera.fd=value[0];
	camera.param.width=value[1];
	camera.param.height=value[2];
        camera.param.framerate=value[3];
        camera.param.brightness=value[4];
        camera.param.contrast=value[5];
        camera.param.saturation=value[6];
        camera.param.exposure=value[7];
        camera.param.white_balance=value[8];
}

//Main function
void wsmain(int argc, char *argv[]){
	int* value = (int *)calloc(9,sizeof(int));

	camera.name="/dev/video0";
	camera.capturing=0;

	signal(SIGINT, de_init);

	//printf("%d, %d, %d\n", camera.fd, camera.param.width, camera.param.height);
	value[0]=camera.fd;
    	value[1]=camera.param.width;
    	value[2]=camera.param.height;
	value[3]=camera.param.framerate;
	value[4]=camera.param.brightness;
	value[5]=camera.param.contrast;
	value[6]=camera.param.saturation;
	value[7]=camera.param.exposure;
	value[8]=camera.param.white_balance;
	wsentry1(value);
	//printf("%d, %d, %d\n", camera.fd, camera.param.width, camera.param.height);
	value[0]=camera.fd;
    	value[1]=camera.param.width;
    	value[2]=camera.param.height;
	value[3]=camera.param.framerate;
	value[4]=camera.param.brightness;
	value[5]=camera.param.contrast;
	value[6]=camera.param.saturation;
	value[7]=camera.param.exposure;
	value[8]=camera.param.white_balance;
	wsentry1(value);

	//Initialize the device
	init();	
	
	//Capture the images from the device
	image_capture();
}

//Init
void wsinit(int argc, char *argv[]){
	camera.fd=0;
	camera.param.width=0;
	camera.param.height=0;
	camera.param.framerate=0;
	camera.param.brightness=0;
	camera.param.contrast=0;
	camera.param.saturation=0;
	camera.param.exposure=0;
	camera.param.white_balance=0;
}


//Function for errors
void errno_exit (const char *s) {
      	fprintf (stderr, "%s error %d, %s\n",s, errno, strerror (errno));
      	exit (EXIT_FAILURE);
}


//Function in place of iotcl
int xioctl (int fd, int request, void *arg) {
      	int r;
   
      	do r = ioctl (fd, request, arg);
      	while (-1 == r && EINTR == errno);
   
      	return r;
}
