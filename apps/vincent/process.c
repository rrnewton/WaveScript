#include "process.h"
#include "capture.c"
#include "init.c"
#include "change_control.c"



//Function for error
void errno_exit (const char *s){
	fprintf (stderr, "%s error %d, %s\n",s, errno, strerror (errno));
	close(fd);
	exit (EXIT_FAILURE);
}


//Function in place of iotcl
int xioctl (int fd, int request, void *arg){
	int r;
   
	do r = ioctl (fd, request, arg);
	while (-1 == r && EINTR == errno);
   
	return r;
}





//In case of error
void wserror(const char *p){
	printf("WSERROR: %s\n", p);
}


//To have the value in the C file
void wssink(int *value){
	fd=value[0];
	width=value[1];
	height=value[2];
}

//Main function
void wsmain(int argc, char *argv[]){
	int* value = (int *)calloc(3,sizeof(int));
	int i = 0;
	signal(SIGINT, stop_capturing);
	printf("%d, %d, %d\n", fd, width, height);
	value[0]=fd;
    	value[1]=width;
    	value[2]=height;
	wsentry1(value);
	printf("%d, %d, %d\n", fd, width, height);
	value[0]=fd;
    	value[1]=width;
	value[2]=height;
	wsentry1(value);
	//init the device
	init();
	for(i=0; i<argc; i++)
        if(strcasecmp(argv[i],"-wc")==0)
            change_control();       
	printf("Start Capturing\n");
	start_capturing();
	mainloop();
}

//Init
void wsinit(int argc, char *argv[]){
	fd=0;
	width=0;
	height=0;
}
