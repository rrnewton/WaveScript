//Function to do all the work with the v4l2 interface

include "stdlib.ws"
include "unix.ws"

//Call foreign source
wsentry1 :: Stream Array Int = foreign_source("wsentry1", ["process.c"])

//Function to send data to the C program
rcvr :: Array Int -> () = foreign("wssink", ["process.c"])

fd_ = 3;
width_ = 640;
height_ = 480;

strm_process = iterate value in wsentry1 {
	fd = fd_ ;
	width = width_;
	height = height_;
	value = #[fd, width, height];
	emit value;
}

strm_send_data = iterate value in strm_process {
	rcvr(value);
	emit() 
}

main = strm_send_data;


