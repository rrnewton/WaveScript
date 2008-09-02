include "load_option.ws"
include "unix.ws"
include "stdlib.ws"


open :: (String, Int) -> Int = foreign("open",["fcntl.h"])
close :: (Int) -> () = foreign("close",["fcntl.h"])

//Call foreign source
wsentry1 :: Stream Array Int = foreign_source("wsentry1", ["process.h"])
wsentry2 :: Stream Pointer "void*" = foreign_source("wsentry2", ["process.h"])

//Function to send data to the C program
rcvr :: Array Int -> () = foreign("wssink", ["process.c"])



//Function to open the device
//--------------------------------------------
fun open_() {
	option = load_option();
	width=stringToInt(List:ref(option,1));
	height=stringToInt(List:ref(option,3));
	//print("Option: "++option++"\n");
	device_name :: String ="/dev/video0";
	print("Opening the device\n");
	O_RDWR :: Int = 2;
	result = open(device_name, O_RDWR);
	if (result == -1) then wserror("Could not open device "++ device_name);
	value = #[result, width, height];
	value;
}


strm_process1 = iterate value in wsentry1 {
	emit value;
}


strm_process2 = iterate data in wsentry2 {
	emit data;
}


strm_send_data = iterate value in strm_process1 {
	value = open_();
	rcvr(value);
	emit() 
}

strm_write = iterate data in strm_process2 {
	print("et la on ecrit le fichier");
	emit ();
}


main = strm_send_data;
