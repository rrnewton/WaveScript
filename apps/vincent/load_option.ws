include "unix.ws"
include "stdlib.ws"


//----------------------------------------------------------
//-- Function to load default value when there is no file --
//----------------------------------------------------------
fun default_value() {
	
	out=["width","640"];
	toadd=["height","480"];
	out := List:append(out,toadd);
	toadd:=["framerate","30"];
	out := List:append(out,toadd);
	toadd:=["brightness","80"];
	out := List:append(out,toadd);
	toadd:=["contrast","100"];
	out := List:append(out,toadd);
	toadd:=["saturation","0"];
	out := List:append(out,toadd);
	toadd:=["hue","0"];
	out := List:append(out,toadd);
	toadd:=["exposure","255"];
	out := List:append(out,toadd);
	toadd:=["white_balance","0"];
	out := List:append(out,toadd);
	toadd:=["gain","130"];
	out := List:append(out,toadd);
	toadd:=["gamma","0"];
	out := List:append(out,toadd);
	out;
}
//----------------------------------------------------
//-- Function to load informations from config file --
//----------------------------------------------------
fun load_option() {
	
	using Unix;
	print("Loading informations from camera.conf\n");
	
	//Open the file
	conf=fopen("camera.conf","r");
	out = [];
	if ptrIsNull(conf) then {
		//there is no file
		out := default_value();
		print("No file, load default value\n"++out);
		wserror("No File");
	} else {
		buf :: String = String:fromArray(Array:make(128, '_'));
		result=fgets(buf,128,conf);
		//while the line in not NULL
		while not(ptrIsNull(result)) {
			line=Array:toList(ptrToArray(result, 128));
			first=List:ref(line,0);	
			//if the line begins with # or if empty, we skip the line
			if first=='#' || first==intToChar(10) then {
			} else {	
				i = 0;		
				//Find the first word
				
				while not(List:ref(line,i)==' ') {						
					i += 1;
				};
				//print("Length: " ++ i ++ "\n"); 
				word = String:implode(Array:toList(ptrToArray(result, i)));
				//print("Option: " ++ word ++ "\n");
				j = 0;
				//jump to the value in case there is several ' '
				while List:ref(line,i+j)==' ' {
					j += 1;
				};
				k = 0;
				value = [];
				//jump to the end of the line or a space ' '
				while not(List:ref(line,i+j+k)==intToChar(10)||List:ref(line,i+j+k)==' ') {						
					val = List:ref(line,i+j+k);
					k += 1;
					value := val:::value;
				};
				//Create the string value
				value:=List:reverse(value);
				valstr=String:implode(value);
				//Save it in the out list
				toadd = [word,valstr];
				out := List:append(out,toadd);
			};
			//get new line
			result := fgets(buf,128,conf);
		};

		//Close the file
		fclose(conf);
	};
	out;
}
 



//-------------------
//-- Function main --
//-------------------
main = iterate _ in timer(3) {	
	out = load_option();
	print("out: "++out);
	emit ();
}
