include "stdlib.ws";

// 0 says not to replay the data-file after it's finished:
// 1000 says to set sample rate at 1Khz -- not important for this app:
data = (dataFile("data2.txt", "text", 1000, 0) :: Stream (Float));

// Note, this error check doesn't work right because of the way static
// elaboration currently operates:
//winsize = GETENV("windowsize");
//wins = if winsize == "" 
//       then wserror("Environment variable 'windowsize' must be set!")
//       else window(data, stringToInt(winsize));

wins = window(data, stringToInt(GETENV("windowsize")));

// BASE <- snoop("hello" , wins);


avgs = iterate (w in wins) {
  // This is a purely functional way to iterate over the array:
  sum = Array:fold(fun (acc, n) acc + n,
		   0.0,
		   w.toArray);
  emit sum / intToFloat(w.width);
};

BASE <- avgs;
