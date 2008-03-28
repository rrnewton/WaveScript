



// This records audio from the mote and transmits it over the serial.

// It would work better if we compressed on the mote.  At the very
// least we could shave the four unused bits off each sample.

// I'm using a baud rate of 230400.

include "stdlib.ws";
//include "unix.ws";

using TOS;
namespace Node {
  //bufsize = 1000; 
  //rate = 8000.0;
  bufsize = 10; 
  rate = 16.0;
  src1 = read_telos_audio(bufsize, rate);
}

writer = iterate arr in Node:src1 {
  emit ();
}

main = writer;
