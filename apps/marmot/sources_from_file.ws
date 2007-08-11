
include "stdlib.ws";

// ================================================================================


//marmotfile = "/archive/4/marmots/brief.raw";
//marmotfile = "/archive/4/marmots/real_100.raw";
marmotfile =
  if FILE_EXISTS("15min_marmot_sample.raw") then "15min_marmot_sample.raw" else
  if FILE_EXISTS("3min_marmot_sample.raw") then "3min_marmot_sample.raw" else
  if FILE_EXISTS("6sec_marmot_sample.raw") then "6sec_marmot_sample.raw" else
  //  if FILE_EXISTS("~/archive/4/marmots/brief.raw") then "~/archive/4/marmots/brief.raw" else
  wserror("Couldn't find sample marmot data, run the download scripts to get some.\n");

winsize = 16384;
driver = timer(24000.0 / winsize`i2f);
chans = (readFile(marmotfile, "mode: binary window: "++winsize, driver) :: Stream Sigseg (Int16));

fun onechan(offset)
  iterate w in chans {
    size = w`width / 4;
    assert_eq("source stream multiple of 4", w`width, size * 4);
    arr = Array:build(size, fun (i) (w[[(i*4) + offset]]));
    emit toSigseg(arr, w`start / 4`intToInt64 , w`timebase)
  }

// When we're not live we just print log messages to the stream.
fun log(l,s) println(s)

// Testing, trying this instead:
ch1i = onechan(0); 
ch2i = onechan(1); 
ch3i = onechan(2); 
ch4i = onechan(3);
