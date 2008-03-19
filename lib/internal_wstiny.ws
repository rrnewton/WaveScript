


source_count = Mutable:ref(0);

namespace TOS {

fun timer(rate) {
  n = source_count;
  source_count += 1;
  funname = "timer_ws_entry"++n;
  // There's a hack for foreign_source in wstiny: the first "filename" stores the rate:
  s1 = (foreign_source(funname, [show(rate)]) :: Stream ());
  top = "";  
  conf1 = "";  
  conf2 = "components new TimerMilliC() as Timer"++n++";\n"++
          "WSQuery.Timer"++n++" -> Timer"++n++";\n";
  mod1  = "uses interface Timer<TMilli> as Timer"++n++";\n";
  boot  = "call Timer"++n++".startPeriodic( "++(1000.0 / rate)++" );\n";
  mod2  = "event void Timer"++n++".fired() { "++funname++"(0); }\n";
  s2 = inline_TOS(top, conf1, conf2, mod1, mod2, boot, "");
  merge(s1,s2);
}


led0Toggle = (foreign("call Leds.led0Toggle", []) :: () -> ());
led1Toggle = (foreign("call Leds.led1Toggle", []) :: () -> ());
led2Toggle = (foreign("call Leds.led2Toggle", []) :: () -> ());

// This includes a Telos-Specific Counter component.
// This is an unpleasant programming style:
load_telos32khzCounter = {
  conf2 = " 
  components new Msp430CounterC(TMilli) as Cntr;
  components Msp430TimerC;
  Cntr.Msp430Timer -> Msp430TimerC.TimerB;  
  WSQuery.Cntr -> Cntr.Counter;\n"; 
  mod1 = "  uses interface Counter<TMilli,uint16_t> as Cntr;\n";
  mod2 = "
  //uint32_t counter_overflows_32khz = 0;
  async event void Cntr.overflow() { /* counter_overflows_32khz++; */ }\n";
  inline_TOS("", "", conf2, mod1, mod2, "", "");
}

clock32khz = (foreign("call Cntr.get", []) :: () -> Uint16);

// Shoud use a union type for this "enum":
/* led_toggle = { */
/*   fun (type) { */
/*     if type == "RED"   then led0Toggle() else */
/*     if type == "GREEN" then led1Toggle() else */
/*     if type == "BLUE"  then led2Toggle() else */
/*     wserror("Invalid LED specifier to led_toggle: "++type); */
/*   } */
/* } */

/* fun sensor_stream(which, rate) { */
/*     //new VoltageC() as Sensor,  */
/*   if which == "LIGHT" */
/*   then 0 */
/*   else 0 */
/* } */

// Sets up a timer which drives a sensor.  Returns a stream of results.
fun sensor_uint16(name, rate) {
  n = source_count;
  source_count += 1;
  funname = "sensor_ws_entry"++n;
  s1 = (foreign_source(funname, [show(rate)]) :: Stream Uint16);
  smod = "SensorStrm"++n;
  tmod = "SensorTimer"++n;
  conf2 = "components new "++name++"() as "++smod++";\n"++
          "WSQuery."++smod++" -> "++smod++";\n" ++
	  "components new TimerMilliC() as "++tmod++";\n"++
          "WSQuery."++tmod++" -> "++tmod++";\n";
  mod1  = "uses interface Read<uint16_t> as "++smod++";\n" ++
          "uses interface Timer<TMilli> as "++tmod++";\n";
  boot  = "call "++tmod++".startPeriodic( "++(1000.0 / rate)++" );\n";
  mod2  = "
event void "++tmod++".fired() { call "++smod++".read(); }
event void "++smod++".readDone(error_t result, uint16_t data) { 
    if (result != SUCCESS) 
      wserror(\"sensor_uint16 read failure\");    
    else "++funname++"(data);
  }
";
  s2 = inline_TOS("", "", conf2, mod1, mod2, boot, "");
  merge(s1,s2);
}

// This uses the ReadStream instead of Read:
// The is the per-sample rate, not the per-buffer rate.
fun readstream_uint16(name, bufsize, rate) {
  arbitraryStupidLimit = 16; // Thanks Tinyos 2.0...
  if rate < arbitraryStupidLimit then 
    wserror("readstream_uint16: cannot handle rates less than "++
            arbitraryStupidLimit++" hz: "++rate);
  n = source_count;
  ty = "uint16_t";
  source_count += 1;
  adjusted = rate / bufsize.gint;
  funname = "readstream_ws_entry"++n;
  s1 = (foreign_source(funname, [show(adjusted)]) :: Stream (Array Uint16));
  smod = "SensorStrm"++n;
  conf2 = "components new "++name++"() as "++smod++";\n"++
          "WSQuery."++smod++" -> "++smod++".ReadStream;\n";
  mod1  = "uses interface ReadStream<"++ty++"> as "++smod++";\n";
  boot  =
    "call "++smod++".postBuffer(buf1 + 1, "++bufsize++");\n" ++
    "call "++smod++".postBuffer(buf2 + 1, "++bufsize++");\n" ++
    //"call "++smod++".read( "++(1000000.0 / adjusted)++" );\n";
    "call "++smod++".read( "++ floatToInt(1000000.0 / rate)++" );\n";
  mod2  = "
    "++ty++" buf1["++ bufsize+1 ++"];
    "++ty++" buf2["++ bufsize+1 ++"];
    "++ty++"* curbuf = 0;

  event void "++smod++".bufferDone(error_t result, "++ty++"* buf, uint16_t cnt) {
    if (result != SUCCESS)
       wserror(\"readstream_uint16 failure\");
    else {
      uint16_t i;
      buf[-1] = cnt;
      "++funname++"(buf);
      // This is the inefficient way of doing things:
      // But it won't work with the current task-based methodology:
      //for (i=0; i<"++bufsize++"; i++) "++funname++"(buf[i]);
    }
    // We need to repost the buffer at the *end* of the processing chain.
    // This reposts it IMMEDIATELY (hack) and assumes that it won't be filled 
    // until readstream finishes filling the other buffer:
    //call "++smod++".postBuffer(buf, cnt);
    curbuf = buf;
  }

  event void "++smod++".readDone(error_t result, uint32_t period) {
    if (result != SUCCESS) wserror(\"ReadStream.readDone completed incorrectly\");
  }

";
  endtraverse = "call "++smod++".postBuffer(curbuf, "++bufsize++");\n";
  s2 = inline_TOS("", "", conf2, mod1, mod2, boot, endtraverse);
  merge(s1,s2);
}





}  // End namespace

// Alias the default timer primitive:
//timer = tos_timer;
Node:timer = TOS:timer;
