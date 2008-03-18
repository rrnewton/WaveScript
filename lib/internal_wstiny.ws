


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
  s2 = inline_TOS(top, conf1, conf2, mod1, mod2, boot);
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
  inline_TOS("", "", conf2, mod1, mod2, "");
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
  s2 = inline_TOS("", "", conf2, mod1, mod2, boot);
  merge(s1,s2);
}

// This uses the ReadStream instead of Read:
fun readstream_uint16(name, bufsize, rate) {
  n = source_count;
  ty = "uint16_t";
  source_count += 1;
  adjusted = rate / bufsize.gint;
  funname = "readstream_ws_entry"++n;
  s1 = (foreign_source(funname, [show(adjusted)]) :: Stream Uint16);
  smod = "SensorStrm"++n;
  conf2 = "components new "++name++"() as "++smod++";\n"++
          "WSQuery."++smod++" -> "++smod++";\n";
  mod1  = "uses interface ReadStream<"++ty++"> as "++smod++";\n";
  boot  =
    ty++" buf1["++ bufsize+1 ++"];\n" ++
    ty++" buf2["++ bufsize+1 ++"];\n" ++
    "call "++smod++".postBuffer(buf1 + 1, "++bufsize++");\n" ++
    "call "++smod++".postBuffer(buf2 + 1, "++bufsize++");\n" ++
    "call "++smod++".read( "++(1000.0 / adjusted)++" );\n";
  mod2  = "
  event void "++smod++".bufferDone(error_t result, "++ty++"* buf, uint16_t cnt);
    if (result != SUCCESS)
       wserror(\"readstream_uint16 failure\");
    else {
      buf[-1] = cnt;
      "++funname++"(buf);
    }
    // We need to repost the buffer at the *end* of the processing chain.
    call "++smod++".postBuffer(buf, cnt);
  }
";
  s2 = inline_TOS("", "", conf2, mod1, mod2, boot);
  merge(s1,s2);
}





}  // End namespace

// Alias the default timer primitive:
//timer = tos_timer;
Node:timer = TOS:timer;
