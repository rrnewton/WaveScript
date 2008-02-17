


source_count = Mutable:ref(0);

fun tos_timer(rate) {
  n = source_count;
  source_count += 1;
  funname = "timer_ws_entry"++n;
  s1 = (foreign_source(funname, []) :: Stream ());
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

// Shoud use a union type for this "enum":
/* led_toggle = { */
/*   fun (type) { */
/*     if type == "RED"   then led0Toggle() else */
/*     if type == "GREEN" then led1Toggle() else */
/*     if type == "BLUE"  then led2Toggle() else */
/*     wserror("Invalid LED specifier to led_toggle: "++type); */
/*   } */
/* } */

fun sensor_stream(which, rate) {
    //new VoltageC() as Sensor, 
  if which == "LIGHT"
  then 0
  else 0

}

// Sets up a timer which drives a sensor.  Returns a stream of results.
fun sensor_uint16(name, rate) {
  n = source_count;
  source_count += 1;
  funname = "sensor_ws_entry"++n;
  s1 = (foreign_source(funname, []) :: Stream Int16);
  mod = "SensorStrm"++n;
  conf2 = "components new "++name++"() as "++mod++";\n"++
          "WSQuery."++mod++" -> "++mod++";\n" ++
	  "components new TimerMilliC() as Timer"++n++";\n"++
          "WSQuery.Timer"++n++" -> Timer"++n++";\n";
  mod1  = "uses interface Read<uint16_t> as "++mod++";\n" ++
          "uses interface Timer<TMilli> as Timer"++n++";\n";
  boot  = "call Timer"++n++".startPeriodic( "++(1000.0 / rate)++" );\n";
  mod2  = "
event void Timer"++n++".fired() { call "++mod++".read(); }
event void "++mod++".readDone(error_t result, uint16_t data) { 
    if (result != SUCCESS)
      {
	data = 0xffff;
	//wserror(\"ReadDone failure\");
      }
   "++funname++"(data);
  }
";
  s2 = inline_TOS("", "", conf2, mod1, mod2, boot);
  merge(s1,s2);
}






// Alias the default timer primitive:
//timer = tos_timer;
