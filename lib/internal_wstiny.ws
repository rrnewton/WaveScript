

timer_count = Mutable:ref(0);

fun tos_timer(rate) {
  n = timer_count;
  timer_count += 1;
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

fun sensor_stream(which, rate) {
  if which == "LIGHT"
  then 0
  else 0
}

fun leds_toggle() {
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




// Alias the default timer primitive:
//timer = tos_timer;
