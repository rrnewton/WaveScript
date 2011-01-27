

// [2008.07.31] 
// This similar to just_timer.ws, except it uses the BUFSIZE variable
// and mimics the other pass_* microbenchmarks.

// This can do at least 200 million numbers timer ticks a second on honor.

// One problem with these passes is that printing and flushing at the
// end are too costly.  Simply removing the flushing speeds it up 30%.

// This simple benchmark should do around 4 comparison/branches, 4
// adds, and an AND on each element.  

// Then removing the print speeds it up 2X.  Reaching 700 million
// numbers a second.  One every four cycles.  That is 2.8 gb of data a second.

bufsize = if GETENV("BUFSIZE") == "" then 100 else stringToInt(GETENV("BUFSIZE"));

s1 = iterate _ in timer(1000) {
  state { count = 0; }
  count += 1;
  if count == bufsize then count := 0;
  emit count; 
}

main = iterate x in s1 { 
  state { counter = 0;
          sum = 0;   }
  counter += 1;
  sum += x;
  if counter == bufsize then {    
    tmp = sum;
    sum := 0;
    counter := 0;
    emit tmp;
  }
}
