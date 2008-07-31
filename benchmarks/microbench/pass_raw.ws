

// [2008.07.31] 
// This similar to just_timer.ws, except it uses the BUFSIZE variable
// and mimics the other pass_* microbenchmarks.

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
