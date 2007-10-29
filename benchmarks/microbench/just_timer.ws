

// On faith: scheme = <1s / 50K * 50, 
//   mlton = 3s ?? And that's whether or not we use 32 or 64 bit virttime stamps.
//   wsc -O2 = 15.5s 9.3s (user) with no usleeping at all, 
//   with the "setBatchSize" method, it was 29.2s real, 12.1s. user.
printevery = 50 * 1000;

// We don't want to print too much output, only produce an output tuple every 10,000.
BASE <- iterate _ in timer(10000.0) {
  state { count :: Int = 0 }
  count += 1;
  if count == printevery then {
    count := 0;
    emit ();
  }
}
