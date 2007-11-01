

// On faith: scheme = <1s / 50K * 50, (-n 50)
//   mlton = 3s ?? And that's whether or not we use 32 or 64 bit virttime stamps.
//   wsc -O2 = 15.5s 9.3s (user) with no usleeping at all, 
//   with the "setBatchSize" method, it was 29.2s real, 12.1s. user.
printevery = 20 * 1000 * 1000;
//printevery = 1000;
//printevery = 1;


fun amplify(n,s)
  iterate x in s {
    for i = 1 to n {
      emit x;
    }
  }

// We put an amplifier on it, so we don't have to run the actual timer source at a high rate.
mytimer = amplify(1700, amplify(2000, timer(10.0)))
//mytimer = amplify(1700 * 2000, timer(10.0))

// We don't want to print too much output, only produce an output tuple every 10,000.
//BASE <- iterate _ in timer(1000.0) {
BASE <- iterate _ in mytimer {
  state { count :: Int = 0 }
  count += 1;
  if count == printevery then {
    count := 0;
    emit 1; // Added some output... otherwise I think MLton may have optimized the whole thing away.
  }
}
