
// This demo allocates an array and then passes it repeatedly. 


printevery = 20 * 1000;

source = iterate _ in timer(10.0) {
  state {  arr = Array:make(1000,0) }
  arr[500] := 39;
  emit arr;
}
// We don't want to print too much output, only produce an output every once in a while
BASE <- iterate arr in source {
  state { count :: Int = 0 }
  count += 1;
  if count == printevery then {
    count := 0;
    emit arr[500]; 
  }
}
