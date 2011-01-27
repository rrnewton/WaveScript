
// There should only be two bytes overhead for the length field... but
// I'm having problems.  And when it doesn't work I don't see any
// packets using seriallisten

// 10 / 28 works
// 12 / 28 works
// 14 / 28 works

// Oddly I get nothing if I try to pass more than 18/28 bytes in the payload.  What's going on?

//BYTES= 28  OVERHEAD = 14
BYTES    = if GETENV("BYTES"   )=="" then 28 else stringToInt(GETENV("BYTES"))
OVERHEAD = if GETENV("OVERHEAD")=="" then 14 else stringToInt(GETENV("OVERHEAD"))

//_ = SHELL("echo "++ BYTES + ?? ++ " > TOSH_DATA_LENGTH");
//_ = SHELL("echo "++ 28 ++ " > TOSH_DATA_LENGTH");

elements = (BYTES - OVERHEAD)/2

using Array;
arr = build(elements, fun(i ) Uint16!(i+1))

using TOS;
Node:arrs = iterate _ in timer(1) {
  if      arr.length == elements
  then led0Toggle()
  else if arr.length > elements
  then led1Toggle()
  else led2Toggle();

  //arr[0] := getTreeParent();
  // if getID() != 1 then
  arr[0] := Uint16! getID();
  emit arr;
}


main = Node:arrs;
