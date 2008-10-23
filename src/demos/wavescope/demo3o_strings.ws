
// Test string API

include "common.ws"

main = iterate _ in timer(30) {
  using String;
  str = "abcdef";
  assert_eq_prnt("length", 6, str.length);

  //ls = explode(str);
  ls = toList(str);
  println(ls);
  //println$ "Exploded: "++ls;
  assert_eq_prnt("exploded length", 6, str.length);

  //ls = ['a', 'b', 'c'];

  str2 = implode(ls);
  println$ "Imploded: "++str2;
  
  assert_eq_prnt("implode round trip", str, str2);

  arr = toArray(str);
  println(arr);
  assert_eq_prnt("toArray length", 6, arr.Array:length);

  str3 = fromArray(arr);
  assert_eq_prnt("fromArray round trip", str, str3);

  emit ()  
}
