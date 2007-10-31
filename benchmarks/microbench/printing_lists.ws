


ls = List:build(100, fun(i) i);

BASE <- iterate _ in timer(1.0) {
  print("List: "++ ls ++ "\n");
  emit ();
}
