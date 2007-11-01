


ls = List:build(100, fun(i) i);


fun amplify(n,s)
  iterate x in s {
    for i = 1 to n {
      emit x;
    }
  }


BASE <- iterate _ in amplify(750, timer(10.0)) {
  print("List: "++ ls ++ "\n");
  emit ();
}
