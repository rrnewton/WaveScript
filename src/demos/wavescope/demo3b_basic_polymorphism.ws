

fun f(x) { x ++ "bar" }

BASE <- iterate _ in timer(3.0) { 
  emit (f(3), f(true));
}
