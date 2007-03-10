
fun f(s) {
  s2 as (x,y) = s;
  //s2.<x>;
  s2.(x,x,y);
}

BASE <- f(iterate(() in timer(3.0)){ emit (1,2) });
