
//namespace Foo {
s1 = timer(3.0);
//}

fun f(s) {
  s2 as (x,y) = s;
  //s2.<x>;
  s2.(x,x,y);
}

// s2 = { using Foo; iterate... }
//using Foo;
// Foo:f

BASE <- f(iterate(() in s1){ emit (1,2) });
