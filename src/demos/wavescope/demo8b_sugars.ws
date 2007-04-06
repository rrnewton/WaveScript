
namespace Foo {
  //s0 = timer(300.0);
  s0 = (readFile("./countup.raw", "mode: binary  window: 4096") :: Stream (Sigseg Int));
}

namespace Bar {
  namespace Baz {
    s1 = { using Foo; s0 };
  }
}

s2 = Bar:Baz:s1;


fun f(s) {
  s2 as (x,y) = s;
  //s2.<x>;
  s2.(x,x,y);
}

fun id(x) x;

BASE <- f(iterate(() in s2){ emit (1,2) });
