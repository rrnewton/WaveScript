

// First test out namespaces a little bit:
namespace Foo {
  s0 = timer(300.0);
}
namespace Bar {
  namespace Baz {
    s1 = { using Foo; s0 };
  }
}

// Nested namespace:
s2 = Bar:Baz:s1;
//s3 = { using Bar:Baz; s1} // BREAKS CURRENTLY!!!
__s = { using Bar; using Baz; s1}

// Next, test out 'as' syntax:
fun f(s) {
  news as (x,y) = s;
  news.(x,x,y);
}
s3 = iterate () in s2 { emit (1,2) };
s4 = f(s3);
s5 as (a,b,c) = s4;
s6 = s5.(a,b,c, a,b,c)

// [2008.08.15] Now test implicit ref variables:
foo = 0;
s7 = iterate x in s6 {  
  bar = 0;
  foo += 1;
  bar += 1;  
  emit (foo,bar,x)
}
 
main = s7
