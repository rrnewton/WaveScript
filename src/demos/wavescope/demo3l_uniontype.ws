





//union type Foo = A Int | B Float;
//type union Foo = A Int | B Float
//type Foo = A of Int | B of Float

uniontype Foo = A Int | B Float;

x = A(3);
y = (+ 3) $ 4;
f = (3 *);
z = y`f + y.f;

BASE <- iterate _ in timer(3.0) { emit x }
