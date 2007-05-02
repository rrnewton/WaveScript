


foo1 :: Int -> Int = foreign "foo" in "foo.c"
foo2 :: Int -> Int = foreign "foo" in "foo.c"


//box = (foreign_box    "bar_box" in "foo.c" :: Stream Int -> Stream Int);
//src = (foreign_source "bar_src" in "foo.c" :: Stream Int);



BASE <- iterate _ in timer(30.0) { emit foo2( foo1(3)) }

