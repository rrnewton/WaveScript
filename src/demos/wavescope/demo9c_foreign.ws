


//foreign foo :: int -> int = "foo" in "foo.a";

//foreign foo :: int -> int = "foo" in "foo.a";
//foo :: Int -> Int = foreign "foo" in "foo.so"
foo :: Int -> Int = foreign "foo" in "foo.c"

//foo :: (int -> int) = bar;
//foo :: Int = 3
//bar :: Int = 4


//foreign_box far :: (int -> int) = "far_box" in "foo.c";
//foreign_source bar :: (int -> int) = "bar_src" in "foo.c";


BASE <- iterate _ in timer(3.0) { emit foo(3) }


