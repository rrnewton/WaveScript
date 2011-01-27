

src :: Stream Int = foreign_source("wsentry", ["demo9l_foreign_back_and_forth.c"])

modified = iterate n in src { emit n * 10 }

// Here's our C function to serve as a sink.
rcvr :: Int -> () = foreign("wssink", ["demo9l_foreign_back_and_forth.c"])

// Here's the sink.
main = iterate n in modified { rcvr(n); emit () }


