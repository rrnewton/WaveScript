

fun pullN(N, S) {
  iterate(x in S) {
    state { count :: Int = 0; }
    print("Fired " ++ show(count) ++" of " ++ show(N) ++ ".\n");
    if count < N
    then emit x
    else (); // Here we would like an *exit* primitive.
    count := count + 1;
  }
}

main = pullN((5::Int), timer(2.0));
