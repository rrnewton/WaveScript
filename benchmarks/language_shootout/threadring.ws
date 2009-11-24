
include "stdlib.ws"

fun spawnthread(id,s) 
  iterate n in s {
    if n == 1 
    then println(id)
    else emit n-1
  }

fun chain(cnt, strm) {
  if cnt ==0 
  then strm
  else spawnthread(id, chain(cnt-1, strm))
}

startit = iterate _ in timer(0) { emit 10 * 1000 * 1000 }

main = feedbackloop(startit, fun(s) chain(503,s))


/*
fun chain(cnt, k) {
  if cnt ==0 
  then k
  else chain(cnt-1, fun(s) thread(id, k(s)))
}


fun chain(cnt) 
  fun(strm) {
    if cnt ==0 
    then strm
    else thread(id, chain(cnt-1))
  }


//main = tieKnot()
//main = closeLoop(chain(503))

main = closeLoop(chain(503, fun(s) ))

// What?  Return an empty stream?
// Snoop on the point where the loop is closed?
closeLoop :: (Stream t -> Stream t) -> Stream a;

*/
