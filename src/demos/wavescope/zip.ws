


// Buffers only *one* element.
fun zip(s1, s2) {
  let slist = [s1, s2];
  iterate((ind, x) in unionList(slist)) {
    state { 
      buf1 = []; // Using list for poor-man's Maybe type.
      buf2 = [];
    }
    if ind == 0 
    then buf1 := [x] // Might throw out element.
    else if ind == 1 
    then buf2 := [x]
    else wserror("implementation error");

    if buf1.listLength == 1  && buf2.listLength == 1
    then { emit (buf1.head, buf2.head);
	   buf1 := [];
	   buf2 := [];
    }
  }
}

s1 = audioFile("countup.raw",32,0);
s2 = audioFile("countup.raw",32,0);

BASE <- zip(s1,s2);
