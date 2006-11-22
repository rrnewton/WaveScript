


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
    else wserror("implementation error: got ind "++ show(ind));

    if buf1.listLength == 1  && buf2.listLength == 1
    then { emit (buf1.head, buf2.head);
	   buf1 := [];
	   buf2 := [];
    }
  }
};

countup = iterate(_ in timer(3000)) {
  state{ cnt = 0 }
  cnt := cnt+1;
  emit cnt;
};


s1 = countup;
s2 = smap(fun(x) x+1, countup);

BASE <- zip(s1,s2);
