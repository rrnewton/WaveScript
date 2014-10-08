include "stdlib.ws";

// [2007.04.09] Hmm... having a problem with this not elaborating
// properly if I do a using rather than Array:fold.

// OH! This has to do with the source positions.

// [2007.06.29] Disabling this till it works in more backends.
//src = union2(timer(3.0), timer(4.0))
src = COUNTUP(30);

fun assert_prnt(str,a,b) {
  assert_eq(str,a,b);
  print("Assert passed: "++ str ++ ": " ++  b ++"\n");
}

// Deinterleave both with SS and plain version:
s1 = window(src, 10);
ls = deinterleaveSS(2, 10, s1);
ch1 = dewindow$  ls`List:ref(0);
ch2 = dewindow$  ls`List:ref(1);

ch1b = deinterleave(2, src) ` List:ref(0);

zipped = zipN_sametype(10, [ch1,ch1b]);

zipped2 = zip4_sametype(10, ch1,ch1b, ch1,ch1b);

everyother = iterate x in s1 {
  state { flag = true }
  if flag then emit x;
  flag := not(flag);
}

degapped = degap(everyother, gint(0), 1000);


test_unions = iterate sum in union2(degapped,zipped) {
    {
    print("Type Unions: ");
      x = Left(3);
      case x {
        Left(x): println("Got left! ")
        Right(y): println("Got right! ")
      }
    };

  case sum {
    // Degapped:
    Left(x) : println("woot "++x)

    // Zipped:
    Right(x): {
      assert_prnt("deinterlaces the same", List:ref(x,0), List:ref(x,1));
      //emit "Right";
      emit x;
    }
  }
}


unionsSupported = 
  if GETENV("WSVARIANT") == "wsc2"
  then false else true

_ = println("unionsSupported: "++ unionsSupported)

middle = if unionsSupported
         then test_unions
         else zipped

result = iterate _ in middle {
  state { first = true }
  if first then {
    first := false;
    {
      println("\n  Basic Functions");
      println("=====================");
      assert_prnt("ceilF ", ceilF(3.01), 4.0);
      assert_prnt("ceilF ", ceilF(3.0),  3.0);
      assert_prnt("floorF ", floorF(3.0),  3.0);
      assert_prnt("floorF ", floorF(3.01),  3.0);

      assert_prnt("ceilD ", ceilD(Double! 16.01), 17);
      assert_prnt("ceilD ", ceilD(Double! 16.0),  16);
      assert_prnt("floorD ", floorD(Double! 16.0),  16);
      assert_prnt("floorD ", floorD(Double! 16.01), 16);
    };

    {
      println("\n  FIFO ADT ");
      println("=====================");
      using FIFO;
      q = make(10);
      enqueue(q,1);
      enqueue(q,2);
      assert_prnt("fifo elements", q`elements, 2);
      assert_prnt("fifo peek 1", q`peek(0), 1);
      assert_prnt("fifo peek 2", q`peek(1), 2);
      enqueue(q,3);
      println("Just enqueued.");
      assert_prnt("fifo andmap #t", andmap((> 0), q), true);
      assert_prnt("fifo andmap #f", andmap((< 3), q), false);
      x = dequeue(q);
      y = dequeue(q);
      enqueue(q,4);
      z = dequeue(q);
      w = dequeue(q);
      assert_prnt("fifo", [1,2,3,4], [x,y,z,w]);
    };

    {
      println("\n  Array primitives: ");
      println("=====================");
      using Array;

      assert_prnt("Length of null", length(#[]), 0);

      arr = build(10, fun(x) x);
      assert_prnt("Built 0..9:", arr, #[0,1,2,3,4,5,6,7,8,9]);
      f1 = Array:fold((+), 0, arr);
      f2 = Array:fold1((+), arr);
      assert_prnt("Folds equal", f1,f2);

      new = make(15,0);
      Array:blit(new, 2, arr, 5, 5);
      println("Blit: " ++ new);
      sb = sub(new, 2, 5);
      println("Sub: " ++ sb);
      println("Concat: " ++ concat([sb,sb,sb]));
      println("Append: " ++ append(sb,sb));

      //flipped = fun(x,y) (y:::x); 
      //      println("FoldCons:  " ++ Array:fold(fun(x,y)(y:::x), [], arr));

      //      println("FoldRange: " ++ Array:foldRange(3, 4, 0, (+)));
      //      println("FoldRange: " ++ Array:foldRange(3, 7, [], fun(x,y)(y:::x)));
    };

    println("");
    {
       using List;
       println("  List primitives: ");
       println("====================");
       ls1 = [1,2,3];
       ls2 = [4,5,6];        
       summed = List:map2((+),ls1,ls2);
       assert_prnt("List:map2", [5,7,9], summed);

       assert_prnt("List:mapi", 6, List:fold1((+), List:mapi(fun(i,x) i, [0,0,0,0])));

       assert_prnt("choplast", (4,[1,2,3]), choplast([1,2,3,4]));

       bld = List:build(5, fun(i)i);
       assert_prnt("List:build", bld, [0,1,2,3,4]);
       assert_prnt("List:foldi", foldi(fun(i,sum,x) i+sum+x, 0, bld), 20);
    };

    {
       println("\n  String primitives: ");
       println("====================");
       using String;
       assert_prnt("toArray", toArray("abc"), #['a','b','c']);
       assert_prnt("toArray of empty", toArray(""), #[]);
       assert_prnt("fromArray of null", fromArray(#[]), "");
    }
       
  };
  emit ();
}



//main = zipped
//main = zip2_sametype(ch1, ch1b)
//main = union2(degapped,zipped)
main = result
