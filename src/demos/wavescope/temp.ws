
/* //include "stdlib.ws"; */


 fun bar(r) if true then ( r | X = 2 ) else ( (|) | X = 4 )

/*

 fun foo(r) if true then ( r | X = 2 ) else ( r | Y = 2 )
 fun bar(r) if true then ( r | X = 2 ) else ( (|) | X = 4 )

 fun baz(r1,r2) if true then ( r1 | X = 2 ) else ( r2 | X = 4 )
 fun qux(r1,r2) if true then ( r1 | X = 2 ) else ( r2 | Y = 4 )

 */


/*

main = iterate _ in timer(1) {
  r = (A=3, B="four");
  print(r ++ "\n");
  print(r.A ++ "\n");

  r2 = (r | C=());
  print(r2 ++ "\n");

  emit 0;
}
*/



/* using TOS; */
/* namespace Node { */
/*   bufsize = 2000;  */
/*   rate = 8000.0; */
/*   src1 = read_telos_audio(bufsize, rate); */
/*   s2 = iterate arr in src1 { */
/*     len = Array:length(arr); */
/*     if true */
/*     then { */
/*       // Note, this hackishly does out of bounds accesses!! */
/*       print(arr[-1] ++ " | "); */
/*       print(arr[0] ++ " "); */
/*       print(arr[1] ++ " "); */
/*       print(arr[2] ++ " "); */
/*       print(arr[3] ++ " ... "); */
/*       print(arr[len-2] ++ " "); */
/*       print(arr[len-1] ++ " | "); */
/*       print(arr[len] ++ " \tsum: ");       */

/*       sum = Array:fold(fun(acc,n) acc + (cast_num(n)::Int32), (0::Int32), arr); */
/*       print((sum) ++ "  avg: "); */
/*       print((sum / (cast_num(len)::Int32)) ++ "\n"); */
/*     }; */
/*     emit len; */
/*   } */
/* } */

/* main = Node:s2; */
