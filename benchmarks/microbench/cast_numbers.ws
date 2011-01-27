


main = iterate _ in timer(1) {  

  i = 9;
  f :: Double = 9;
  
  st = clock();
  for _ = 1 to 10 * 1000 * 1000 
  { 
    //for _ = 1 to 1000 * 1000 {
      i := Int! (f + 1);
      //f := Float! (i - 1);
      f := Double! (i - 1);
      //}
  };
  print$ "Time for ?? int/float conversions: "++ clock() - st ++" "++ i ++ "\n";

  emit ();    
}
