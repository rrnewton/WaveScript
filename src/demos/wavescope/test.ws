
chans = (dataFile("6sec_marmot_sample.raw", "binary", 44000, 0) :: Stream (Int16 * Int16 * Int16 * Int16));
//ch1 = window(iterate((a,_,_,_) in chans){ emit intToFloat(a) }, 100);

BASE <- iterate(w in chans) {
  emit (w.width(x,y,z));
  //print("woot " ++ 3 ++ " \n")
}

/* BASE <- iterate(w in ch1) { */
/*   arr = toArray(w); */
/*   new = makeArray(w.width, nullarr); */
/*   for i = 0 to w.width-1 { */
/*     new[i] := arr; */
/*   } */
/*   emit m_invert(new); */
/* } */
