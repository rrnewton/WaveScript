
fun lcm(num1,num2) {
  using Mutable;
  n1 = ref$ num1;
  n2 = ref$ num2;
  product = num1 * num2;  
  while n1 != 0 {
    // Swap so that n1 >= n2
    if n1 < n2 then {
      tmp = n1;
      n1 := n2;
      n2 := tmp;
    };
    n1 := n1.moduloI(n2);
  };
  product / n2;
}


/*
unsigned long
isqrt(x)
unsigned long x;
{
    register unsigned long op, res, one;
    op = x;
    res = 0;
    /* "one" starts at the highest power of four <= than the argument. */
    one = 1 << 30;  /* second-to-top bit set */
    while (one > op) one >>= 2;
    while (one != 0) {
        if (op >= res + one) {
            op = op - (res + one);
            res = res +  2 * one;
        }
        res >>= 1;
        one >>= 2;
    }
    return(res);
}
*/
