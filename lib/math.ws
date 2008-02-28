
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
