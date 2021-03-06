
// This file is meant to demonstrate how the compiler can merge iterates.
// Run the compiler/emulator with -verbose watch the intermediate code.

s1 = (readFile("./countup.raw", "mode: binary  window: 10", timer(2400.0)) :: Stream (Sigseg Int));

s2 = iterate sigseg in s1 { emit sigseg[[0]]; }

s3 = iterate (y in iterate x in s2 {  
                      emit x - 1;
                   }
             )
{
  emit y / 100;
}

//s3 = iterate x in s2 {
//  emit (x+1)*(x+1);
//}

main = s3;


// x -> [ a of x ] -> [ b of y ] -> ??? ... 

// x -> [ b(a) ] -> ... (replace varrefs)
// x -> [ a(b) ] -> ... (replace emits)

