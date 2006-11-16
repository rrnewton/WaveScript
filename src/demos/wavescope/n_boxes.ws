
s1 = audioFile("./countup.raw", 10, 0);


//pipe : Sigseg Float, Int -> Sigseg Float;
fun pipe(inStr, n)
{
   if n==0
   then inStr
   else iterate (w in inStr) { pipe(inStr, n-1) + 1 };
}

str = pipe(s1, 3);
//str = s1;

BASE <- str;
