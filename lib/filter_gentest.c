
#include <math.h>
#include <stdio.h>

float sqr(float x) { return x*x; }

int main()
{
  int i;

  for (i=1; i<16384; i++) {
    float j = 128.0 * sqr(((float)i) / 16384.0);
    float v = 100*sin(2*j*i/512.0);
    printf("%f %f\n", v, j);
  }

  return 0;
}
