
#include <time.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv) {

  int i,j,k;
  int c1,c2,r1,r2;
  c1=c2=r1=r2=1000;
  double *x = calloc(c1*r1,sizeof(double));
  double *r = calloc(c1*r1,sizeof(double));

  for (i=0; i<c1*r1; i++) x[i] = i;

  clock_t start,end;

  start = clock();
  for (i=0; i<r1; i++) {
    int tmpiout = i*c2;
    for (j=0; j<c2; j++) {
      double sum = 0;
      int tmpi = i*c1;
      int tmpj = j;
      for (k=0; k<r2; k++) {
	sum += x[tmpi] * x[tmpj];
        tmpi += 1;
        tmpj += c2;
      };
      r[tmpiout] = sum;
      tmpiout++;
    }
  }  
  end = clock();

  printf("diff=%lf\n", (end-start)/(double)CLOCKS_PER_SEC);

  // prevent optimization !
  double s = 0;
  for (i=0; i<c1*r1; i++) s += r[i];
  if (s > 100000) {
    printf("\n");
  }

  return 0;
}
