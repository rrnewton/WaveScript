
#include <time.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv) {

  int i,j,k;
  int c1,c2,r1,r2;
  c1=c2=r1=r2=500;
  double *x1 = calloc(c1*r1,sizeof(double));
  double *x2 = calloc(c1*r1,sizeof(double));
  double *r = calloc(c1*r1,sizeof(double));

  for (i=0; i<c1; i++) {
    for (j=0; j<r1; j++) {
      x1[j*c1+i] = i+j;
      x2[j*c1+i] = i*j;
    }
  }

  clock_t start,end;

  start = clock();
  for (i=0; i<r1; i++) {
    int tmpiout = i*c2;
    for (j=0; j<c2; j++) {
      double sum = 0;
      int tmpi = i*c1;
      int tmpj = j;
      for (k=0; k<r2; k++) {
	sum += x1[tmpi] * x2[tmpj];
        tmpi += 1;
        tmpj += c2;
      };
      r[tmpiout] = sum;
      tmpiout++;
    }
  }  
  end = clock();

  printf("diff=%lf ms\n", (end-start)/(CLOCKS_PER_SEC/1000.0));

  // prevent optimization !
  double s = 0;
  for (i=0; i<c1*r1; i++) s += r[i];
  printf("sum=%lf\n", s);

  return 0;
}
