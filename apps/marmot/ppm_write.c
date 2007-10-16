
//#include <libmisc/misc.h>
#include <stdio.h>

int write_ppm_file(const char *filename, int wid, int height, int* R, int* G, int* B)
{
  FILE *fptr;
  double **new_plot;
  int x,y;
  fptr = fopen(filename,"w");

  if (fptr == NULL)
    return -1; // didn't read the file!

  // output results to a ppm file (very simple :-)
  fprintf(fptr,"P6\n");
  fprintf(fptr,"%d %d\n", wid, height); // size of the file
  fprintf(fptr,"255\n");         // colour depth
  for (y=0; y < height; y++) {
    for (x=0; x < wid; x++) {
      int ind = x + (y * wid);
      fprintf(fptr,"%c", R[ind]); // red
      fprintf(fptr,"%c", G[ind]); // green
      fprintf(fptr,"%c", B[ind]); // blue
    }
  }
  fclose(fptr);
  return 0; // success!
}
