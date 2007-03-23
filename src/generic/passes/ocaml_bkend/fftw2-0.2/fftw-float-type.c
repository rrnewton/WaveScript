/* Check the type of float/complex: single or double */

#include <stdio.h>
#include <fftw.h>

int main(void)
{
#ifdef FFTW_ENABLE_FLOAT
    printf("32");
#else
    printf("64");
#endif
    
    return(0);
}
