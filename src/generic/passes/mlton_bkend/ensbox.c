
#include <wavescope_ensbox.h>
//#include "wavescope_ensbox.h"


int counter = 0;

/* this is called from the ENSBox lib */
// 48 K samples per second
void wavescope_push(void *data, char *buf, int count) 
{
  //printf("GOT PUSH %p %d %d \n", buf, count, counter);
  wsmlton_entry(buf, count);
}

void init_ensbox() {

    printf("Called into C to initialize and start vxpcdsystem\n"); 

    static char *argv[2] = { "", 0 };
    int argc = 1;
    ensbox_main(argc, argv); 
    ensbox_start(0);

/*        short* foo = malloc(10 * sizeof(short));     */
/*     wsmlton_entry(foo,10); */
/*     wsmlton_entry(foo,10); */
/*     wsmlton_entry(foo,10); */
}
