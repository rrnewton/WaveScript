


#include <wavescope_ensbox_new.h>
//#include "wavescope_ensbox.h"

int counter = 0;

/* this is called from the ENSBox lib */
// 48 K samples per second
void wavescope_push(char *buf, int count, uint64_t sample_count) 
{
  printf("GOT PUSH FROM MIKE %p %d \n", buf, count);
  wsmlton_entry(buf, count);
  // MAKE SURE TO FREE *buf when finished with..
  free(buf);
}

// This doesn't do anything currently.  Mike initialized vxpcd for me.
void init_ensbox() {

    printf("Called into C to initialize and start vxpcdsystem\n"); 
    
    static char *argv[2] = { "", 0 };
    int argc = 1;
    //ensbox_main(argc, argv); 
    //ensbox_start(0);
    ws_main(argc, argv);    
}
