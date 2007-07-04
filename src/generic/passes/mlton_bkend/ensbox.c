
//#include <wavescope_ensbox.h>
#include "wavescope_ensbox.h"


/* this is called from the ENSBox lib */
void wavescope_push(void *data, char *buf, int count) 
{
  wsmlton_entry(buf, count);
}

void startup() {
    static char *argv[2] = { "", 0 };
    int argc = 1;
    ensbox_main(argc, argv); 
    ensbox_start(0);
}
