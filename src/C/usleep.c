/* usleep.c    ---   foreign function
 *
 *
 * Compile with (Solaris):
 *   gcc -fPIC -G -o usleep.so usleep.c 
 *
 * Compile with (linux):
 *   gcc -fPIC -shared -o usleep.so usleep.c 
 *
 *
 * Bring it in with:
    (define usleep (foreign-procedure "usleep" (integer-32) void))
 *
 */


#include <unistd.h>
//#include <sys/time.h>
//#include <sys/types.h>

//int usleep(unsigned long);
//int usleep(useconds_t msecs);

void rrn_usleep( unsigned long msecs ) {
  // I subtract a bit for the overhead:
  if (msecs > 1)
    usleep((msecs - 1) * 1000);
  //printf("usleep called! %d\n", msecs);
  //printf("usleep returned: %d\n", usleep((msecs - 1) * 1000));
  //printf("hrm\n");
}
