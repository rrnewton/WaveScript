
// This isn't actually used, but it's here for reference.

/* 
 
 The basic interface is simple: init, size, get, put.
 Put needs to be a macro because it passes the type.

 */

void wsfifoinit(wsfifo* ff, int optional_size_limit, int elemsize);
int  wsfifosize(wsfifo* ff);
void* wsfifoget(wsfifo* ff);
// #define WSFIFOPUT(ff, val, ty)

/* 

 We optionally use an extendend interface for two-stage fifos.

 */

int wsfifo_pending(wsfifo* ff);
void* wsfifo_recheck(wsfifo* ff);
void wsfifo_release_one(wsfifo* ff);


void grab_wsfifo(wsfifo*);
void release_wsfifo(wsfifo*);
