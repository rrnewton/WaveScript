
//#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h> 

#include <errno.h> 

#include <pthread.h> 

// [2009.06.02] Need some systematic way to generate these wrappers...

// This builds a struct:

struct sockaddr_in* ws_make_sockaddr_in (short sin_family, 
                                         unsigned short portno, 
					 int s_addr	 
                                         //struct hostent *server
					 )
{
  struct sockaddr_in* x = (struct sockaddr_in*)malloc(sizeof(struct sockaddr_in));
  x->sin_family      = sin_family;
  //x->sin_port        = portno;
  x->sin_port        = htons(portno);
  x->sin_addr.s_addr = s_addr;
  //bcopy(addr, &(x->sin_addr.s_addr), addr_len);
  //bcopy(addr, &(x->sin_addr.s_addr), addr_len);
  /*
  bcopy((char *)  (server->h_addr), 
        (char *)& (x->sin_addr.s_addr),
        server->h_length);
  */
  return x;
}

int ws_hostent_h_addr(struct hostent* server) {
  // HACK, assumes ipv4 & 4 byte int:
  return *(int*)server->h_addr; 
}

// These build enum values:

short ws_AF_INET() { return AF_INET; }
unsigned short ws_SOCK_STREAM() { return SOCK_STREAM; }
int ws_INADDR_ANY() { return INADDR_ANY; }

int ws_sizeof_sockaddr() { return sizeof(struct sockaddr); }

int ws_print_sockaddr(struct sockaddr* addr) {   
  struct sockaddr_in* x = (struct sockaddr_in*)addr;
  printf("Sockaddr, assuming _in: %p \n", x);
  printf("  family %d  port %d  s_addr %X \n", 
         x->sin_family, ntohs(x->sin_port), x->sin_addr.s_addr);  
}

int ws_errno() { return errno; }

struct thread_arg {
  int port;
  int filedecr;
};

/********************************************************************************/

// We could use a hash table from Glib or something.
// But we probably 

typedef struct alist 
{
  short port;
  int64_t value;
  struct alist* next;
} alist_t;

// No concurrent modification!
alist_t* alist_add(alist_t* ls, short p, int64_t v)
{
  alist_t* newp = (alist_t*)malloc(sizeof(alist_t));
  newp->port  = p;
  newp->value = v;
  newp->next  = NULL;

  if (ls == NULL)  return newp;

  alist_t* ptr = ls;
  while (ptr->next != NULL) {
    ptr = ptr->next;
  }
  ptr->next = newp;
  return ls;
}

// Null return value means it is not present.
int64_t* alist_lookup(alist_t* ls, short p) {
   alist_t* ptr = ls;
   while ( ptr != NULL ) {
      if (ptr->port == p)  return & ptr->value;
      ptr = ptr->next;
   }
   return NULL;
}

alist_t* global_table;

/********************************************************************************/

// The helper that runs on a separate thread to setup a socket.
void* socket_setup_helper(void* vptr) {
  struct thread_arg* arg = (struct thread_arg*)vptr;

  printf("  <socket.ws> Spawned thread to listen on port %d\n", arg->port);  

  int sockfd = socket(AF_INET, SOCK_STREAM, 0);
  if (sockfd < 0) { 
    printf("socket_out: ERROR opening socket.");
    exit(99);
  }

  const struct sockaddr_in* serv_addr = ws_make_sockaddr_in(AF_INET, arg->port, INADDR_ANY);
  int b = bind(sockfd, (const struct sockaddr*)serv_addr, sizeof(struct sockaddr_in));
  if (b < 0) { 
    printf("socket_out: ERROR on binding: %d errno %d\n", b, errno);
    exit(99);
  }

  int l = listen(sockfd, 5);
  if (l < 0) { 
    printf("socket_out: ERROR on listen: returned %d, errno %d\n", l, errno);
    exit(99);
  }
  
  // Accept client connection:
  //const struct sockaddr_in* cli_addr = ws_make_sockaddr_in(0,0,0);
  struct sockaddr cli_addr;
  int clientfd;
  socklen_t size = sizeof(cli_addr);
  while (1) {
    clientfd = accept(sockfd, &cli_addr, &size);
    if (clientfd < 0) { 
      //printf("socket_out: ERROR on accept: returned %d, errno %d\n", clientfd, errno);
      //exit(99);
      perror("accept");
      continue;
    }
    printf("  <socket.ws> Server got connection (fd %d) on port %d.  Helper thread finished.\n", clientfd, arg->port);
    // Write back the file descriptor:
    arg->filedecr = clientfd;  // WARNING, read by another thread.  May need a fence.
    __sync_synchronize();

    printf("  <socket.ws> Wrote file descriptor %d to %p.\n", clientfd, & arg->filedecr);

    return 0; // Exit the pthread.
  }
}

// Spawn a pthread to setup the socket connection.

// Return a unique identifier for the resulting computation which can be used to later
// retrieve the file descriptor.
int64_t start_spawn_socket_server(short port) {
  pthread_t threadID;

  // We use a single memory cell here to communicate the port, and then to get back the file descriptor.
  struct thread_arg* cell = malloc(sizeof(struct thread_arg));
  cell->port = port;
  cell->filedecr = 0;

  // Convert the pointer to a scalar:
  int64_t num = (int64_t)cell;

  // NOT THREAD SAFE:
  global_table = alist_add(global_table, port, num);
  fprintf(stderr,"  <socket.ws> Extended global table of socket-connecting helper threads with port=%d, val=%p\n", port, num);

  pthread_create(&threadID, NULL, &socket_setup_helper, (void*)cell);
  return num;
}

// Poll the socket to see if it has connected.
// This will return nonzero if it is ready.
int socket_server_ready(int64_t ptr) {
  struct thread_arg* arg = (struct thread_arg*)ptr;
  if(!ptr) {
    printf(" <socket.ws> ERROR: socket_server_ready called with NULL ptr\n");
    abort();
  }
  // fprintf(stderr, "  <socket.ws> Polled location %p value %ld\n", &arg->filedecr, arg->filedecr);
  return arg->filedecr;
}

// This one uses the port to do a lookup in the global table.
int socket_server_ready_port(short port) {
  // Lookups are threadsafe as long as the table was written/finalized during a single threaded phase.
  int64_t* pptr = alist_lookup(global_table, port);
  
  if(!pptr) {
    printf(" <socket.ws> ERROR, socket_server_ready_port: no entry in global table for port %d\n", port);
    abort();
  }
  // fprintf(stderr, "  <socket.ws> Alist lookup of port %d returned %p\n", port, *pptr);
  return socket_server_ready(*pptr);
}
