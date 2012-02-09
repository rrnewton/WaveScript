
//#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h> 
#include <fcntl.h> 

#include <errno.h> 

#include <pthread.h> 


// This builds a struct:
// [2009.06.02] Need some systematic way to generate these wrappers...
struct sockaddr_in* ws_make_sockaddr_in (short sin_family, 
                                         unsigned short portno, 
                                         int s_addr) // NONPORTABLE?
                                         // unsigned long long?
{
  struct sockaddr_in* x = (struct sockaddr_in*)malloc(sizeof(struct sockaddr_in));
  x->sin_family      = sin_family;
  x->sin_port        = htons(portno);
  x->sin_addr.s_addr = s_addr;
  //bcopy(addr, &(x->sin_addr.s_addr), addr_len);
  //bcopy(addr, &(x->sin_addr.s_addr), addr_len);
  return x;
}

int ws_EWOULDBLOCK() { return EWOULDBLOCK; }

/*
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
*/

int ws_errno() { return errno; }

struct thread_arg {
  int port;
  int filedecr;
  char* address;
};


/********************************************************************************/
//                <  Simple association list data type. >
// We could use a hash table from Glib or the linux kernel headers or something.
//
// But instead I am assuming this table will not grow very long and therefore using a
// simple association list to avoid extra dependencies.

// The alist currently stores maps port numbers onto a pointer to the thread_arg
// structure.  (But it stores these pointers as int64s.)
typedef struct alist 
{
  short port;
  int64_t value;
  struct alist* next;
} alist_t;

// Not threadsafe!  No concurrent modification!
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

alist_t* global_outbound_table;
alist_t* global_inbound_table;

/********************************************************************************/

// The helper that runs on a separate thread to setup a socket.
void* outbound_socket_setup_helper_thread(void* vptr) {
  struct thread_arg* arg = (struct thread_arg*)vptr;
  fprintf(stderr,"  <socket.ws> outbound: Spawned thread to listen on port %d\n", arg->port);  

  int sockfd = socket(AF_INET, SOCK_STREAM, 0);
  if (sockfd < 0) { 
    printf("socket_out: ERROR opening socket.");
    exit(99);
  }

  // Make the socket reusable:
  int on = 1; 
  setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on));

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
    fprintf(stderr,"  <socket.ws> outbound: Server got connection (fd %d) on port %d.  Helper thread finished.\n", clientfd, arg->port);
    // Write back the file descriptor:
    arg->filedecr = clientfd;  // WARNING, read by another thread.  May need a fence.
    __sync_synchronize();
    // printf("  <socket.ws> outbuond: Wrote file descriptor %d to %p.\n", clientfd, & arg->filedecr);

    return 0; // Exit the pthread.
  }
}

// Spawn a pthread to setup the socket connection.

// Return a unique identifier for the resulting computation which can be used to later
// retrieve the file descriptor.
int64_t spawn_socket_server_helper(short port) {
  pthread_t threadID;
  // We use a single memory cell here to communicate the port, and then to get back the file descriptor.
  struct thread_arg* cell = malloc(sizeof(struct thread_arg));
  cell->port = port;
  cell->filedecr = 0;

  // Convert the pointer to a scalar:
  int64_t num = (int64_t)cell;

  // NOT THREAD SAFE:
  global_outbound_table = alist_add(global_outbound_table, port, num);
  fprintf(stderr,"  <socket.ws> outbound: Extended table of socket-connecting helper threads with port=%d, val=%p\n", port, num);

  pthread_create(&threadID, NULL, &outbound_socket_setup_helper_thread, (void*)cell);
  return num;
}

// Poll the socket to see if it has connected.
// This will return nonzero if it is ready.
int poll_socket_server_ready(int64_t ptr) {
  struct thread_arg* arg = (struct thread_arg*)ptr;
  if(!ptr) {
    printf(" <socket.ws> ERROR: poll_socket_server_ready called with NULL ptr\n");
    abort();
  }
  // fprintf(stderr, "  <socket.ws> Polled location %p value %ld\n", &arg->filedecr, arg->filedecr);
  return arg->filedecr;
}

// This one uses the port to do a lookup in the global table.
int poll_socket_server_ready_port(short port) {
  // Lookups are threadsafe as long as the table was written/finalized during a single threaded phase.
  int64_t* pptr = alist_lookup(global_outbound_table, port);
  
  if(!pptr) {
    printf(" <socket.ws> ERROR, poll_socket_server_ready_port: no entry in global table for port %d\n", port);
    abort();
  }
  // fprintf(stderr, "  <socket.ws> Alist lookup of port %d returned %p\n", port, *pptr);
  return poll_socket_server_ready(*pptr);
}


/********************************************************************************/
// And for the input side as well:

// This is spawned on a separate thread:
void* inbound_socket_setup_helper_thread(void* vptr) 
{
  struct thread_arg* arg = (struct thread_arg*)vptr;
  fprintf(stderr,"  <socket.ws> inbound: Spawned thread to connect to host %s port %d\n", arg->address, arg->port);

  int sockfd = socket(AF_INET, SOCK_STREAM, 0);
  if (sockfd < 0) { printf("socket_in: ERROR opening socket."); abort(); }

  struct hostent* server = gethostbyname( arg->address );
  if (server == NULL) { printf("socket_in: ERROR no such host"); abort(); }

  struct sockaddr_in* sockaddr = (struct sockaddr_in*)calloc(1,sizeof(struct sockaddr_in));
  sockaddr->sin_family      = AF_INET;
  sockaddr->sin_port        = htons(arg->port);
  //sockaddr->sin_addr.s_addr = server->h_addr; // Why didn't this work?
  // This is a very odd way to assign an unsigned long.  I guess the point is that it could be 4 bytes or more for IPv6 etc.
  memcpy((char *)& sockaddr->sin_addr.s_addr,
         (char *)server->h_addr, 
         server->h_length);
//printf("COPIED %d bytes to s_addr, contents: %ld \n", server->h_length, (unsigned long)sockaddr->sin_addr.s_addr);

  while(1) {
      fprintf(stderr,"  <socket.ws> inbound:   Helper thread starting connection loop.  (host %s port %d)\n", arg->address, arg->port);
      // This is an "upcast" of sockaddr:
      if ( connect(sockfd, (struct sockaddr*)sockaddr, sizeof(struct sockaddr_in)) < 0 ) { 
         int code = get_errno();
         fprintf(stderr,"  <socket.ws> inbound: WARNING: connect returned error code %d, retrying.\n", code);
         usleep(200 * 1000);
         continue;
      } else { 
        fprintf(stderr,"  <socket.ws> inbound: Established client connection, port %d\n", arg->port);
        break;
      }
  }

  // EWOULDBLOCK EAGAIN
  // Make the inbound socket non-blocking (outbound sockets are blocking).
  int flags = fcntl(sockfd, F_GETFL);
  flags |= O_NONBLOCK;
  fcntl(sockfd, F_SETFL, flags);
  fprintf(stderr,"  <socket.ws> inbound: Socket file descriptor set to non-blocking: %d (flags %o)\n", sockfd, flags);

  arg->filedecr = sockfd;  // WARNING, read by another thread.  May need a fence.
  __sync_synchronize();
  fprintf(stderr,"  <socket.ws> inbound: Client made connection (fd %d) on port %d.  Helper thread finished.\n", sockfd, arg->port);
}


int64_t spawn_socket_client_helper(char* address, short port) {
  pthread_t threadID;
  // We use a single memory cell here to communicate the port, and then to get back the file descriptor.
  struct thread_arg* cell = malloc(sizeof(struct thread_arg));
  cell->port = port;
  cell->address = address;
  cell->filedecr = 0;

  // Convert the pointer to a scalar:
  int64_t num = (int64_t)cell;

  // NOT THREAD SAFE:
  global_inbound_table = alist_add(global_inbound_table, port, num);
  fprintf(stderr,"  <socket.ws> inbound: Extended table of inbound socket-connecting helper threads with port=%d, val=%p\n", port, num);

  // [2012.02.09] When we create an inbound connection we increment the active source count:
  wsincr_source_count_fun();

  pthread_create(&threadID, NULL, &inbound_socket_setup_helper_thread, (void*)cell);
  return num;  
}


// DUPLICATED CODE:
int poll_socket_client_ready_port(short port) {
  // Lookups are threadsafe as long as the table was written/finalized during a single threaded phase.
  int64_t* pptr = alist_lookup(global_inbound_table, port);
  
  if(!pptr) {
    printf(" <socket.ws> ERROR, poll_socket_client_ready_port: no entry in global table for port %d\n", port);
    abort();
  }

  struct thread_arg* arg = (struct thread_arg*)(*pptr);
  if(!arg) { printf(" <socket.ws> ERROR: NULL ptr found in table.\n");  abort(); }

  return arg->filedecr;
}


// FIXME -- should only send EOS on OUTBOUND sockets.  These are SIMPLEX connections.
void shutdown_list(alist_t* ptr, int is_outbound)
{
   while (ptr) {
      struct thread_arg* ta = (struct thread_arg*)ptr->value;
      // HACK: TMP: Write out a special EOS length header:
      // ----------------------------------------
      int special = -999;
      if (is_outbound) { 
	  int code = write(ta->filedecr, &special, 4);
          if (code != 4) {
	      fprintf(stderr, "  <socket.ws>  ERROR: Failed to write final EOS length header to descriptor %d, code %d\n", 
		      ta->filedecr, code);
	  }
	  // fflush(ta->filedecr);
      }
      // ----------------------------------------
      close(ta->filedecr);
      fprintf(stderr,"  <socket.ws>   Closed descriptor %d, port %d, outbound %d\n", 
	      ta->filedecr, ta->port, is_outbound);
      ptr = ptr->next;

      // Record that we've lost exactly one input source:
      wsdecr_source_count_fun();
   }
}

// It would be nice to register this as a callback from within wsexit...
void shutdown_sockets() 
{
   fprintf(stderr,"  <socket.ws> Closing all sockets before exiting.\n"); 
   shutdown_list(global_inbound_table, 0);
   shutdown_list(global_outbound_table, 1);
}
