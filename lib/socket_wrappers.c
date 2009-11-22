
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

// These deal with pthreads:

int start_spawn_socket_server(int port) {
  printf("SPAWNING: port %d\n", port);
  return 0;
  //  pthread_create(&threadID, NULL, &worker_thread, (void*)(size_t)i);

/*       sockfd = socket(Int! AF_INET(), Int! SOCK_STREAM(), 0); */
/*       if sockfd < 0 then error("socket_out: ERROR opening socket."); */
/*       serv_addr = make_sockaddr_in(AF_INET(), port, INADDR_ANY()); */
/*       b = bind(sockfd, serv_addr, sizeof_sockaddr()); */
/*       if b < 0 then error("socket_out: ERROR on binding: "++b++ " errno " ++ errno()); */
/*       listen(sockfd,5);       */
/*       // Accept client connection: */
/*       cli_addr = make_sockaddr_in(0,0,0); */
/*       clilen = #[sizeof_sockaddr()]; */
/*       clientfd := accept(sockfd, cli_addr, clilen); */

}

bool socket_server_ready(int id) {
  printf("CHECKING SERVER READY... %d\n", id);
  return 1;
}




/********************************************************************************/
// SCRAP

/*

struct hostent {
        char    *h_name;        
        char    **h_aliases;    
        int     h_addrtype;     
        int     h_length;       
        char    **h_addr_list;  
}
#define h_addr  h_addr_list[0]  

struct sockaddr {
    unsigned short    sa_family;    // address family, AF_xxx
    char              sa_data[14];  // 14 bytes of protocol address
};


// IPv4 AF_INET sockets:

struct sockaddr_in {
    short            sin_family;   // e.g. AF_INET, AF_INET6
    unsigned short   sin_port;     // e.g. htons(3490)
    struct in_addr   sin_addr;     // see struct in_addr, below
    char             sin_zero[8];  // zero this if you want to
};

struct in_addr {
    unsigned long s_addr;          // load with inet_pton()
};


// IPv6 AF_INET6 sockets:

struct sockaddr_in6 {
    u_int16_t       sin6_family;   // address family, AF_INET6
    u_int16_t       sin6_port;     // port number, Network Byte Order
    u_int32_t       sin6_flowinfo; // IPv6 flow information
    struct in6_addr sin6_addr;     // IPv6 address
    u_int32_t       sin6_scope_id; // Scope ID
};

struct in6_addr {
    unsigned char   s6_addr[16];   // load with inet_pton()
};


// General socket address holding structure, big enough to hold either
// struct sockaddr_in or struct sockaddr_in6 data:

struct sockaddr_storage {
    sa_family_t  ss_family;     // address family

    // all this is padding, implementation specific, ignore it:
    char      __ss_pad1[_SS_PAD1SIZE];
    int64_t   __ss_align;
    char      __ss_pad2[_SS_PAD2SIZE];
};

*/
