
// [2009.06.02] A socket library.
// [2011.05.20] Trying a new version where we keep some global state.

include "stdlib.ws"
include "unix.ws"

//================================================================================
// First, we extend the Unix namespace with these basic wrappers.

socket_pthread_includes = 
  ["sys/types.h", "sys/socket.h", "netinet/in.h", "socket_wrappers.c", 
   "pthread.h", "socket_wrappers.c", "libpthread.so"]

namespace Unix {
  //  socket_includes = ["sys/types.h", "sys/socket.h", "netinet/in.h", "socket_wrappers.c"]

  socket  :: (Int,Int,Int) -> Int                          = foreign("socket", socket_pthread_includes);
  connect :: (Int, Pointer "struct sockaddr*", Int) -> Int = foreign("connect", socket_pthread_includes);
  bind    :: (Int, Pointer "struct sockaddr*", Int) -> Int = foreign("bind", socket_pthread_includes)
  listen  :: (Int, Int) -> Int                             = foreign("listen", socket_pthread_includes);
  accept  :: (Int, Pointer "struct sockaddr*", Array Int) -> Int = foreign("accept", socket_pthread_includes);
  
  gethostbyname :: String -> Pointer "struct hostent*"  = foreign("gethostbyname", socket_pthread_includes);
  hostent_h_addr :: Pointer "struct hostent*"  -> Int   = foreign("ws_hostent_h_addr", socket_pthread_includes);

  make_sockaddr_in :: (Int16, Uint16, Int) -> Pointer "struct sockaddr_in*"
    = foreign("ws_make_sockaddr_in", socket_pthread_includes);

  AF_INET     :: () -> Int16  = foreign("ws_AF_INET", socket_pthread_includes);
  SOCK_STREAM :: () -> Uint16 = foreign("ws_SOCK_STREAM", socket_pthread_includes);
  INADDR_ANY  :: () -> Int    = foreign("ws_INADDR_ANY", socket_pthread_includes);

  errno  :: () -> Int = foreign("ws_errno", socket_pthread_includes);

  // These wrap enum values.
  // These don't work yet... but it shouldn't be too hard to make them.
  //AF_INET     :: Int16 = foreign("AF_INET", socket_pthread_includes);
  //SOCK_STREAM :: Uint16 = foreign("SOCK_STREAM", socket_pthread_includes);

  sizeof_sockaddr :: () -> Int = foreign("ws_sizeof_sockaddr", socket_pthread_includes);
  print_sockaddr  :: (Pointer "struct sockaddr*") -> () = foreign("ws_print_sockaddr", socket_pthread_includes);
}

//================================================================================

// High level interface for socket communication.
// API:

// socket_in() and socket_out() return two values:
//   * result stream -- either with input tuples or the empty stream (for output)
//   * initialization stream function --
//     This is an initialization function with a funky representation.
//     It shoud be called once at startup; the way we accomplish that is by feeding the stream
//     function a singleton stream (e.g. timer(0)).  This is the users responsibility.

//================================================================================


// Socket_in goes here.


//================================================================================

/* [2009.10.20] Version 2 

   We can avoid blocking.  We simply have to fork an additional
   pthread that does the blocking for us.  Without changing the WS
   compiler it's possible for us to use the foreign interface to
   create our own separate subsystem that establishes all the
   connections.

   That's fine if we are ok with data dumping on the floor while we
   wait for connections to be made.  (Which in turn requires that
   programs be robust against arbitrary *skew*.).  The difficulty is
   if we *want* the whole system to block (backpressure).  We can't
   just block within an individual kernel, because we don't know the
   threading structure of the backend we're in.  For this to work I
   think we'll need to expose a WS call that this socket library can
   use to quiesce the system.  That in turn will require support in
   all the backends that support the FFI.

   (How would I implement that on a future TBB backend where I don't
   control the scheduler?)

*/

// my_pthread_includes = ["pthread.h", "socket_wrappers.c", "libpthread.so"]

// Create a separate thread to wait for the client to connect.
start_spawn_socket_server  :: Uint16 -> Int64 = foreign("start_spawn_socket_server", socket_pthread_includes)
socket_server_ready        :: Int64  -> Int   = foreign("socket_server_ready",       socket_pthread_includes)

// This makes a blocking call to connect to a server.  We need to put
// this on its own thread as well to avoid deadlock.  (Case in point:
// imagine a program that connects to its own socket with a
// single-threaded scheduler.  )

socket_out :: (Stream a, Uint16) -> Stream b; // Returns empty stream.
fun socket_out(strm, port) {
  //  pthread_create(&threadID, NULL, &worker_thread, (void*)(size_t)i);	
  // The thread will return when it has made its connection.
  iterate x in strm {
    state { first = true; 
            id = 0;
            clientfd = 0;

            // [2011.05.20] New hack -- introduce a small amount of buffering so that,
            // during the disconnected phase, we can RETURN from this kernel and let 
            // sockets initiate their connections.
            // buffer = 

            nodrop = { 
              envvar = GETENV("WS_SOCKET_ALLOWDROP");
              tmp = envvar == "" || envvar == "0";
              if not(tmp) then 
              Unix:puts_err(" <socket.ws> WS_SOCKET_ALLOWDROP unset; stall data sources until sockets are up.  REQUIRES SINGLE THREADED EXECUTION.\n");
              tmp
            }
          }
    using Unix;
    if first then {
      first := false;
      id    := start_spawn_socket_server(port);
    };

    fun shoot() // Requires clientfd be ready.
    {
        // Marshal and send it:
        bytes = marshal(x);    
        len = Array:length(bytes);  
        lenbuf = marshal(len);  
        // Use unix 'write' command:
        write_bytes(clientfd, lenbuf, 4);
        write_bytes(clientfd, bytes, len);
        ()
    };
   
    if clientfd == 0 then clientfd := socket_server_ready(id);

    // We are making the ASSUMPTION that real socket descriptors are nonzero.
    if clientfd != 0 then shoot() else

    if nodrop then {
      // Here we simple stall the current thread until data is ready.
      puts_err("  <socket.ws> SPINNING main WS thread to wait for data source connection (server).\n");
      while ( clientfd == 0 ) {
        // The problem with this is that we may be holding other socket_out sources up
        // from calling start_spawn_socket_server:
        usleep(2 * 1000);
        clientfd := socket_server_ready(id);
      };
      shoot();

    } else {
      // Otherwise data gets DROPPED ON THE FLOOR.
    }    

  }
}


// Here we have a problem though... we really can't return to the WS
// scheduler without having produced some data item (if we are a source).
// And we really should be a source.  The way we drove it by a timer
// we can ignore ticks, but this is not correct, because it requires
// picking an arbitrary timer frequency.

// [2009.11.22] TEMPORARY:
// For now we have a blocking socket_in that.

// This will ONLY WORK as long as our WS processes' socket connections
// form a DAG.  There must be source node(s) that have output but no input.
// And interior nodes must not depend upon any of their outputs.

