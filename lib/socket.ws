
// TODO -- set buffer size on TCP stack....  


// [2009.06.02] A socket library.

// First, we extend the Unix namespace with these basic wrappers.

include "stdlib.ws"
include "unix.ws"

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
  
  gethostbyname  :: String -> Pointer "struct hostent*" = foreign("gethostbyname", socket_pthread_includes);
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

socket_in     :: (String, Uint16) -> Stream a;
socket_in_raw :: (String, Uint16) -> Stream (Array Uint8);

// Returns a stream of byte arrays.
fun socket_in_raw( address, port) {
  // Rather than driving this by a foreign_source or by an infinite loop inside a single 
  // kernel invocation, we use the convention that a timer with a negative rate will run
  // as fast as psosible.  (At least as fast as any real timer rates in the system.)
  iterate _ in timer(-1.0) 
  {
    state { first = true;
            connected = false;
            sockfd = 0;
            tempbuf = Array:make(4,0);
            sockaddr = ptrMakeNull();
          }
    using Unix;
    if first then {
      first := false;
      sockfd := socket(Int! AF_INET(), Int! SOCK_STREAM(), 0);
      if sockfd < 0 then error("socket_in: ERROR opening socket.");  
      server = gethostbyname( address );
      if server.ptrIsNull then error("socket_in: ERROR no such host");
      sockaddr := make_sockaddr_in(AF_INET(), port, hostent_h_addr(server));
    };

    // We do not block or spin in the connection phase.  We just return without producing
    // anything and give other sources a chance to go.  Therefore a program consisting of
    // all socket_in sources will poll them round robin.
    if (not(connected)) then {
      c = connect(sockfd, sockaddr, sizeof_sockaddr());
      code = get_errno();
      if c < 0 then puts_err("  <socket.ws> WARNING: connect returned error code "++ code ++", retrying.\n")
      else { 
        connected := true;
        puts_err("  <socket.ws> Established client connection, port " ++ port ++ "\n");
      }
    };

    fun check_read_result(msg, result, expected) {
        if (result == -1) then {
           code = get_errno();
           error("  <socket.ws> ERROR: read() returned errno "++ code ++ "\n");
        };
        assert_eq(msg++" read wrong length", result, expected);
    };

    if (connected) then 
    {
       // TODO: make this non-blocking:
       // If nothing is available we can just poll other input sources.

       rd = read_bytes(sockfd, tempbuf, 4);  // Read length.
       check_read_result("rd header:", rd, 4);
       len :: Int = unmarshal(tempbuf,0);

       buf = Array:make(len, 0);
       rd = read_bytes(sockfd, buf, len);
       check_read_result("rd payload:", rd, len);

       // Emit the raw bytes.
       emit buf;
    }
    // Otherwise fizzle.
   }
}

fun socket_in(address, port) {
  rawbytes = socket_in_raw(address,port);
  iterate buf in rawbytes {
   // We are on thin ice typing wise:
    ob = unmarshal(buf,0);
    emit ob;
  }
}


//----------------------------------------

/* [2009.10.20] socket_out Version 2 

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
spawn_socket_server_helper  :: Uint16 -> Int64 = foreign("spawn_socket_server_helper", socket_pthread_includes)
poll_socket_server_ready    :: Int64  -> Int   = foreign("poll_socket_server_ready",   socket_pthread_includes)

// This makes a blocking call to connect to a server.  We need to put
// this on its own thread as well to avoid deadlock.  (Case in point:
// imagine a program that connects to its own socket with a
// single-threaded scheduler.  )
socket_out :: (Stream a, Uint16) -> Stream b;

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
      id    := spawn_socket_server_helper(port);
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
   
    if clientfd == 0 then clientfd := poll_socket_server_ready(id);

    // We are making the ASSUMPTION that real socket descriptors are nonzero.
    if clientfd != 0 then shoot() else

    if nodrop then {
      // Here we simple stall the current thread until data is ready.
      puts_err("  <socket.ws> SPINNING main WS thread to wait for data source connection (server).\n");
      while ( clientfd == 0 ) {
        // The problem with this is that we may be holding other socket_out sources up
        // from calling start_spawn_socket_server:
        usleep(2 * 1000);
        clientfd := poll_socket_server_ready(id);
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

