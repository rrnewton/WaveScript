
// [2009.06.02] A socket library.
// [2011.05.20] Trying a new version where we do some global initialization.

include "stdlib.ws"
include "unix.ws"

//================================================================================
// First, we extend the Unix namespace with these basic wrappers.

socket_pthread_includes = 
  ["sys/types.h", "sys/socket.h", "netinet/in.h", "socket_wrappers.c", 
   "pthread.h", "socket_wrappers.c", "libpthread.so"]

// We add some calls to the Unix namespace:
namespace Unix {
  //  socket_includes = ["sys/types.h", "sys/socket.h", "netinet/in.h", "socket_wrappers.c"]

  socket  :: (Int,Int,Int) -> Int                          = foreign("socket", socket_pthread_includes);
  connect :: (Int, Pointer "struct sockaddr*", Int) -> Int = foreign("connect", socket_pthread_includes);
  bind    :: (Int, Pointer "struct sockaddr*", Int) -> Int = foreign("bind", socket_pthread_includes)
  //listen  :: (Int, Int) -> Int                             = foreign("listen", socket_pthread_includes);
  //accept  :: (Int, Pointer "struct sockaddr*", Array Int) -> Int = foreign("accept", socket_pthread_includes);

  ewouldblock :: () -> Int = foreign("ws_EWOULDBLOCK", socket_pthread_includes);
  errno       :: () -> Int = foreign("ws_errno", socket_pthread_includes);

  sizeof_sockaddr :: () -> Int = foreign("ws_sizeof_sockaddr", socket_pthread_includes);
  print_sockaddr  :: (Pointer "struct sockaddr*") -> () = foreign("ws_print_sockaddr", socket_pthread_includes);
}

// create a separate thread to wait for the client/server to connect successfully.
spawn_socket_server_helper    :: Uint16 -> Int64 = foreign("spawn_socket_server_helper", socket_pthread_includes)
spawn_socket_client_helper    :: (String, Uint16) -> Int64 = foreign("spawn_socket_client_helper", socket_pthread_includes)
poll_socket_server_ready_port :: Uint16 -> Int   = foreign("poll_socket_server_ready_port",  socket_pthread_includes)
poll_socket_client_ready_port :: Uint16 -> Int   = foreign("poll_socket_client_ready_port",  socket_pthread_includes)

shutdown_sockets              :: ()     -> ()    = foreign("shutdown_sockets", socket_pthread_includes)

//================================================================================

// High level interface for socket communication.

socket_in     :: (String, Uint16) -> (Stream a);
socket_in_raw :: (String, Uint16) -> (Stream (Array Uint8));

dbgINSOCK = false;

// Returns a stream of byte arrays.
fun socket_in_raw( address, port) {
  // Initialization code.  Fork a thread to attempt to connect.
  // This runs once at the begining of time and before normal timers begin.  That is the
  // convention for "zero rate timers".
  init = iterate _ in timer(0) {
     spawn_socket_client_helper(address, port);
  };

  len_size = 4; // Constant: size in bytes of length header.

  // Rather than driving this by a foreign_source or by an infinite loop inside a single 
  // kernel invocation, we use the convention that a timer with a negative rate will run
  // as fast as possible, irrespective of whether -realtime mode is used.  This should be
  // least as fast as any other timer in the system.
  s = iterate _ in timer(-1.0) 
  {
    state { 
            connected = false;
            sockfd = 0;
            lenbuf  = Array:make(4,0);   // For reading the length field.
            lenctr  = 0;                 // Leftover bytes in the length buf.
            //	    databuf = Array:make(256,0); // For reading the actual messages.
            databuf = Array:null; // For reading the actual messages.
            datactr = 0;                 // Leftover bytes in the data buf.
            wouldblock = 0;

            // Ugliness to handle partial states.  For example, we have read the length
            // header successfully but not the payload.
            curstate = 0;  
            // States: 
            //  0 -> In the process of reading length field.
            //  1 -> In the process of reading data field.
            msglen = 0; // Store the unmarshaled value of the length if we have it.
          }
    using Unix;

    fun check_read_result(msg, result) {
        if (result == -1) then {
           code = get_errno();
           if code == wouldblock
//           then false // { puts_err(" ... would have blocked ... \n"); false }
           then (if dbgINSOCK then puts_err(" ... would have blocked ... \n"))
           else error("  <socket.ws> ERROR: read() returned errno "++ code ++ "\n");
        } else {
/* 	  if result == expected then true */
/* 	  else { */
/* 	    if not(warned) then { */
/*              // print(msg++" WARNING read wrong length, FIXME socket_in should tolerate partial reads.  Dropping data for now.\n"); */
/*              // TEMP: For now we will use this as an exit mechanism. */
/*              puts_err("  <socket.ws> "++msg++" Read wrong length, taking this as a signal that upstream is closed.  Exiting.\n"); */
/*              shutdown_sockets(); */
/*              wsexit(0); */
/*         }; */
//  	    warned := true;
	  }

    };    


    if sockfd == 0 then {
      sockfd := poll_socket_client_ready_port(port);
      if sockfd != 0 then { 
        connected := true;
        wouldblock := ewouldblock(); // Lame.
      }
    };

    // We do not block or spin in the connection phase.  We just return without producing
    // anything and give other sources a chance to go.  Therefore a program consisting of
    // all socket_in sources will poll them round robin.
    if (connected) then 
    {

if dbgINSOCK then print("Connected!\n");
       // TODO: make this non-blocking:
       // If nothing is available we can just poll other input sources.
      if (curstate == 0) then {
         // State 0: Reading length header:

if dbgINSOCK then print("  In state 0\n");

         remaining = len_size - lenctr;
         rd = read_bytes_offset(sockfd, lenbuf, lenctr, remaining );  // Read length.
             check_read_result("rd header:", rd);

if dbgINSOCK then print("    rd "++ rd ++" of "++ remaining ++"\n");

         if (rd == remaining) then {
            tmp :: Int = unmarshal(lenbuf,0); // But this fixes it.
            curstate := 1; // Completed, move to reading data.
            msglen  := tmp;

            // TODO: if msglen = -999 then that signifies an End-of-Stream

            lenctr  := 0;
            datactr := 0;
            databuf := Array:makeUNSAFE(msglen);
    /*             curlen  = Array:length(databuf); */
    /*             if (curlen < msglen) then  */
    /* 	      databuf := Array:makeUNSAFE(max(msglen, 2*curlen)); */

if dbgINSOCK then print("  Entering state 1 ... len = "++ msglen ++"\n");

         } else if (rd == 0) then {
           // Do nothing, stay in current state.
         } else {
            // Incomplete read.
            lenctr   := lenctr + max(rd,0);
         }

      // References are second class which makes it annoying to factor out this duplicated code:
      // ----------------------------------------
      } 
      else 
      // if (curstate == 1)
      { 
         // State 1: Reading data payload:

if dbgINSOCK then print("  In state 1... \n");

         remaining = msglen - datactr;
         rd = read_bytes_offset(sockfd, databuf, datactr, remaining ); // Read message.
             check_read_result("rd payload:", rd);

if dbgINSOCK then print("    rd "++ rd ++" of "++ remaining ++"\n");

         if (rd == remaining) then {
            emit databuf;
            databuf := Array:null;
            curstate := 0; // Completed, move back to reading length header.
            lenctr  := 0;
            datactr := 0;
            msglen  := 0;

         } else if (rd == 0) then {
           // Do nothing, stay in current state.
         } else {
            // Incomplete read.
            datactr := datactr + max(rd,0);
         }
      }
     }
  }; // End iterate

   // We merge the empty initialization stream into the result.
   // Thus it is included in the final program if the result stream is used, which is
   // approrpiate.
   merge(init,s)
} // End socket_in_raw

fun socket_in(address, port) {
  let rawbytes = socket_in_raw(address,port);
  iterate buf in rawbytes {
   // We are on thin ice typing wise:
    ob = unmarshal(buf,0);
    emit ob;
  };
}


//================================================================================

/* [2009.10.20] 

   We can avoid blocking to connect.  We simply have to fork an additional pthread that
   does the blocking for us.  Without changing the WS compiler it's possible for us to use
   the foreign interface to create our own separate subsystem that establishes all the
   connections.

   That's fine if we are ok with data dumping on the floor while we wait for connections
   to be made.  (Which in turn requires that programs be robust against arbitrary
   *skew*.).  The difficulty is if we *want* the whole system to block (backpressure).  We
   can't just block within an individual kernel, because we don't know the threading
   structure of the backend we're in.  For this to work I think we'll need to expose a WS
   call that this socket library can use to quiesce the system.  That in turn will require
   support in all the backends that support the FFI.

   (How would I implement that on a future TBB backend where I don't control the
   scheduler?)


   [2011.05.29] For now we are using a backpressure strategy that ONLY WORKS in single
   threaded mode.  We just block the main thread with a slow spin/sleep wait.

*/

// This makes a blocking call to connect to a server.  We need to put
// this on its own thread as well to avoid deadlock.  (Case in point:
// imagine a program that connects to its own socket with a
// single-threaded scheduler.  )

socket_out :: (Stream a, Uint16) -> Stream b; // Returns empty stream.
fun socket_out(strm, port) {
  init = iterate _ in timer(0) {
     // The problem with doing initialization in a separate thread is that we need to
     // communicate the ID of the socket back through shared memory.
     // Currently this is done through a global table in socket_wrappers.c
     spawn_socket_server_helper(port);
  };
  // The thread will return when it has made its connection.
  s = iterate x in strm {
    state { 
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

    // Poll to see if the connection has been made yet.
    if clientfd == 0 then clientfd := poll_socket_server_ready_port(port);

    // We are making the ASSUMPTION that real socket descriptors are nonzero.
    if clientfd != 0 then shoot() else

    if nodrop then {
      // Here we simple stall the current thread until data is ready.
      puts_err("  <socket.ws> outbound: SPINNING main WS thread to wait for connection on port "++port++" (server).\n");
      while ( clientfd == 0 ) {
        // The problem with this is that we may be holding other socket_out sources up
        // from calling spawn_socket_server_helper:
        // usleep(2 * 1000);
        usleep(100 * 1000);
        clientfd := poll_socket_server_ready_port(port);
      };
      puts_err("  <socket.ws> outbound: Done SPINNING, client connected on port "++port++".\n");
      shoot();

    } else {
      // Otherwise dropping is allowed and data gets DROPPED ON THE FLOOR.
    }

  };
  // Merge both empty streams:
  merge(init,s)
}
