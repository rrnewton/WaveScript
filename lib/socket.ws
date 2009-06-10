
// [2009.06.02] A socket library.

// First, we extend the Unix namespace with these basic wrappers.

include "stdlib.ws"
include "unix.ws"

namespace Unix {
  
  socket_includes = ["sys/types.h", "sys/socket.h", "netinet/in.h", "socket_wrappers.c"]

  socket  :: (Int,Int,Int) -> Int                          = foreign("socket", socket_includes);
  connect :: (Int, Pointer "struct sockaddr*", Int) -> Int = foreign("connect", socket_includes);
  bind    :: (Int, Pointer "struct sockaddr*", Int) -> Int = foreign("bind", socket_includes)
  listen  :: (Int, Int) -> Int                             = foreign("listen", socket_includes);
  accept  :: (Int, Pointer "struct sockaddr*", Array Int) -> Int = foreign("accept", socket_includes);
  
  gethostbyname :: String -> Pointer "struct hostent*"  = foreign("gethostbyname", socket_includes);
  hostent_h_addr :: Pointer "struct hostent*"  -> Int   = foreign("ws_hostent_h_addr", socket_includes);

  make_sockaddr_in :: (Int16, Uint16, Int) -> Pointer "struct sockaddr_in*"
    = foreign("ws_make_sockaddr_in", socket_includes);

  AF_INET     :: () -> Int16  = foreign("ws_AF_INET", socket_includes);
  SOCK_STREAM :: () -> Uint16 = foreign("ws_SOCK_STREAM", socket_includes);
  INADDR_ANY  :: () -> Int    = foreign("ws_INADDR_ANY", socket_includes);

  errno  :: () -> Int = foreign("ws_errno", socket_includes);

  // These wrap enum values.
  // These don't work yet... but it shouldn't be too hard to make them.
  //AF_INET     :: Int16 = foreign("AF_INET", socket_includes);
  //SOCK_STREAM :: Uint16 = foreign("SOCK_STREAM", socket_includes);

  sizeof_sockaddr :: () -> Int = foreign("ws_sizeof_sockaddr", socket_includes);
  print_sockaddr  :: (Pointer "struct sockaddr*") -> () = foreign("ws_print_sockaddr", socket_includes);
}

//================================================================================

// High level interface for socket communication.

// The convention here is that out-bound sockets are the server.
fun socket_out(strm, port) {
  iterate x in strm {
    state { first = true; 
            clientfd = 0;
          }
    using Unix;
    if first then {
      first := false;
      sockfd = socket(Int! AF_INET(), Int! SOCK_STREAM(), 0);
      if sockfd < 0 then error("socket_out: ERROR opening socket.");
      serv_addr = make_sockaddr_in(AF_INET(), port, INADDR_ANY());
      b = bind(sockfd, serv_addr, sizeof_sockaddr());
      if b < 0 then error("socket_out: ERROR on binding: "++b++ " errno " ++ errno());
      listen(sockfd,5);      
      // Accept client connection:
      cli_addr = make_sockaddr_in(0,0,0);
      clilen = #[sizeof_sockaddr()];
      clientfd := accept(sockfd, cli_addr, clilen);
    };

    // Marshal and send it:
    bytes = marshal(x);    
    len = Array:length(bytes);  
    lenbuf = marshal(len);  
    write_bytes(clientfd, lenbuf, 4);
    write_bytes(clientfd, bytes, len);
  }
}

// Returns a stream of byte arrays.
fun socket_in_raw(addr, port) {
  // FIXME: this really should not be driven by a timer.  It should be a foreign_source:
  // Either that or it should run in an infinite loop.
  iterate _ in timer(500) {
    state { first = true;
            sockfd = 0;
	    tempbuf = Array:make(4,0);
          }
    using Unix;
    if first then {
      first := false;
      sockfd := socket(Int! AF_INET(), Int! SOCK_STREAM(), 0);
      if sockfd < 0 then error("socket_in: ERROR opening socket.");  
      server = gethostbyname(addr);
      if server.ptrIsNull then error("socket_in: ERROR no such host");
      addr = make_sockaddr_in(AF_INET(), port, hostent_h_addr(server));
      // May block for some time:
      c = connect(sockfd, addr, sizeof_sockaddr());
      if c < 0 then error("ERROR connecting "++ c);
    };

   rd = read_bytes(sockfd, tempbuf, 4);  // Read length.
      assert_eq("read wrong length", rd, 4);
   len :: Int = unmarshal(tempbuf,0);

   buf = Array:make(len, 0);
   rd = read_bytes(sockfd, buf, len);
     assert_eq("read wrong length", rd, len);

   //ob :: Array String = unmarshal(buf2,0);  
   // We're on thin ice typing wise:
   //ob = unmarshal(buf,0);
   //emit ob;

   // Emit the raw bytes.
   emit buf;
  }
}

fun socket_in(addr, port) {
  rawbytes = socket_in_raw(addr,port);
  iterate buf in rawbytes {
   // We're on thin ice typing wise:
    ob = unmarshal(buf,0);
    emit ob;
  }
}


//================================================================================

