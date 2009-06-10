
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
fun socket_in_bytes(addr, port) {
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
  rawbytes = socket_in_bytes(addr,port);
  iterate buf in rawbytes {
   // We're on thin ice typing wise:
    ob = unmarshal(buf,0);
    emit ob;
  }
}


//================================================================================


// Here's a simple example of using socket_in and socket_out.
//================================================================================

port = 9000 + Uint16! randomI(1000);
_ = println("Picked port: " ++ port);

nums = iterate n in COUNTUP(10) { emit #[n,n] };

out = socket_out(nums, port);
instrm :: Stream (Array Int) = socket_in("localhost", port);

// We need to merge in & out.. ask me why.
main = merge(out, instrm);
//main = merge(out, (instrm :: Stream (Array Int)));




/*
// connect to www.example.com port 80 (http)

struct addrinfo hints, *res;
int sockfd;

// first, load up address structs with getaddrinfo():

memset(&hints, 0, sizeof hints);
hints.ai_family = AF_UNSPEC;  // use IPv4 or IPv6, whichever
hints.ai_socktype = SOCK_STREAM;

// we could put "80" instead on "http" on the next line:
getaddrinfo("www.example.com", "http", &hints, &res);

// make a socket:

sockfd = socket(res->ai_family, res->ai_socktype, res->ai_protocol);

// connect it to the address and port we passed in to getaddrinfo():

connect(sockfd, res->ai_addr, res->ai_addrlen);
 */

//================================================================================
// Here's a simple example written by hand:

port = 9996;

server = iterate _ in timer(0) {
  using Unix;

  print("Sender starting\n");
  sockfd = socket(Int! AF_INET(), Int! SOCK_STREAM(), 0);
  if sockfd < 0 then error("ERROR opening socket.");  
  println("Sender: Allocated socket " ++ sockfd); 

  serv_addr = make_sockaddr_in(AF_INET(), port, INADDR_ANY());

  print_sockaddr(serv_addr);

  b = bind(sockfd, serv_addr, sizeof_sockaddr());
  if b < 0 then error("ERROR on binding: "++b++ " errno " ++ errno());
  println(" Bound...");

  listen(sockfd,5);
  println(" Listening...");

  // Go ahead and send out the token that will trigger the downstream.
  emit 0;

  // Storage space:  
  cli_addr = make_sockaddr_in(0,0,0);
  clilen = #[sizeof_sockaddr()];
  newsockfd = accept(sockfd, cli_addr, clilen);
    println( " server accepted connection! " ++ newsockfd);

   buf1 = Array:make(255,0);
   // Read the length:
   rd = read_bytes(newsockfd, buf1, 4);   
     assert_eq("read wrong length", rd, 4);
   len :: Int = unmarshal(buf1,0);   
   println(" server read length "++ len);  

   buf2 = Array:make(len, 0);
   rd = read_bytes(newsockfd, buf2, len);
     assert_eq("read wrong length", rd, len);
   println(" server read bytes "++ buf2);  

   ob :: Array String = unmarshal(buf2,0);
   println(" unmarshaled "++ ob);

  exit(1);
}

client = iterate _ in server {
  using Unix;
  print("Receiver starting\n");

  sockfd = socket(Int! AF_INET(), Int! SOCK_STREAM(), 0);
  if sockfd < 0 then error("ERROR opening socket.");  

  server = gethostbyname("localhost");
  if server.ptrIsNull then error("ERROR no such host");
  println("  Client: got host " ++ server); 

  addr = make_sockaddr_in(AF_INET(), port, hostent_h_addr(server));
  println("  Client: got addr " ++ addr);   
  print_sockaddr(addr);

  c = connect(sockfd, addr, sizeof_sockaddr());
  if c < 0 then error("ERROR connecting "++ c);  
  println("  Client: got connection!");   

  //s = "foobarmsg";

  buf = marshal(#["my happy array", "yay"]);

  len = Array:length(buf);  
  lenbuf = marshal(len);  

  println$ "Marshaling "++ len ++ " bytes";
  write_bytes(sockfd, lenbuf, 4);
  write_bytes(sockfd, buf, len);
  
  //write_str(sockfd, s, s.String:length);
  println $ "  Client: msg written";
  
  emit 0;
} 

//main = client

