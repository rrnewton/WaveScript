#ifndef CLIENT_H_
#define CLIENT_H_

#include "ESBox.h"
#include "Server.h"

class Client : public ESBox, public Thread {

 private:
  int _port;
  string _ip;

  // socket info
  int sockfd;
  struct sockaddr_in  ser_addr;

  ESBuffer* _buffer;    // internal buffer, like Source
  char* _values;
  size_t sz;
  size_t offset;

  int count;

 public:
  char inbuf[BUFTEMP];
  char outbuf[BUFTEMP];
  
 public:
  Client(int port, string ip, string msg, 
	 int index, EventType* etype) 
    : ESBox(index, CLIENT, etype, etype), Thread(), _port(port), _ip(ip) {
    strcpy(outbuf, msg.c_str());
    string threadname = "client-" + _to_string(index);
    strcpy(_threadname, threadname.c_str());
    
    _buffer = new ESBuffer();
    sz = etype->bytes();
    offset = 0;

    _values = (char*)malloc(sz);

    count = 0;
    
    init();
  }

  ~Client() { delete _buffer; free(_values); }

  // client initialization
  void init() {

    // socket
    sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if(sockfd < 0) {
      perror("ERROR opening socket. \n"); exit(2);
    }

    // clean the ser_addr
    memset((char*)&ser_addr, 0, sizeof(ser_addr));
    ser_addr.sin_family = AF_INET;
    ser_addr.sin_port = htons(_port);
    ser_addr.sin_addr.s_addr = inet_addr(_ip.c_str());    
    
    // connect to the server
    if(connect(sockfd, (sockaddr*)&ser_addr, sizeof(ser_addr)) < 0) {
      perror("ERROR connecting. \n"); exit(2);    
    }
    
    // strcpy(outbuf,"~~~~~~connecting to the server~~~~~~~\n");
    
    if(send(sockfd, outbuf, BUFTEMP, 0) < 0) {
      perror("send() error!\n"); exit(2);
    }
  }

  void run() {
    // writing data from socket to operator buffer
    
    int nbytes;
    size_t bytes = 0;
    while(true) {
      usleep(10);
      memset(&inbuf, 0, BUFTEMP); 
      if((nbytes =  recv(sockfd, inbuf, BUFTEMP, 0)) <= 0) {
	if(nbytes == 0) 
	  printf("socket %d hung up..........\n", sockfd);
	else 
	  printf("server recv has error from socket: %d \n", sockfd);	
	break;
      }
      else {  // successfully read
	//printf("Got Message %d bytes: %s\n", nbytes, inbuf);
	bytes = 0;
	while(nbytes - bytes + offset >= sz) {
	  memcpy(_values+offset, inbuf+bytes, sz);
	  EventPtr ep = new Event(_values, sz);
	  _buffer->push_back(ep);
	  ep->print(cout, _inetptr);
	  count++;
	  bytes += sz - offset;
	  offset = 0;	  
	}
	offset = nbytes - bytes;
	if(offset > 0) memcpy(_values, inbuf+bytes, offset);	
      }      
    }
    cout << count << endl;      
    
  }


};
#endif // CLIENT_H_
