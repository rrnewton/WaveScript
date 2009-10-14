#ifndef SERVER_H_
#define SERVER_H_

#include <sys/socket.h>
#include <arpa/inet.h>


#define MutexMapIter   map<int, pthread_mutex_t*>::iterator

class Server : public Thread {
  //class Server {
 private: 
  int _port;
  string _hostname;
  Catalog* _catalog;

  /* socket info */
  int sockfd;                             // server socket descriptor 
  struct sockaddr_in ser_addr, cli_addr;
  fd_set candidate_fds;                   // candidate file descriptors
  fd_set read_fds;
  int fdmax;                              // maximum file descriptor
  char inbuf[BUFTEMP]     ;               // input buffer    

  /* operator id -> client */
  //  multimap<int, int> outputmap;  
  //pthread_mutex_t outputmap_mutex;        // mutex for the whole map  

  /* operator id -> operator pointer */
  
 public:
  Server(int port, string hostname, Catalog* cat) : Thread(), _port(port), 
    _hostname(hostname), _catalog(cat) {
      string threadname = hostname + _to_string(port);
      strcpy(_threadname, threadname.c_str());   

      //string cwthreadname = threadname + "cw";
      //cw = new ClientWriter(cwthreadname, this);
      init();
  }

  /*Server(int port, string hostname) : _port(port), _hostname(hostname) {    
    init();
    }*/
 
  ~Server() {

  };
  

  // server initialization
  void init() {

    // fd_sets initialization
    FD_ZERO(&candidate_fds);
    FD_ZERO(&read_fds);

    // socket
    sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if(sockfd < 0) { 
      perror("ERROR opening socket \n"); exit (1); 
    }
    string s = _hostname + _to_string(_port);
    printf("Server %s socket OK..........\n", s.c_str());

    // allow socket descriptor reusable
    int yes = 1;
    if(setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int)) < 0) {
      perror("ERROR setsockopt. \n"); exit(1);
    }
    printf("Server %s setsockopt OK..........\n", s.c_str());

    // bind
    memset((char*)&ser_addr, 0, sizeof(ser_addr));
    ser_addr.sin_family = AF_INET;
    ser_addr.sin_addr.s_addr = INADDR_ANY;
    ser_addr.sin_port = htons(_port);
    
    if(bind(sockfd, (struct sockaddr *) &ser_addr, 
	    sizeof(ser_addr)) < 0) {
      perror("ERROR on binding.\n"); exit(1);
    }
    printf("Server %s binding OK..........\n", s.c_str());
    
    // listen
    if(listen(sockfd, 15) < 0) {
      perror("ERROR on listening.\n"); exit(1);
    }
    printf("Server %s listening OK..........\n", s.c_str());    
    
    fdmax = sockfd;               
    FD_SET(sockfd, &candidate_fds);    // add the sock descriptor to the fd set 
  }

  void run() {
    int clilen = sizeof(cli_addr);
    string s = _hostname + _to_string(_port);
    
    while(true) {
      read_fds = candidate_fds; // file descriptor list
      if(select(fdmax+1, &read_fds, NULL, NULL, NULL) < 0) {
	perror("ERROR on select. \n"); exit(1);
      }
      printf("Server %s select OK..........\n", s.c_str());     

      // run through the existing connection looking for data to be read    
      for(int i=0; i<=fdmax; i++) {
	if(FD_ISSET(i, &read_fds)) {
	  printf("got read socket descriptor %d..........\n", i);
	  
	  // got one 	  
	  if(i == sockfd) {	    
	    
	    int newsockfd = accept(sockfd, (struct sockaddr*) &cli_addr, 
				   (socklen_t*)&clilen);
	    if(newsockfd < 0)
	      perror("ERROR on accept.\n");
	    printf("Server %s accept OK..........\n", s.c_str());
	    printf("New connection from %s on socket %d..........\n", 
		   inet_ntoa(cli_addr.sin_addr), newsockfd);
	    FD_SET(newsockfd, &candidate_fds);    // add to the candidate fd set
	    
	    if(newsockfd > fdmax) fdmax = newsockfd;
	  }
	  else {
	    /* handle the connection header from the client
	       where to buffer data */
	   
	    memset(&inbuf, 0, sizeof(inbuf));
	    int nbytes;
	    if((nbytes =  recv(i, inbuf, sizeof(inbuf), 0)) <= 0) {
	      if(nbytes == 0) 
		printf("socket %d hung up..........\n", i);
	      else 
		printf("server recv has error from socket: %d \n", i);

	      // close it
	      close(i);
	      FD_CLR(i, &candidate_fds);
	      clientclean(i);
	    }
	    else { // successfully read	   
	      printf("Got Message %s\n", inbuf);
	      parseinput(inbuf, i);	 
	    }  
	  }
	}	
      }   
    }    
  }
    
 private:
  /* message format: divided by ,
   * 1).control type: 1: connect output to this client
   * 2).operator id: indicate which operator's output
   */
  void parseinput(char* buf, int clisock) {
    string controls = string(strtok(buf, ","));
    int control = atoi(controls.c_str());
    switch(control) {
    case 1: {
      string ops = string(strtok(NULL, ","));
      int opid = atoi(ops.c_str());
      //printf("operator id is %d\n", opid);

      _catalog->requireoutputmaplock();
      MapIter iter = _catalog->outputmap.find(opid);
      if(iter == _catalog->outputmap.end()) {
	// nobody else bind to this operator yet before
	// initialize the operator buffer
	_catalog->requireoperatormaplock();
	
	_catalog->releaseoperatormaplock();
      }
      _catalog->outputmap.insert(pair<int, int>(opid, clisock));      
      _catalog->printoutputmap();
      _catalog->releaseoutputmaplock();
      break;
    }      
    default: {     
      printf("control value %d undefined\n", control);
      exit(1);
    }      
    }
  };

  void clientclean(int clisock) {
    // clean the client from this node's outputmap
    _catalog->requireoutputmaplock();
    for(MapIter iter=_catalog->outputmap.begin(); 
	iter!= _catalog->outputmap.end(); iter++) {
      if(iter->second == clisock) {
	_catalog->outputmap.erase(iter);
	break;
      }
    }
    _catalog->printoutputmap();
    _catalog->releaseoutputmaplock();
  }
 

 public:
  int port() { return _port; }
  const char* name() { return _hostname.c_str(); }
  const char* fullname() { 
    string s = _hostname + _to_string(_port);
    return s.c_str();
  }

};
#endif // SERVER_H_
