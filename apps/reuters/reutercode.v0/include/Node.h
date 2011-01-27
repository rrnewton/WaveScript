#ifndef NODE_H_
#define NODE_H_

#include <map>
#include <vector>
#include "ESBox.h"
#include "Thread.h"
#include "EventStream.h"
#include "QueryEngine.h"
#include "Server.h"
#include "ClientWriter.h"
#include "Client.h"


class Node {

 private:
  Catalog* _catalog;  
  QueryEngine* _qengine;
  Server* _server;
  ClientWriter* _cw;      // writing corresponding data to those clients   
  vector<Client*> _clientvec;

  string _nodename;

 public:
  // not allowed !!
  Node(string name) : _nodename(name) { _server = 0;}

  Node(int serverport, string name, ESBox* root) {
    _catalog = new Catalog();
    _qengine = new QueryEngine(name, root, _catalog);
    //_qengine = 0;
    _server = new Server(serverport, name, _catalog);
    //_server = 0;   
    
    string cwn = name + _to_string(serverport) + "_cw";
    _cw = new ClientWriter(cwn, _catalog);
    //_cw = 0; 
    _nodename = name;
  }   

  ~Node() {
    if(_catalog != 0) delete _catalog; 
    if(_qengine != 0) delete _qengine;
    if(_server != 0) delete _server;
    if(_cw != 0) delete _cw;
    
    /*for(vector<Client*>::iterator iter = _clientvec.begin(); 
	iter != _clientvec.end(); iter++) {
      if(*iter != NULL)
	delete (*iter);
	}*/
    _clientvec.clear();
  }

  void start() {
    // start the server
    if (_server != 0) {
      _server->start();    
      //_server->join();
    }
    
    // start the client to request data
    for(vector<Client*>::iterator iter = _clientvec.begin();
	iter != _clientvec.end(); iter++) {
      (*iter)->start();
    }

    // start the writer
    if (_cw != 0) {
      _cw->start();
      // _cw->join();
    }

    // start the query processor
    if(_qengine != 0) {
      _qengine->run();
      //      _qengine->join();
    }

    long sum = 0;
    for(long i=0; i<1000; i++)
      sum += i*(i+1000);

    printf("sum = %ld\n", sum);      
  }

  void close() {};
  
  Client* addclient(int port, string ip, string msg, 
		    int index, EventType* etype) {
    Client* c = new Client(port, ip, msg, index, etype);
    _clientvec.push_back(c);
    return c;
  } 

  void addclient(Client* c) { _clientvec.push_back(c); }
  Catalog* catalog() { return _catalog; }
  
};

#endif
