includes TokenMachineRuntime;

#define TOKCACHE_LENGTH 10 // Buffer 10 incoming messages. Should be around 320 bytes.

module BasicTMCommM {
  provides {
    interface TMComm[uint8_t id];
    interface StdControl;
    //    interface ReceiveMsg;

    // I don't actually want to expose these, but I don't know how to
    // make private commands.
    async command result_t add_msg(TOS_MsgPtr token);     
    async command TOS_Msg pop_msg();
    async command TOS_Msg peek_nth_msg(uint16_t indx);

    command void print_cache();
    async command int16_t num_tokens();
   }
  uses {

    // This is the output module produced by my Regiment compiler.
    interface TMModule;

    interface Timer;
    interface ReceiveMsg[uint8_t id];
    interface SendMsg[uint8_t id];
    interface Random;
  }
} implementation {

  TOS_Msg temp_msg;
  TOS_Msg temp_msg_ptr;

  // This is a FIFO for storing incoming messages, implemented as a wrap-around buffer.
  TOS_Msg token_cache[TOKCACHE_LENGTH];
  // cache_start is the position of the first element in the fifo.
  int16_t cache_start;
  // cache_end is the position of the last element, or -1 if there are
  // no elements in the fifo.
  int16_t cache_end;

  task void tokenhandler () {
    atomic { // Access cache_end/cache_start
      if ( cache_end == -1 ) {
	dbg(DBG_USR1, "TM BasicTMComm: tokenhandler: NO messages available.\n"); 	
      } else {
	temp_msg = call pop_msg();
	//temp_msg_ptr = &(call pop_msg());
	call TMModule.process_token(&temp_msg); // Do nothing with returned pointer.
      }
    }
  }

  // Add a message to the token_cache if there's room available.
  async command result_t add_msg(TOS_MsgPtr token) {    
    result_t res;
    atomic {    
//      if ( (cache_start == 0 && cache_end == (TOKCACHE_LENGTH - 1)) || 	 
//	   (cache_start != 0 && cache_end == (cache_start - 1)) ) {
      if ( call num_tokens() == TOKCACHE_LENGTH ) {
	dbg(DBG_USR1, "TM BasicTMComm: cache full; cannot add token %d->%d\n", cache_start, cache_end);
	res = FAIL;
      } else {
	if ( cache_end == -1 ) { cache_end = cache_start; }
	else { cache_end++; }
	//	dbg(DBG_USR1, "TM BasicTMComm: Adding token payload at %d, origin:%d \n", 
	//	    cache_end, payload.origin);	
	
	//memcpy(token_cache + cache_end, &payload, sizeof(TOS_Msg));	
	token_cache[cache_end] = *token;
	res = SUCCESS; 
      }
    }
    return res;
  }

  // Raisse error if there's no payload in the FIFO: 
  async command TOS_Msg pop_msg() {
    TOS_Msg ret_msg;
    atomic { 
      if ( cache_end == -1 ) {
	// raise error!
      } else {      
	//memcpy((&ret_msg),token_cache + cache_start, sizeof(TOS_Msg));
	ret_msg = token_cache[cache_start];

	// If we're popping the last one, then it's empty after this:
	if (cache_start == cache_end) { 
	  cache_end = -1; 
	  cache_start = 0;
	} else {
	  cache_start++;
	  if (cache_start == TOKCACHE_LENGTH) { cache_start = 0; }
	}
      }
    }
    return ret_msg;
  }
  
  // This is for abstracting over the annoying wrap around buffer and
  // accessing the nth element in the FIFO, where 0 is the next item
  // to be popped.
  async command TOS_Msg peek_nth_msg(uint16_t indx) {
    TOS_Msg ret_msg;
    atomic { 
      if ( indx >= (call num_tokens()) ) {
	// raise error!
      } else {
	indx += cache_start;
	if ( indx >= TOKCACHE_LENGTH ) { indx -= TOKCACHE_LENGTH; }	
	//memcpy((&ret_msg),token_cache + indx, sizeof(TOS_Msg));
	ret_msg = token_cache[indx];
      }
    }
    return ret_msg;
  }


  // This is another helper method that abstracts over the obnoxious
  // start/end indices.
  async command int16_t num_tokens(){ 
    int16_t res;
    atomic { // Access cache_end/cache_start
      if (cache_end == -1) { 
	res = 0;
      } else if (cache_end >= cache_start) {
	res = (1 + cache_end - cache_start);
      } else {
	res = (TOKCACHE_LENGTH + 1 + cache_end - cache_start); // TEST THIS
      }
    }
    return res;
  }
  
  // This is a debugging function.
  command void print_cache() {
    int16_t i,j;

    atomic { // The token_cache had better stay still while we print it out.

      dbg(DBG_USR1, "TM BasicTMComm: CONTENTS OF CACHE, #tokens=%d start-end:%d/%d\n", 
	  call num_tokens(), cache_start, cache_end);
	  //	  99, cache_start, cache_end);
      
      for (i=0; i < call num_tokens(); i++) {
	j = i + cache_start;
	if (j >= TOKCACHE_LENGTH) { j -= TOKCACHE_LENGTH; }
	
	/*	dbg(DBG_USR1, "TM BasicTMComm:   %d/%d: par:%d orig:%d  time:%d count:%d \n", 
	    i, j, 
	    token_cache[j].origin,    token_cache[j].parent, 
	    token_cache[j].timestamp, token_cache[j].counter);
	*/
      }
    }
  }

  command result_t StdControl.init() {
    atomic {
      cache_start = 0;
      cache_end = -1;
    }
   
    /*    temp_msg.origin = 91;
    temp_msg.parent = 92;
    temp_msg.timestamp = 93;
    temp_msg.counter = 94;*/

    dbg(DBG_USR1, "TM BasicTMCommM: Initializing, buffersize: %d\n", TOKCACHE_LENGTH);

    return call Random.init();
  }

  command result_t StdControl.start() {
    return call Timer.start(TIMER_REPEAT, 3000);
  }
  command result_t StdControl.stop() {
    return call Timer.stop();
  }

  // Hope this gets statically wired and inlined 
  command result_t TMComm.emit[uint8_t id](uint16_t address, uint8_t length, TOS_MsgPtr msg) {    

    //    call SendMsg.send[msg->type](address, length, msg); 
    call SendMsg.send[id](address, length, msg); 
    return SUCCESS;
  }

  command result_t TMComm.relay[uint8_t id](uint16_t address, uint8_t length, TOS_MsgPtr msg) {
    return SUCCESS;
  }

  command result_t TMComm.return_home[uint8_t id](uint16_t address, uint8_t length, TOS_MsgPtr msg) {
    return SUCCESS;
  }

  event result_t Timer.fired() {

    ((TM_Payload*)temp_msg.data)->origin = call Random.rand();
    ((TM_Payload*)temp_msg.data)->parent = call Random.rand();
    ((TM_Payload*)temp_msg.data)->timestamp = call num_tokens();
    ((TM_Payload*)temp_msg.data)->counter = (uint8_t)call Random.rand();

    call add_msg(&temp_msg);

    call print_cache();

    // dbg(DBG_USR1, "TM BasicTMCommM: Timer Fired\n");
    return SUCCESS;
  }

  event result_t SendMsg.sendDone[uint8_t sent_id](TOS_MsgPtr msg, bool success) {
    // dbg(DBG_USR1, "TM BasicTMCommM: Done sending type: %d\n", msg->type);
    return SUCCESS;
  }
  
  event TOS_MsgPtr ReceiveMsg.receive[uint8_t id](TOS_MsgPtr msg) {
    // Post the token handler.
    call add_msg(msg);
    post tokenhandler();
    //return signal TMComm.receive[id](msg);
    return msg;
  }

  // This should never fire.  In fact, maybe I should signal an error here.
  //default event TOS_MsgPtr TMComm.receive[uint8_t id](TOS_MsgPtr msg) { return msg; }
}
