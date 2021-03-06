includes TokenMachineRuntime;
includes TestMachine;

// [2005.01.12] Adding return_home

// TODO: fix timestamping of messages


// TODO: fix memcpy's to only copy the used portion of the message.



#define TOKBUFFER_LENGTH 10 // Buffer 10 incoming messages. Should be around 320 bytes.

module BasicTMCommM {
  provides {
    interface TMComm[uint8_t id];
    interface StdControl;
    //    interface ReceiveMsg;

    // FIXME: I don't actually want to expose these, but I don't know
    // how to make private commands.
    async command result_t add_msg(TOS_MsgPtr token);     
    async command result_t pop_msg(TOS_MsgPtr dest);
    async command TOS_Msg peek_nth_msg(uint16_t indx);

    command void print_buffer();
    async command int16_t num_tokens(); // The number of tokens in the buffer
   }
  uses {

    // This is the output object produced by my Regiment compiler.
    interface TMModule;

    interface Timer;
    interface ReceiveMsg[uint8_t id];
    interface SendMsg[uint8_t id];
    interface Random;
  }
} implementation {

  bool cached_presence[NUM_TOKS];
  TOS_Msg cached_tokens[NUM_TOKS];
  //  TM_Payload* cached_payload;

  bool    is_processing;         
  TOS_Msg currently_processing; // The incoming token, there should only be one of these


  // This is a FIFO for storing incoming messages, implemented as a wrap-around buffer.
  TOS_Msg token_in_buffer[TOKBUFFER_LENGTH];

  // in_buffer_start is the position of the first element in the fifo.
  int16_t in_buffer_start;
  // in_buffer_end is the position of the last element, or -1 if there are
  int16_t in_buffer_end;     // no elements in the fifo.


  // For the time being we only have one slot in the out buffer:
  TOS_Msg token_out_buffer; //[TOKBUFFER_LENGTH];
  bool send_pending; //[TOKBUFFER_LENGTH];


  // ======================================================================

  task void tokenhandler () {
    uint8_t ind;
    atomic { // Access in_buffer_end/in_buffer_start
      if ( in_buffer_end == -1 ) {
	dbg(DBG_USR1, "TM BasicTMComm: tokenhandler: NO messages available.\n"); 	
      } else {
	dbg(DBG_USR1, "TM BasicTMComm: tokenhandler: message available, calling process_token.\n"); 

	ind = currently_processing.type - 1;

	is_processing = TRUE;
	if ( call pop_msg( &currently_processing ) ) {
	  // Execute the handler:
	  call TMModule.process_token(&currently_processing); // Do nothing with returned pointer.

	  // AUTO CACHING FOR NOW!
	  cached_presence[ind] = TRUE;
	  memcpy(cached_tokens + ind, &currently_processing, sizeof(TOS_Msg));
	  //	  cached_payload = (TM_Payload*)cached_token->data; 
	  
	  is_processing = FALSE;
	}
      }
    }
  }

  // ======================================================================
  // Here's the implementation of the incoming FIFO
  // I really need to go over the buffer management and make sure it's sane.

  // Add a message to the token_in_buffer if there's room available.
  async command result_t add_msg(TOS_MsgPtr token) {    
    result_t res;
    atomic {    
//      if ( (in_buffer_start == 0 && in_buffer_end == (TOKBUFFER_LENGTH - 1)) || 	 
//	   (in_buffer_start != 0 && in_buffer_end == (in_buffer_start - 1)) ) {
      if ( call num_tokens() == TOKBUFFER_LENGTH ) {
	dbg(DBG_USR1, "TM BasicTMComm: cache full; cannot add token %d->%d\n", in_buffer_start, in_buffer_end);
	res = FAIL;
      } else {
	if ( in_buffer_end == -1 ) { in_buffer_end = in_buffer_start; }
	else { in_buffer_end++; }
	//	dbg(DBG_USR1, "TM BasicTMComm: Adding token payload at %d, origin:%d \n", 
	//	    in_buffer_end, payload.origin);	
	
	//memcpy(token_in_buffer + in_buffer_end, &payload, sizeof(TOS_Msg));	
	token_in_buffer[in_buffer_end] = *token;
	res = SUCCESS; 
      }
    }
    return res;
  }

  // Raisse error if there's no payload in the FIFO: 
  async command result_t pop_msg(TOS_MsgPtr dest) {
    atomic {
      if ( in_buffer_end == -1 ) {
	// raise error!
      } else {      
	memcpy(dest,token_in_buffer + in_buffer_start, sizeof(TOS_Msg));
	//dest = token_in_buffer[in_buffer_start];

	// If we're popping the last one, then it's empty after this:
	if (in_buffer_start == in_buffer_end) { 
	  in_buffer_end = -1; 
	  in_buffer_start = 0;
	} else {
	  in_buffer_start++;
	  if (in_buffer_start == TOKBUFFER_LENGTH) { in_buffer_start = 0; }
	}
      }
    }
    return SUCCESS;
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
	indx += in_buffer_start;
	if ( indx >= TOKBUFFER_LENGTH ) { indx -= TOKBUFFER_LENGTH; }	
	//memcpy((&ret_msg),token_in_buffer + indx, sizeof(TOS_Msg));
	ret_msg = token_in_buffer[indx];
      }
    }
    return ret_msg;
  }


  // This is another helper method that abstracts over the obnoxious
  // start/end indices.
  async command int16_t num_tokens(){ 
    int16_t res;
    atomic { // Access in_buffer_end/in_buffer_start
      if (in_buffer_end == -1) { 
	res = 0;
      } else if (in_buffer_end >= in_buffer_start) {
	res = (1 + in_buffer_end - in_buffer_start);
      } else {
	res = (TOKBUFFER_LENGTH + 1 + in_buffer_end - in_buffer_start); // TEST THIS
      }
    }
    return res;
  }
  
  // This is a debugging function.
  command void print_buffer() {
    int16_t i,j;

    atomic { // The token_in_buffer had better stay still while we print it out.

      dbg(DBG_USR1, "TM BasicTMComm: CONTENTS OF BUFFER, #tokens=%d start-end:%d/%d\n", 
	  call num_tokens(), in_buffer_start, in_buffer_end);
	  //	  99, in_buffer_start, in_buffer_end);
      
      for (i=0; i < call num_tokens(); i++) {
	j = i + in_buffer_start;
	if (j >= TOKBUFFER_LENGTH) { j -= TOKBUFFER_LENGTH; }
	
	/*	dbg(DBG_USR1, "TM BasicTMComm:   %d/%d: par:%d orig:%d  time:%d count:%d \n", 
	    i, j, 
	    token_in_buffer[j].origin,    token_in_buffer[j].parent, 
	    token_in_buffer[j].timestamp, token_in_buffer[j].counter);
	*/
      }
    }
  }

  // ======================================================================
  // This accesses the one cached copy of the current token:
  // Might return NULL
  // Modified it to return currently processing if available...
  command TOS_MsgPtr TMComm.get_cached[uint8_t id](uint8_t ind) {
    if ( cached_presence[ind-1] == FALSE ) {
      if ( is_processing &&
	   currently_processing.type == ind )
	return &currently_processing;
      else return NULL;
    } else return cached_tokens + ind - 1;
  }
      
  // I don't think I really want this in the interface:
  command result_t TMComm.set_cached[uint8_t id](TOS_MsgPtr newtok) {
    // This copies the data over.
    //cached_token = *newtok;
    if ( cached_presence[id-1] ) {
      cached_presence[id-1] = TRUE;
      //cached_payload = (TM_Payload*)cached_token->data;
    }
    memcpy(cached_tokens + id - 1, newtok, sizeof(TOS_Msg));
    return SUCCESS;
  }

  // ======================================================================
    // The TMComm interface is what the TMModule expects the
    // communications system to be able to do:

  // [2005.11.09] Removed all gradient functions.

  // ============================================================
  // These don't actually use ID and so are not specific to this token-interface:  


  // ======================================================================
  // Here we obligatorily fill in the rest of our required interfaces.

  command result_t StdControl.init() {
    int i;
    atomic {
      in_buffer_start = 0;
      in_buffer_end = -1;
      is_processing = FALSE;
      for (i=0; i<NUM_TOKS; i++) {
	cached_presence[i] = FALSE;
      }
    }
   
    /*    extra_token2.origin = 91;
    extra_token2.parent = 92;
    extra_token2.timestamp = 93;
    extra_token2.counter = 94;*/

    dbg(DBG_USR1, "TM BasicTMCommM: Initializing, buffersize:%d, tokdatalen:%d  rettoklen:%d\n", 
	TOKBUFFER_LENGTH, TOK_DATA_LENGTH, RETURNTOK_DATA_LENGTH );

    return call Random.init();
  }

  command result_t StdControl.start() {
    return call Timer.start(TIMER_REPEAT, 3000);
  }
  command result_t StdControl.stop() {
    return call Timer.stop();
  }

  event result_t Timer.fired() {
    /* // Add random messages:
    ((TM_Payload*)extra_token2.data)->origin = call Random.rand();
    ((TM_Payload*)extra_token2.data)->parent = call Random.rand();
    ((TM_Payload*)extra_token2.data)->timestamp = call num_tokens();
    ((TM_Payload*)extra_token2.data)->counter = (uint8_t)call Random.rand();
    */
    //    call add_msg(&extra_token2);

    call print_buffer();

    // dbg(DBG_USR1, "TM BasicTMCommM: Timer Fired\n");
    return SUCCESS;
  }

  event result_t SendMsg.sendDone[uint8_t sent_id](TOS_MsgPtr msg, bool success) {
    dbg(DBG_USR1, "TM BasicTMCommM: Done sending type: %d\n", msg->type);
    send_pending = FALSE;
    
    return SUCCESS;
  }
  
  event TOS_MsgPtr ReceiveMsg.receive[uint8_t id](TOS_MsgPtr msg) {
    TM_Payload* payload = (TM_Payload*)(msg->data);
    TM_Payload* cached_payload;

    // If it's a return message we accept automatically.
    if ( AM_RETURNMSG != msg->addr ) {
      // otherwise... Bounce it if it's not a good path.
      if ( cached_presence[id-1] ) {
	cached_payload = (TM_Payload*)(cached_tokens[id-1].data);
	dbg(DBG_USR1, "TM BasicTMComm: receiving message to %d, curNULL posting tokenhandler o%d/p%d/c%d/g%d.\n", 
	    id, payload->origin, payload->parent, payload->counter, payload->generation);
      } else if (	//	payload->generation > 
		 payload->counter < payload->counter) {
	dbg(DBG_USR1, "TM BasicTMComm: receiving message to %d, posting tokenhandler o%d/p%d/c%d/g%d.\n", 
	    id, payload->origin, payload->parent, payload->counter, payload->generation);
      } else {
	dbg(DBG_USR1, "TM BasicTMComm: receiving message to %d, BOUNCED (cur c%d) o%d/p%d/c%d/g%d.\n", 
	    id, cached_payload->counter,
	    payload->origin, payload->parent, payload->counter, payload->generation);
	return msg;
      }
    }

    // Post the token handler.

    // TOUNDO:
    call add_msg(msg);
    post tokenhandler();

    //return signal TMComm.receive[id](msg);
    return msg;
  }

  // This should never fire.  In fact, maybe I should signal an error here.
  //default event TOS_MsgPtr TMComm.receive[uint8_t id](TOS_MsgPtr msg) { return msg; }
}
