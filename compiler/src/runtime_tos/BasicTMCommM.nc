includes TokenMachineRuntime;


// [2005.01.12] Adding return_home

// TODO: fix timestamping of messages


// TODO: fix memcpy's to only copy the used portion of the message.




#define TOKBUFFER_LENGTH 10 // Buffer 10 incoming messages. Should be around 320 bytes.

module BasicTMCommM {
  provides {
    interface TMComm[uint8_t id];
    interface StdControl;
    //    interface ReceiveMsg;

    // I don't actually want to expose these, but I don't know how to
    // make private commands.
    async command result_t add_msg(TOS_MsgPtr token);     
    async command result_t pop_msg(TOS_MsgPtr dest);
    async command TOS_Msg peek_nth_msg(uint16_t indx);

    command void print_buffer();
    async command int16_t num_tokens();
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

  // This is a lame way of doing an option type: "Maybe Token"
  TOS_Msg extra_token; // We preallocate this one piece of memory.
  TOS_MsgPtr cached_token; // But we're going to start the pointer of NULL.
  TM_Payload* cached_payload;
  
  TOS_MsgPtr currently_processing; // The incoming token
  TOS_Msg extra_token2;            // An allocated buffer for the incoming token

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
    atomic { // Access in_buffer_end/in_buffer_start
      if ( in_buffer_end == -1 ) {
	dbg(DBG_USR1, "TM BasicTMComm: tokenhandler: NO messages available.\n"); 	
      } else {
	dbg(DBG_USR1, "TM BasicTMComm: tokenhandler: message available, calling process_token.\n"); 
	if ( call pop_msg(&extra_token2) ) {	  
	  currently_processing = &extra_token2;
	  // Execute the handler:
	  call TMModule.process_token(&extra_token2); // Do nothing with returned pointer.
	  // AUTO CACHING FOR NOW!
	  cached_token = &extra_token;
	  cached_payload = (TM_Payload*)cached_token->data; 
	  memcpy(cached_token, currently_processing, sizeof(TOS_Msg));
	  
	  currently_processing = NULL;
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
  command TOS_MsgPtr TMComm.get_cached[uint8_t id]() {
    return cached_token;
  }

  // I don't think I really want this in the interface:
  command result_t TMComm.set_cached[uint8_t id](TOS_MsgPtr newtok) {
    // This copies the data over.
    //cached_token = *newtok;
    if ( cached_token == NULL ) {
      cached_token = &extra_token;
      cached_payload = (TM_Payload*)cached_token->data;
    }
    memcpy(&cached_token, newtok, sizeof(TOS_Msg));
    return SUCCESS;
  }


  // ======================================================================
    // The TMComm interface is what the TMModule expects the
    // communications system to be able to do:

  command uint16_t TMComm.get_dist[uint8_t id]() {
    if ( NULL == currently_processing ) {
      if ( cached_token == NULL ) {
	return NO_DIST;
      } else {	
	return (cached_payload->counter);
      }
    } else {
      return ((TM_Payload*)currently_processing->data)->counter;
    }
  }

  // Hope this gets statically wired and inlined 
  // ID and address should be the same...
  command result_t TMComm.emit[uint8_t id](uint16_t address, uint8_t length, TOS_MsgPtr msg) {
    TM_Payload* payload;
   
    if (send_pending) {
      return FAIL;
    } else {
      send_pending = TRUE;
      // OPTIMIZE THIS:
      memcpy(&token_out_buffer, msg, sizeof(TOS_Msg));

      payload = (TM_Payload*)token_out_buffer.data;

      // Since this is an emisson, set the origin to *US*.
      payload->origin = TOS_LOCAL_ADDRESS;
      payload->parent = TOS_LOCAL_ADDRESS;
      payload->timestamp = 999; // TODO FIXME
      payload->generation = 1001; // TODO FIXME
      payload->counter = 1; // The nodes to receive this are hopcount 1.

      token_out_buffer.type = id;      
      token_out_buffer.length = length;

      dbg(DBG_USR1, "TM BasicTMCommM: Emitting message of length:%d, orig:%d, type:%d address:%d/%d\n", 
	  length, TOS_LOCAL_ADDRESS, msg->type, id);
      return call SendMsg.send[id](address, length, &token_out_buffer);
    }
  }

  //  command result_t TMComm.relay[uint8_t id](uint16_t address, uint8_t length, TOS_MsgPtr msg) {
  command result_t TMComm.relay[uint8_t id]() {
    TM_Payload* payload;

    if ( NULL == currently_processing ) 
      return FAIL;

    if (send_pending) {
      return FAIL;
    } else {
      send_pending = TRUE;
      memcpy(&token_out_buffer, currently_processing, sizeof(TOS_Msg));
      payload = (TM_Payload*)token_out_buffer.data;

      dbg(DBG_USR1, "TM BasicTMCommM: Relaying message of length:%d, orig:%d, par:%d, type:%d/%d\n", 
	currently_processing->length, payload->origin, payload->parent, currently_processing->type, id);

      payload->parent = TOS_LOCAL_ADDRESS;
      //payload->timestamp = 999; // TODO FIXME
      payload->counter++; // Increment the hopcount!
      token_out_buffer.type = id;      
      return call SendMsg.send[id](TOS_BCAST_ADDR, sizeof(uint16_t) * token_out_buffer.length, &token_out_buffer);
    }
  }

  // ============================================================
  // These don't actually use ID and so are not specific to this token-interface:
  
  //  command void process_return(TM_ReturnPayload* msg) {   }

  // This essentially ignores "id" because it sends along the 'via' channel.
  command result_t TMComm.return_home[uint8_t id](uint8_t length, TOS_MsgPtr msg) {
    TM_ReturnPayload* ret;
    if (send_pending) {
      dbg(DBG_USR1, "TM BasicTMCommM: return_home failed because send in progress.\n");
      return FAIL;
    } else {
      send_pending = TRUE;

      // OPTIMIZE THIS:
      memcpy(&token_out_buffer, msg, sizeof(TOS_Msg));

      ret = (TM_ReturnPayload*)token_out_buffer.data;
      dbg(DBG_USR1, "TM: RETURNING HOME: to%d,  via:%d,  lng:%d,  sd:%d,  aggr:%d  \n", 
	  ret->to_tok,
	  ret->via_tok, 
	  length, 
	  ret->seed_val, 
	  ret->aggr_tok);

      token_out_buffer.type = ret->via_tok;
      token_out_buffer.length = length;

      return call SendMsg.send[id](ret->via_tok, length, &token_out_buffer);
    }
  }

  /*typedef struct TM_ReturnPayload
{
  uint16_t to_tok; // Where the return is going.
  uint16_t via_tok; // The spanning tree it's using

  uint16_t aggr_tok; // The aggregation function, 0 for no aggregation.
  uint16_t seed_val; // The seed value, for aggregation

  uint16_t timestamp; // timestamp that the return fired

  uint16_t generation; // The generation of this emission.

  //  int8_t length; // length of return_val

  // Thus the actual payload is the remaining space after we've
  // factored out our overhead for the bookkeeping fields you see above.
  int8_t return_val[RETURNTOK_DATA_LENGTH];    

  // Return messages will be a little different.

} TM_ReturnPayload;
*/


  // ======================================================================
  // Here we obligatorily fill in the rest of our required interfaces.

  command result_t StdControl.init() {
    atomic {
      in_buffer_start = 0;
      in_buffer_end = -1;
      currently_processing = NULL;
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
    // dbg(DBG_USR1, "TM BasicTMCommM: Done sending type: %d\n", msg->type);
    send_pending = FALSE;
    
    return SUCCESS;
  }
  
  event TOS_MsgPtr ReceiveMsg.receive[uint8_t id](TOS_MsgPtr msg) {
    TM_Payload* payload = (TM_Payload*)(msg->data);

    // Bounce it if it's not a good path.
    if (cached_token == NULL) {
      dbg(DBG_USR1, "TM BasicTMComm: receiving message to %d, curNULL posting tokenhandler o%d/p%d/c%d/g%d.\n", 
	  id, payload->origin, payload->parent, payload->counter, payload->generation);
    } else if (	//	payload->generation > 
	       payload->counter < cached_payload->counter) {
      dbg(DBG_USR1, "TM BasicTMComm: receiving message to %d, cur%d posting tokenhandler o%d/p%d/c%d/g%d.\n", 
	  id, cached_payload->counter, payload->origin, payload->parent, payload->counter, payload->generation);
    } else {
      dbg(DBG_USR1, "TM BasicTMComm: receiving message to %d, BOUNCED (cur c%d) o%d/p%d/c%d/g%d.\n", 
	  id, cached_payload->counter,
	  payload->origin, payload->parent, payload->counter, payload->generation);
      return msg;
    }

    // Post the token handler.
    call add_msg(msg);
    post tokenhandler();
    //return signal TMComm.receive[id](msg);
    return msg;
  }

  // This should never fire.  In fact, maybe I should signal an error here.
  //default event TOS_MsgPtr TMComm.receive[uint8_t id](TOS_MsgPtr msg) { return msg; }
}
