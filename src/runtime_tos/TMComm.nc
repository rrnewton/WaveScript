
includes AM;

// These are all the communication services used per-token-handler.
// Provides send, receive, emit, and return
// [2004.09.07] Note! I'm not going to be able to make emitDone and
// relayDone work... it's all just gonna have to use sendDone.


interface TMComm {  
    // Generic send for any token/AM type:
    // command result_t send(uint16_t address, uint8_t length, TOS_MsgPtr msg);
    //    event result_t sendDone(TOS_MsgPtr msg, result_t success);
    
    // Receive messages.  This is only triggered when the messages are
    // not return messages which are automatically handled under the hood.
    //event TOS_MsgPtr receive(TOS_MsgPtr m);

    // Emit: launches a new message
    command result_t emit(uint16_t address, uint8_t length, TOS_MsgPtr msg);
    //    event result_t emitDone(TOS_MsgPtr msg, result_t success);

    // Relay: relaunches a message, updating Payload
    //    command result_t relay(uint16_t address, uint8_t length, TOS_MsgPtr msg);
    command result_t relay();

    //    event result_t relayDone(TOS_MsgPtr msg, result_t success);

    command uint16_t get_dist();

    command void process_return(TM_ReturnPayload* msg);

    command result_t return_home(uint8_t length, TOS_MsgPtr msg);
    // It would be more efficient to put the arguments as arguments to
    // this command rather than hiding them inside the message
    // payload.  But that's an optimization.

    //    event result_t emitDone(TOS_MsgPtr msg, result_t success);

    //    async command result_t add_msg(TOS_MsgPtr token);     

    // This returns the cached version of that token:
    command TOS_MsgPtr get_cached();
    // This sets the cache for that token:
    command result_t set_cached(TOS_MsgPtr newtok);

}

