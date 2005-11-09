
includes AM;

// These are all the communication services used per-token-handler.
// Provides send, receive, emit, and return
// [2004.09.07] Note! I'm not going to be able to make emitDone and
// relayDone work... it's all just gonna have to use sendDone.


interface TMComm {  
    // Generic send for any token/AM type:
    // command result_t send(uint16_t address, uint8_t length, TOS_MsgPtr msg);
    //    event result_t sendDone(TOS_MsgPtr msg, result_t success);
    

    // It would be more efficient to put the arguments as arguments to
    // this command rather than hiding them inside the message
    // payload.  But that's an optimization.

    //    event result_t emitDone(TOS_MsgPtr msg, result_t success);

    //    async command result_t add_msg(TOS_MsgPtr token);     

    // This returns the cached version of that token:
    command TOS_MsgPtr get_cached(uint8_t ind);
    // This sets the cache for that token:
    command result_t set_cached(TOS_MsgPtr newtok);
}

