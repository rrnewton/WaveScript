
includes AM;

// These are all the communication services used per-token-handler.
// Provides send, receive, emit, and return
// [2004.09.07] Note! I'm not going to be able to make emitDone and
// relayDone work... it's all just gonna have to use sendDone.


interface TMComm {
  
    // Generic send/receive for any token/AM type:
    // command result_t send(uint16_t address, uint8_t length, TOS_MsgPtr msg);
    event result_t sendDone(TOS_MsgPtr msg, result_t success);

    // Receive messages.  This is only triggered when the messages are
    // not return messages which are automatically handled under the hood.
    //    event TOS_MsgPtr receive(TOS_MsgPtr m);

    // Emit: launches a new message
    command result_t emit(uint16_t address, uint8_t length, TOS_MsgPtr msg);
    //    event result_t emitDone(TOS_MsgPtr msg, result_t success);

    // Relay: relaunches a message, updating Payload
    command result_t relay(uint16_t address, uint8_t length, TOS_MsgPtr msg);

    //    event result_t relayDone(TOS_MsgPtr msg, result_t success);

    // Return: sends a message 
    // NEEDS MORE ARGS:
    command result_t return_home(uint16_t address, uint8_t length, TOS_MsgPtr msg);
    //    event result_t emitDone(TOS_MsgPtr msg, result_t success);

}
