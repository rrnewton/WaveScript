

module ReturnHandlerM {

  uses interface ReceiveMsg as Recv;

} implementation {

  // Receive a backward flowing return message!
  event TOS_MsgPtr Recv.receive(TOS_MsgPtr msg) {
    return msg;
  }

  // Handle a return message
}
