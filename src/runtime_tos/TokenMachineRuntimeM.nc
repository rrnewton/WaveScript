
/* Author: Ryan Newton <newton@mit.edu>
 * Last modified: [2004.08.21]
 */

module TokenMachineRuntimeM {
  provides {
    interface StdControl;
    command result_t testfun ();
  }
  uses {
    interface Timer;
    interface ReceiveMsg;
    interface SendMsg;
    interface Random;
  }
} implementation {

  task void tokenhandler () {
  }

  command result_t StdControl.init() {
    return call Random.init();
  }
  command result_t StdControl.start() {
    return call Timer.start(TIMER_REPEAT, 1000);
  }
  command result_t StdControl.stop() {
    return call Timer.stop();
  }

  event result_t Timer.fired() {
    dbg(DBG_USR1, "TokenMachineRuntimeM: Sending test\n");
    call SendMsg.send(TOS_BCAST_ADDR, sizeof(uint16_t), &test_packet);    
    return SUCCESS;

  }

  event result_t SendMsg.sendDone(TOS_MsgPtr msg, bool success) {
    dbg(DBG_USR1, "TokenMachineRuntimeM: Done sending msg #%d, success=%d\n", numsent, success);
    numsent++;
    return SUCCESS;
  }

  event TOS_MsgPtr ReceiveMsg.receive(TOS_MsgPtr recv_packet) {
    int i;
    uint8_t length = recv_packet->length;
    uint8_t type = recv_packet->type;
    TM_Payload payload = *((TM_Payload *)recv_packet->data);

    dbg(DBG_USR1, "TokenMachineRuntimeM: Received message from %d of type %d, length %d \n",
	payload.parent, type, length);
    dbg(DBG_USR1, "TokenMachineRuntimeM:   Contents: ");
    for(i=0; i<TOK_DATA_LENGTH; i++) {
      dbg(DBG_USR1, "%d, ", payload.args[i]);
    }
    dbg(DBG_USR1, "\n");
    
    return recv_packet;
  }

  command result_t testfun () {
    return SUCCESS;
  }

}



