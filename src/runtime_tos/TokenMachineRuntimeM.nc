
/* Author: Matt Welsh <mdw@eecs.harvard.edu> 
 * Last modified: 3 August 2003
 */

module TokenMachineRuntimeM {
  provides {
    interface StdControl;
  }
  uses {
    interface Timer;
    interface ReceiveMsg;
    interface SendMsg;
    interface Random;
  }
} implementation {

  TOS_Msg test_packet;

  command result_t StdControl.init() {
    *((uint16_t *)test_packet.data) = TOS_LOCAL_ADDRESS; // Our ID number
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
    dbg(DBG_USR1, "TokenMachineRuntimeM: Done sending, success=%d\n", success);
    return SUCCESS;
  }

  event TOS_MsgPtr ReceiveMsg.receive(TOS_MsgPtr recv_packet) {
    uint16_t nodeaddr = *((uint16_t *)recv_packet->data);
    dbg(DBG_USR1, "TokenMachineRuntimeM: Received message from %d\n", nodeaddr);
    return recv_packet;
  }

}

