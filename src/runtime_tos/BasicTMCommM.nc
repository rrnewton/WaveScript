
module BasicTMCommM {
  provides {
    interface TMComm[uint8_t id];
    interface StdControl;
    //    interface ReceiveMsg;
  }
  uses {
    interface Timer;
    interface ReceiveMsg[uint8_t id];
    interface SendMsg[uint8_t id];
    interface Random;
  }
} implementation {

  int test_test;

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
    dbg(DBG_USR1, "BasicTMCommM: Timer Fired\n");
    return SUCCESS;
  }

  event result_t SendMsg.sendDone[uint8_t sent_id](TOS_MsgPtr msg, bool success) {
    dbg(DBG_USR1, "BasicTMCommM: Done sending \n");
    return SUCCESS;
  }
  
  event TOS_MsgPtr ReceiveMsg.receive[uint8_t id](TOS_MsgPtr msg) {

    //    dbg(DBG_USR1, "TestMachine: RECEIVED MSG OF TYPE %D: addr %d, type %d, group %d \n", 
    //	id, msg->addr, msg->type, msg->group);
    return msg;
  }


}

