module BasicTMCommM {
  provides {
    interface TMComm;
    interface StdControl;
  }
  uses {
    interface Timer;
    //    interface ReceiveMsg;
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
  
  command result_t TMComm.emit(uint16_t address, uint8_t length, TOS_MsgPtr msg) {
    return SUCCESS;
  }

  command result_t TMComm.relay(uint16_t address, uint8_t length, TOS_MsgPtr msg) {
    return SUCCESS;
  }

  command result_t TMComm.return_home(uint16_t address, uint8_t length, TOS_MsgPtr msg) {
    return SUCCESS;
  }

  event result_t Timer.fired() {
    dbg(DBG_USR1, "TokenMachineRuntimeM: Sending test\n");
    return SUCCESS;

  }

  event result_t SendMsg.sendDone(TOS_MsgPtr msg, bool success) {
    dbg(DBG_USR1, "TokenMachineRuntimeM: Done sending \n");
    return SUCCESS;
  }

}

