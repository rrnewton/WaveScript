//includes TMComm;

//  provides interface TMRuntime;

// These are all the communication services used per-token-handler.
// Provides send, receive, emit, and return

interface TMComm {
    command result_t send(uint16_t address, uint8_t length, TOS_MsgPtr msg);
    event result_t sendDone(TOS_MsgPtr msg, result_t success);
    event TOS_MsgPtr receive(TOS_MsgPtr m);

    command result_t emit(uint16_t address, uint8_t length, TOS_MsgPtr msg);
    event result_t emitDone(TOS_MsgPtr msg, result_t success);
    
    command result_t emit(uint16_t address, uint8_t length, TOS_MsgPtr msg);
    event result_t emitDone(TOS_MsgPtr msg, result_t success);

    command result_t return(uint16_t address, uint8_t length, TOS_MsgPtr msg);
    //    event result_t emitDone(TOS_MsgPtr msg, result_t success);


}

configuration TMComm {
  provide {

  }

} implementation {
  
  components Main, TokenMachineRuntimeM, TimerC, RandomLFSR, GenericComm as Comm;

  Main.StdControl -> Comm;
  Main.StdControl -> TimerC;
  Main.StdControl -> TokenMachineRuntimeM;

  TokenMachineRuntimeM.Random -> RandomLFSR;
  TokenMachineRuntimeM.Timer -> TimerC.Timer[unique("Timer")];
  TokenMachineRuntimeM.SendMsg -> Comm.SendMsg[AM_TOKSEND];
  TokenMachineRuntimeM.ReceiveMsg -> Comm.ReceiveMsg[AM_TOKSEND];
}



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
