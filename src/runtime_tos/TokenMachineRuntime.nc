includes TokenMachineRuntime; 

configuration TokenMachineRuntime {
  provides command result_t testfun ();
//  provides interface TMRuntime;
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

