includes TokenMachineRuntime; 

configuration TokenMachineRuntime {
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