//includes TMComm

configuration BasicTMComm {  
  provides interface TMComm;
} implementation {
  components Main, BasicTMCommM, TimerC, RandomLFSR, GenericComm as Comm;

  TMComm = BasicTMCommM.TMComm;

  Main.StdControl -> BasicTMCommM; 
  Main.StdControl -> TimerC;
  //  Main.StdControl -> TokenMachineRuntimeM;

  BasicTMCommM.Random -> RandomLFSR;
  BasicTMCommM.Timer -> TimerC.Timer[unique("Timer")];
  BasicTMCommM.SendMsg -> Comm.SendMsg[0];
  //  TokenMachineRuntimeM.ReceiveMsg -> Comm.ReceiveMsg[0];
}

