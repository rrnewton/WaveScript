//includes TMComm

configuration BasicTMComm {  
  provides interface TMComm;
} implementation {
  components Main, BasicTMCommM, TimerC, RandomLFSR, GenericComm as Comm;

  TMComm = BasicTMCommM.TMComm;

  //  BasicTMCommM.ReceiveMsg -> Comm.ReceiveMsg; //[uint_t id];

  Main.StdControl -> BasicTMCommM; 
  Main.StdControl -> TimerC;
  //  Main.StdControl -> TokenMachineRuntimeM;

  BasicTMCommM.Random -> RandomLFSR;
  BasicTMCommM.Timer -> TimerC.Timer[unique("Timer")];
  BasicTMCommM.SendMsg -> Comm.SendMsg;

  //  TokenMachineRuntimeM.ReceiveMsg -> Comm.ReceiveMsg[0];
}

