//includes TMComm

configuration BasicTMComm {  
  provides interface TMComm;
  provides interface ReceiveMsg[uint8_t id];
} implementation {
  components Main, BasicTMCommM, TimerC, RandomLFSR, GenericComm as Comm;

  TMComm = BasicTMCommM.TMComm;
  ReceiveMsg = Comm.ReceiveMsg;

  //  BasicTMCommM.ReceiveMsg -> ReceiveMsg; //[uint_t id];

  Main.StdControl -> BasicTMCommM; 
  Main.StdControl -> TimerC;
  //  Main.StdControl -> TokenMachineRuntimeM;

  BasicTMCommM.Random -> RandomLFSR;
  BasicTMCommM.Timer -> TimerC.Timer[unique("Timer")];
  BasicTMCommM.SendMsg -> Comm.SendMsg;

  //  TokenMachineRuntimeM.ReceiveMsg -> Comm.ReceiveMsg[0];
}

