//includes TMComm

configuration BasicTMComm {  
  provides interface TMComm[uint8_t id];
} implementation {
  components Main, BasicTMCommM, TimerC, RandomLFSR, GenericComm as Comm;

  TMComm = BasicTMCommM.TMComm;
  //  ReceiveMsg = Comm.ReceiveMsg;

  //  BasicTMCommM.ReceiveMsg -> ReceiveMsg; //[uint_t id];
  
  Main.StdControl -> BasicTMCommM.StdControl; 
  Main.StdControl -> TimerC;
  //  Main.StdControl -> TokenMachineRuntimeM;

  BasicTMCommM.Random -> RandomLFSR.Random;
  BasicTMCommM.Timer -> TimerC.Timer[unique("Timer")];

  BasicTMCommM.SendMsg -> Comm.SendMsg;
  BasicTMCommM.ReceiveMsg -> Comm.ReceiveMsg;

  //  BasicTMCommM.TMComm.receive -> Comm.ReceiveMsg.receive;
  

  //  TokenMachineRuntimeM.ReceiveMsg -> Comm.ReceiveMsg[0];
}

