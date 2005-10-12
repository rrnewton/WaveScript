
includes TestMachine; 
includes TokenMachineRuntime;

configuration Test2
{
}
implementation
{
  components BasicTMComm, TimerC, Main, UARTNoCRCPacket, GenericComm as Comm;
  components Test2M;

  // TokenMachineRuntime, FramerM, UART,

  Main.StdControl -> Test2M.Control;
  Main.StdControl -> Comm;
  Main.StdControl -> TimerC;

  //  Comm.ReceiveMsg[AM_TOKEN_A] -> Test2M.Recv_A;
  //  Comm.ReceiveMsg[AM_TOKEN_B] -> Test2M.Recv_B;

  Test2M.Timer -> TimerC.Timer[unique("Timer")];

  Test2M.TMComm -> BasicTMComm.TMComm[79];

  BasicTMComm.TMModule -> Test2M.TMModule;

}
