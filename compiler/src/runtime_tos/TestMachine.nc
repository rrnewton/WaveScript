
includes TestMachine; 
includes TokenMachineRuntime;

configuration TestMachine
{
  //  provides interface StdControl;

  //  provides interface TokenMachine;
  // GOTTA DEFINE THIS ^^^
}
implementation
{
  components TestMachineM, BasicTMComm, TimerC, Main, UARTNoCRCPacket, GenericComm as Comm;
  // TokenMachineRuntime, FramerM, UART,

  Main.StdControl -> TestMachineM.Control;
  Main.StdControl -> Comm;
  Main.StdControl -> TimerC;

  //  TestMachineM.GeneralSend -> Comm.SendMsg;

  TestMachineM.Recv_A -> Comm.ReceiveMsg[AM_TOKEN_A];
  TestMachineM.Recv_B -> Comm.ReceiveMsg[AM_TOKEN_B];
  TestMachineM.Recv_89 -> Comm.ReceiveMsg[89];
  
  //  Comm.ReceiveMsg[AM_TOKEN_A] -> TestMachineM.Recv_A;
  //  Comm.ReceiveMsg[AM_TOKEN_B] -> TestMachineM.Recv_B;

  TestMachineM.Timer -> TimerC.Timer[unique("Timer")];

  TestMachineM.TMComm -> BasicTMComm.TMComm[91];

  // TestMachineM.Send_A -> Comm.SendMsg[AM_TOKEN_A];
  // TestMachineM.Send_B -> Comm.SendMsg[AM_TOKEN_B];
}
