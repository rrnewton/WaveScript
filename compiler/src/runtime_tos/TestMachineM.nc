
includes TestMachine;
includes TokenMachineRuntime;

module TestMachineM 
{
  provides interface StdControl as Control; 

  provides interface TMModule; 
  uses interface TMComm;
  uses interface Timer;
}
implementation 
{

  command result_t Control.start() {
    return call Timer.start(TIMER_REPEAT, 1000);
  }

  command result_t Control.stop() {
    return call Timer.stop();
  }

  command int8_t fun () {
        return 3;
  }

  event result_t Timer.fired() {
    dbg(DBG_USR1, "TM TestMachineM: Timer fired\n");
    return SUCCESS;
  }
}



