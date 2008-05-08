
includes TestMachine;
includes TokenMachineRuntime;

module Test2M
{
  provides interface StdControl as Control; 
  provides interface TMModule; 

  uses interface Timer;
  uses interface TMComm;
}
implementation 
{
  TOS_Msg test_packet;

  command TOS_MsgPtr TMModule.process_token(TOS_MsgPtr msg) {
    TM_Payload* payload = (TM_Payload*) msg->data;

    dbg(DBG_USR1, "TM Test2: Received msg through TMComm, addr %d, type %d, group %d, from:%d \n", 
	msg->addr, msg->type, msg->group, payload->parent);

    if (call TMComm.relay()) {
      dbg(DBG_USR1, "TM Test2: Relay succeeded!\n");
    } else {
      dbg(DBG_USR1, "TM Test2: Relay failed\n");
    }

    return msg;
  }

  command result_t Control.init() {
    return SUCCESS;
  }

  command result_t Control.start() {
    return call Timer.start(TIMER_REPEAT, 1000);
  }

  command result_t Control.stop() {
    return call Timer.stop();
  }

  event result_t Timer.fired() {
    if (TOS_LOCAL_ADDRESS == BASE_STATION ) {

      dbg(DBG_USR1, "TM Test2: Timer Fired, I'm base, trying emit..\n");
      
      // emit will only return after it's copied the buffer, unlike send!
      test_packet.type = 32;
      test_packet.length = 1;
      call TMComm.emit(TOS_BCAST_ADDR, sizeof(uint16_t), &test_packet);   
      
      return SUCCESS;
    }
  }
}
