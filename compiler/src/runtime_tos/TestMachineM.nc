
includes TestMachine;
includes TokenMachineRuntime;

module TestMachineM 
{
  provides interface StdControl as Control; 
  
  provides command int8_t fun();

  //  uses interface SendMsg as GeneralSend[uint8_t id];

  uses interface ReceiveMsg as Recv_A;
  uses interface ReceiveMsg as Recv_B; 
  uses interface ReceiveMsg as Recv_89; 
  uses interface Timer;

  uses interface TMComm;
}
implementation 
{
  int numsent = 0;
  TOS_Msg test_packet;
  TOS_Msg test_packet2;
  TM_Payload test_token;

  TM_Payload incoming_tokens[100];

  //  task void tokenhandler() {
  //    dbg(DBG_USR1, "TM TestMachine: HANDLER RUNNING.\n");
  //  }

  event TOS_MsgPtr TMComm.receive(TOS_MsgPtr msg) {
    dbg(DBG_USR1, "TM TestMachine: Received msg through TMComm, addr %d, type %d, group %d \n", 
	msg->addr, msg->type, msg->group);
    return msg;
  }

  event TOS_MsgPtr Recv_A.receive(TOS_MsgPtr msg) {
    dbg(DBG_USR1, "TM TestMachine: RECEIVED MSG ON CHANNEL A, addr %d, type %d, group %d \n", 
	msg->addr, msg->type, msg->group);
    dbg(DBG_USR1, "TM TestMachine: But the orig packet..., addr %d, type %d, group %d \n", 
	test_packet.addr, test_packet.type, test_packet.group);

    dbg(DBG_USR1, "TM TestMachine: And the backup packet..., addr %d, type %d, group %d \n", 
	test_packet2.addr, test_packet2.type, test_packet2.group);
    return msg;
  }

  event TOS_MsgPtr Recv_B.receive(TOS_MsgPtr msg) {
    dbg(DBG_USR1, "TM TestMachine: RECEIVED MSG ON CHANNEL B, addr %d, type %d, group %d \n", 
	msg->addr, msg->type, msg->group);
    return msg;
  }

  event TOS_MsgPtr Recv_89.receive(TOS_MsgPtr msg) {
    dbg(DBG_USR1, "TM TestMachine: RECEIVED MSG ON channell 89, addr %d, type %d, group %d \n", 
	msg->addr, msg->type, msg->group);
    return msg;
  }

  /*  event result_t GeneralSend.sendDone[uint8_t id] (TOS_MsgPtr msg, result_t success) {
    dbg(DBG_USR1, "TM TestMachine: Done sending msg from GeneralSend\n");
    return SUCCESS;
    }*/


  command result_t Control.init() {
    int i;

    dbg(DBG_USR1, "TM TestMachineM: INITIALIZING, data length %d\n", TOK_DATA_LENGTH);

    test_token.parent = TOS_LOCAL_ADDRESS; // Our ID number
    test_token.origin = TOS_LOCAL_ADDRESS; // Our ID number    
    //    test_token.timestamp = ????
    test_token.counter = 0;
    //    test_token.numargs = 0;

    for(i=0; i<TOK_DATA_LENGTH; i++) {
      test_token.args[i] = i;
    }

    //    test_packet.addr = ???;
    //    test_packet.group = ???;
    //    test_packet.crc = ???;


    //    test_packet.addr = TOS_LOCAL_ADDRESS;
    test_packet.addr = 65535;
    test_packet.type = 89;
    test_packet.group = 125;
    test_packet.length = sizeof(TM_Payload);
    
    memcpy(test_packet.data, (&test_token), sizeof(TM_Payload));

    //    memcpy(&test_packet, &test_packet2, sizeof(TOS_Msg));
    memcpy(&test_packet2, &test_packet, sizeof(TOS_Msg));


    //    post tokenhandler();
    return SUCCESS;
  }

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
    dbg(DBG_USR1, "TM TestMachineM: Sending that message.....%d %d %d\n",
	test_packet.addr, test_packet.type, test_packet.group);

    call TMComm.emit(TOS_BCAST_ADDR, sizeof(uint16_t), &test_packet);    

    //    call Send_A.send(TOS_BCAST_ADDR, sizeof(uint16_t), &test_packet);    

    //    call PlainSend.send(TOS_BCAST_ADDR, sizeof(uint16_t), &test_packet);    

    //call Send_89.send(TOS_BCAST_ADDR, sizeof(uint16_t), &test_packet); 
    //call BareSend.send(&test_packet);    

    //    call Send_A.send(TOS_BCAST_ADDR, sizeof(uint16_t), &test_packet);

    //    call GeneralSend.send[test_packet.type](TOS_BCAST_ADDR, sizeof(uint16_t), &test_packet);

    return SUCCESS;
  }
}

