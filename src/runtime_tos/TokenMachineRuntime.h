
enum {
  AM_RETURNMSG = 0,
};

// RRN: HACK JUST TO GET IT TO COMPILE ATM [2004.11.19]
#ifndef TOSH_DATA_LENGTH
#define TOSH_DATA_LENGTH 32
#endif

#ifndef TOKBUFFER_LENGTH
#define TOKBUFFER_LENGTH 10 // Buffer 10 incoming messages. Should be around 320 bytes.
#endif

#ifndef BASE_TM_PAYLOAD_SIZE 
#define BASE_TM_PAYLOAD_SIZE (2 + 2 + 2 + 1 + 2)
#endif

#ifndef BASE_TM_RETPAYLOAD_SIZE 
#define BASE_TM_RETPAYLOAD_SIZE (2 + 2 + 2 + 2 + 2 + 2)
#endif

#ifndef TOK_DATA_LENGTH
#define TOK_DATA_LENGTH (TOSH_DATA_LENGTH - BASE_TM_PAYLOAD_SIZE)
#endif

#ifndef RETURNTOK_DATA_LENGTH
#define RETURNTOK_DATA_LENGTH (TOSH_DATA_LENGTH - BASE_TM_RETPAYLOAD_SIZE)
#endif

// This TM_Payload fits inside the data field of TOS_Msg.
typedef struct TM_Payload
{
  uint16_t origin;
  uint16_t parent;

  uint16_t timestamp;
  uint8_t counter;

  uint16_t generation; // The generation of this emission.

  //  int8_t numargs; // length of args

  // Thus the actual payload is the remaining space after we've
  // factored out our overhead for the bookkeeping fields you see above.
  int8_t args[TOK_DATA_LENGTH];    

} TM_Payload;

  // Return messages will be a little different.

typedef struct TM_ReturnPayload
{
  uint16_t to_tok; // Where the return is going.
  uint16_t via_tok; // The spanning tree it's using

  uint16_t aggr_tok; // The aggregation function, 0 for no aggregation.
  uint16_t seed_val; // The seed value, for aggregation

  uint16_t timestamp; // timestamp that the return fired

  uint16_t generation; // The generation of this emission.

  //  int8_t length; // length of return_val

  // Thus the actual payload is the remaining space after we've
  // factored out our overhead for the bookkeeping fields you see above.
  int8_t return_val[RETURNTOK_DATA_LENGTH];    

  // Return messages will be a little different.

} TM_ReturnPayload;
