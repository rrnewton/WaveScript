
enum {
  AM_RETURNMSG = 0,
};

#ifndef TOK_DATA_LENGTH
#define TOK_DATA_LENGTH (TOSH_DATA_LENGTH - (2 + 2 + 2 + 1 ))
#endif

#ifndef RETURNTOK_DATA_LENGTH
#define RETURNTOK_DATA_LENGTH (TOSH_DATA_LENGTH - (2 + 2 + 2 + 2 + 2 ))
#endif

// This TM_Payload fits inside the data field of TOS_Msg.
typedef struct TM_Payload
{
  uint16_t origin;
  uint16_t parent;

  uint16_t timestamp;
  uint8_t counter;

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

  //  int8_t length; // length of return_val

  // Thus the actual payload is the remaining space after we've
  // factored out our overhead for the bookkeeping fields you see above.
  int8_t return_val[RETURNTOK_DATA_LENGTH];    

  // Return messages will be a little different.

} TM_ReturnPayload;
