
enum {
  AM_RETURNMSG = 0,
};

#ifndef TOK_DATA_LENGTH
#define TOK_DATA_LENGTH (TOSH_DATA_LENGTH - (2 + 2 + 2 + 1 + 1))
#endif

// This TM_Payload fits inside the data field of TOS_Msg.
typedef struct TM_Payload
{
  uint16_t origin;
  uint16_t parent;

  uint16_t timestamp;
  uint8_t counter;

  int8_t numargs;

  // Thus the actual payload is the remaining space after we've
  // factored out our overhead for the bookkeeping fields you see above.
  int8_t args[TOK_DATA_LENGTH];    

} TM_Payload;
