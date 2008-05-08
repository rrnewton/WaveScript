// [2004.09.29]
// This is the interface implemented by an object file produced by my
// compiler.

interface TMModule {
  // This handles a token of any type.
  command TOS_MsgPtr process_token( TOS_MsgPtr tok );  

  //  TOS_MsgPtr* token_cache;
  //  uint8_t num_tokens;
  //  int8_t foobar;
}
