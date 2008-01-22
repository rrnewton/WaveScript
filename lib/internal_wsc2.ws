
// These are externally defined primitives that are currently only
// applied to the new wsc2 backend.

include "sigseg_copyalways.ws";

using Sigseg;

/*
main = iterate _ in timer(3.0) { 
  using Sigseg;
  s :: SS Int = nullseg;
  s2 = joinsegs(s,s);

  w :: Sigseg Int = toSigseg(Array:make(100, 90), 50, nulltimebase);

  //nl = nullseg;
  v :: Sigseg Int16 = make_nullseg();
  x :: Sigseg Int64 = make_nullseg();
  //x = nullseg;

  a :: List Int16 = [];
  b :: List Int64 = [];
  null = [];
  c :: List Int = null;
  d :: List Float = null;

  emit w;
}

*/
