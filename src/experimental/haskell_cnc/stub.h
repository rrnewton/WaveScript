

#define INCLUDEMETHOD

#if   CNC_VARIANT == 1
#include "CncPure.hs"
#elif CNC_VARIANT == 2
#include "Cnc.hs"
#elif CNC_VARIANT == 3
#error "Cnc_serialST not fully working yet"
#include "Cnc_serialST.hs"
#else
#error "CNC_VARIANT not set to a known value."
#endif
