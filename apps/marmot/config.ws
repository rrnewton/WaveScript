


// Should we run aml on the server or the client?
AMLSERVERSIDE = true;

// enable adaptive mode
AMLADAPTIVE = false;

// Should we bring up gnuplot and image viewer windows??
GUIENABLED = false;

// Should we record .ppm files for the likelihood maps?
PPMFILES = false; // Saving as false in the repository for regression testing w/ scheme backend.


// [2007.11.13] Evil things I'm doing to get results.
EVILHACKS = true

NUMTHREADS = if GETENV("NUMTHREADS") == ""
             then 1 else stringToInt(GETENV("NUMTHREADS"))

