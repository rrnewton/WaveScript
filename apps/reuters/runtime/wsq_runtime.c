


//====================================================================================================
// wsq_runtime.c
// This compiles to a library that can be linked to another program to give it stream processing capabilities.
// This library does not *include* the WaveScript compiler, it finds it and loads it dynamically.
//====================================================================================================

// This version connects to Chez Scheme.


// NOTES: HOWTO ADD A NEW ENTRYPOINT INTO SCHEME:

// (1) Add a C prototype for the function pointer (Scheme_*).
// (2) Add a wrapper that's exposed to C code (WSQ_*).
// (3) Extend the WSQ_Init to initialize the function pointer by doing
//     a lookup in the global scheme symbol table.
//     (These names end in -entry because of the define-entrypoint macro.)


#include <stdlib.h>
#include <string.h>	
#include <stdio.h>
#include <sys/types.h>
#include <limits.h>
#include <errno.h>

#include "wsq_runtime.h"

//const char *S_date_stamp = "10102009080511";
const char *S_date_stamp = "0";

#include "scheme.h"


//==============================================================================
// First, function pointers to all of the relevant calls.  These point
// to scheme functions.

void (*Scheme_BeginTransaction)(wsid_t id);
int (*Scheme_EndTransaction)();

void (*Scheme_BeginSubgraph)(wsid_t id);
void (*Scheme_EndSubgraph)();
void (*Scheme_RemSubgraph)(wsid_t id);

ptr  (*Scheme_EdgeType)(wsid_t id);

//void (*Scheme_AddReutersSource)(wsid_t ndid, wsid_t id, float freq, const char* path);
void (*Scheme_AddPrinter)(wsid_t ndid, const char* prefix, wsid_t id);
void (*Scheme_AddProject)(wsid_t ndid, wsid_t in, wsid_t out, const char* expr);
void (*Scheme_AddFilter) (wsid_t ndid, wsid_t in, wsid_t out, const char* expr);

void (*Scheme_AddWindowJoin) (wsid_t ndid, wsid_t id_in1, wsid_t id_in2, wsid_t id_out, float seconds, const char* recA, const char* recB, const char* expr);

void (*Scheme_ConnectRemoteOut) (wsid_t ndid, wsid_t out, const char* host, int port);
void (*Scheme_ConnectRemoteIn)  (wsid_t ndid, wsid_t in,  const char* host, int port, const char* types);

// The generic version.
void (*Scheme_AddOp)  (wsid_t ndid, const char* optype, const char* inputs, const char* outputs, const char* args);

void (*Scheme_Shutdown) ();

void (*Scheme_SetOutputFile) (const char* path);
void (*Scheme_SetQueryName) (const char* name);
void (*Scheme_SetBackend) (enum WSQBackend mode);

void (*Scheme_Pause) ();
int  (*Scheme_Unpause) ();


//==============================================================================
/* The functions exposed through the C API are just wrappers for the
   above scheme functions.  The only reason that these are separate
   (annoying I know) is that in our wsq_runtime.h header file we don't
   want to expose the fact that we are using function pointers.
*/

void WSQ_BeginTransaction(wsid_t id) { Scheme_BeginTransaction(id); }
int  WSQ_EndTransaction()          { return Scheme_EndTransaction(); }

void WSQ_BeginSubgraph(wsid_t id) { Scheme_BeginSubgraph(id); }
void WSQ_EndSubgraph()          { Scheme_EndSubgraph(); }
void WSQ_RemSubgraph(wsid_t id)   { Scheme_RemSubgraph(id); }

char* WSQ_EdgeType  (wsid_t id) { 
  ptr sstr = Scheme_EdgeType(id);
  iptr len = Sstring_length(sstr);
  char* buf = malloc(len+1);
  int i;  

  /*
  printf("Copying %d chars from scheme string: %s\n", len, (char*)sstr);
  for(i=0; i<len; i++) 
    printf(" %c", Sstring_ref(sstr, i));
  printf("\n\n");
  */

  //memcpy(buf, (char*)Sstring_value(sstr), len+1);
  // FIXME: Bug kent about string_value
  //memcpy(buf, (char*)(sstr), len+1);

  // [2009.10.13] I am currently FAILING to memcpy out the string.
  for(i=0; i<len; i++) 
    buf[i] = Sstring_ref(sstr, i);
  buf[len] = 0;

  //printf("Resulting buffer: %s\n", buf);

  return buf; 
}

//void WSQ_AddReutersSource(wsid_t ndid, wsid_t id, float freq, const char* path) { Scheme_AddReutersSource(ndid, id, freq, path); } 
void WSQ_AddPrinter(wsid_t ndid, const char* prefix, wsid_t id)     { Scheme_AddPrinter(ndid, prefix, id); }

void WSQ_AddProject(wsid_t ndid, wsid_t in, wsid_t out, const char* expr) { Scheme_AddProject(ndid,in,out,expr); } 
void WSQ_AddFilter (wsid_t ndid, wsid_t in, wsid_t out, const char* expr) { Scheme_AddFilter(ndid,in,out,expr); }

void WSQ_AddWindowJoin (wsid_t ndid, wsid_t in1, wsid_t in2, wsid_t out, float seconds, const char* recA, const char* recB, const char* expr) { 
    Scheme_AddWindowJoin(ndid,in1,in2,out,seconds,recA, recB,expr); 
}

void WSQ_ConnectRemoteOut (wsid_t ndid, wsid_t out, const char* host, int port) { Scheme_ConnectRemoteOut(ndid,out,host,port); }
void WSQ_ConnectRemoteIn  (wsid_t ndid, wsid_t in, const char* host, int port, const char* types)  { Scheme_ConnectRemoteIn (ndid,in,host,port,types); }

void WSQ_Shutdown() {
  Scheme_Shutdown();
  Sscheme_deinit(); // Chez call to bring down the runtime.
}

// Generic version, the other functions could be replaced with this one:
void WSQ_AddOp(wsid_t ndid, const char* optype, const char* inputs, 
	       const char* outputs, const char* args) {

    Scheme_AddOp(ndid, optype, inputs, outputs, args);
}

//==============================================================================

typedef void (*intfun) (int);
typedef void (*voidfun) ();

//void foo* (int);
intfun foo_fun;

const int PATHMAX = 1000;

char* get_machine_type() {
    //FILE* strm = popen("$WAVESCRIPTD/depends/get_machine_type", "r");
    FILE* strm = popen("$WAVESCRIPTD/apps/reuters/runtime/chez_machine_type_threaded", "r");
    char* format = malloc(100);
    fscanf(strm, "%s\n", format);
    return format;
}

//int do_scheme(int argc, char* argv[]) {
void WSQ_Init(const char* outfile) {

  // void Sscheme_init(void (*abnormal_exit)(void))
  char* chezd = getenv("CHEZD");
  char* machinetype = get_machine_type();
  //char* machinetype = "ti3le"; // [2010.07.30] Temptoggle: hardcoding
  
  char bootfile[PATHMAX];
  
  if (!chezd) {
    printf("Environment variable CHEZD must be bound to the Chez-Scheme install directory, usually called csv<ver>.");
    exit(1);
  }

  int verbose = 0;
  const char* v = getenv("WSQ_VERBOSE");
  if (v) verbose = strtol(v, NULL, 10);
  if (errno) { printf("WSQ_VERBOSE set to invalid value: %s\n", v); abort(); }

  // Sset_verbose(verbose); This won't suppress the banner.
  // Sset_verbose(5);
  Sscheme_init(0);  

  sprintf(bootfile, "%s/boot/%s/petite.boot", chezd, machinetype);
  Sregister_boot_file(bootfile);
  sprintf(bootfile, "%s/boot/%s/scheme.boot", chezd, machinetype);
  Sregister_boot_file(bootfile);

  Sbuild_heap(".", 0);
  //Sbuild_heap(argv[0], 0);

  Senable_expeditor(0);

  const char* regimentd = getenv("WAVESCRIPTD");
  char script[PATHMAX];
  sprintf(script, "%s/apps/reuters/runtime/load_interface.ss", regimentd);

  // NOTE: This is a real oddity.  I must *pad* the argument list??
  // Seems like there might be a bug.
  const char* new_args[] = {"apparently ignored", script};
  // [2010.09.24] Trying to make it quiet...
  // It seems to treat all arguments as files to be loaded, even empty strings!
  //const char* new_args[] = {"", "-q"};
  //const char* new_args[] = {"", "--quiet", script};

  // Tell chez scheme not to print the greeting:
  // (Doesn't work for me presently.)
  //Scall1(Stop_level_value( Sstring_to_symbol("suppress-greeting")), Sfalse);
  Scall1(Stop_level_value( Sstring_to_symbol("suppress-greeting")), Strue);

  if (verbose>=1) printf(" <WSQ>  WSQ_Init: Starting Scheme runtime system.\n");
  int result = Sscheme_start(2, new_args);
  //int result = Sscheme_start(3, new_args);
  if (result) { 
      printf("ERROR: Exited from scheme initialization with non-zero code %d\n", result);
      abort(); 
  }

  // This is awful gross.  Oh well.
  // ============================================================

  if (verbose>=1) printf(" <WSQ> Registering WSQ runtime interface with control module.\n");

  // Grab the relevant function entrypoints from the Scheme symbol table.
  ptr tbegin =  Stop_level_value( Sstring_to_symbol("WSQ_BeginTransaction-entry"));
  ptr tend   =  Stop_level_value( Sstring_to_symbol("WSQ_EndTransaction-entry"));

  ptr gbegin =  Stop_level_value( Sstring_to_symbol("WSQ_BeginSubgraph-entry"));
  ptr gend   =  Stop_level_value( Sstring_to_symbol("WSQ_EndSubgraph-entry"));
  ptr grem   =  Stop_level_value( Sstring_to_symbol("WSQ_RemSubgraph-entry"));
  ptr gtyp   =  Stop_level_value( Sstring_to_symbol("WSQ_EdgeType-entry"));

//  ptr addsrc =  Stop_level_value( Sstring_to_symbol("WSQ_AddReutersSource-entry"));
  ptr addprn =  Stop_level_value( Sstring_to_symbol("WSQ_AddPrinter-entry"));
  ptr addfil =  Stop_level_value( Sstring_to_symbol("WSQ_AddFilter-entry"));
  ptr addpro =  Stop_level_value( Sstring_to_symbol("WSQ_AddProject-entry"));

  ptr addwin =  Stop_level_value( Sstring_to_symbol("WSQ_AddWindowJoin-entry"));

  ptr con_in  = Stop_level_value( Sstring_to_symbol("WSQ_ConnectRemoteIn-entry"));
  ptr con_out = Stop_level_value( Sstring_to_symbol("WSQ_ConnectRemoteOut-entry"));

  ptr addop   = Stop_level_value( Sstring_to_symbol("WSQ_AddOp-entry"));

  ptr shutdwn = Stop_level_value( Sstring_to_symbol("WSQ_Shutdown-entry"));

  ptr setout     = Stop_level_value( Sstring_to_symbol("WSQ_SetOutputFile-entry"));
  ptr setquery   = Stop_level_value( Sstring_to_symbol("WSQ_SetQueryName-entry"));
  ptr setbackend = Stop_level_value( Sstring_to_symbol("WSQ_SetBackend-entry"));

  ptr pause    = Stop_level_value( Sstring_to_symbol("WSQ_Pause-entry"));
  ptr unpause  = Stop_level_value( Sstring_to_symbol("WSQ_Unpause-entry"));
  

  Scheme_BeginTransaction = (intfun)Sinteger_value(tbegin);
  Scheme_EndTransaction   = (int(*)())Sinteger_value(tend);

  Scheme_BeginSubgraph    = (intfun)Sinteger_value(gbegin);
  Scheme_EndSubgraph      = (voidfun)Sinteger_value(gend);
  Scheme_RemSubgraph      = (intfun)Sinteger_value(grem);
  Scheme_EdgeType         = (ptr(*)(int))Sinteger_value(gtyp);

  Scheme_AddProject       = (void(*)(int,int,int,const char*))Sinteger_value(addpro);
  Scheme_AddFilter        = (void(*)(int,int,int,const char*))Sinteger_value(addfil);
  Scheme_AddWindowJoin    = (void(*)(int,int,int,int,float,const char*, const char*, const char*))Sinteger_value(addwin);

//  Scheme_AddReutersSource = (void(*)(int,int,float,const char*))Sinteger_value(addsrc);
  Scheme_AddPrinter       = (void(*)(int,const char*,int))Sinteger_value(addprn);

  Scheme_ConnectRemoteIn  = (void(*)(int,int,const char*,int,const char*))Sinteger_value(con_in);
  Scheme_ConnectRemoteOut = (void(*)(int,int,const char*,int))Sinteger_value(con_out);

  Scheme_AddOp            = (void(*)(int,const char*,const char*,const char*,const char*))Sinteger_value(addop);

  Scheme_Shutdown         = (void(*)())Sinteger_value(shutdwn);
  Scheme_SetOutputFile    = (void(*)(const char*))Sinteger_value(setout);

  Scheme_SetQueryName     = (void(*)(const char*))Sinteger_value(setquery);
  Scheme_SetBackend       = (void(*)(enum WSQBackend mode))Sinteger_value(setbackend);

  Scheme_Pause      = (void(*)())Sinteger_value(pause);
  Scheme_Unpause    = (int(*)()) Sinteger_value(unpause);

  // ============================================================
  if (verbose>=1) printf(" <WSQ> Bringing up WSQ runtime system (forking threads)...\n");

  // fork some threads...

  //
  Scheme_SetOutputFile(outfile);

  if (verbose>=1) printf(" <WSQ> Returning to control module...\n");  
}


void WSQ_SetQueryName(const char* name) {
   Scheme_SetQueryName(name);
}

void WSQ_SetBackend(enum WSQBackend mode) {
   Scheme_SetBackend(mode);
}

void WSQ_Pause()   { Scheme_Pause(); }
int  WSQ_Unpause() { return Scheme_Unpause(); }


//int main(int argc, char* argv[]) { do_scheme(argc,argv); }
