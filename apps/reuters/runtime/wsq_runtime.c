
#include <stdio.h>

#include "wsq_runtime.h"

// This version connects to Chez.

//const char *S_date_stamp = "10102009080511";
const char *S_date_stamp = "0";

#include "scheme.h"

//==============================================================================
// First, function pointers to all of the relevant calls.  These point
// to scheme functions.

void (*Scheme_BeginTransaction)(id_t id);
void (*Scheme_EndTransaction)();

void (*Scheme_BeginSubgraph)(id_t id);
void (*Scheme_EndSubgraph)();
void (*Scheme_RemSubgraph)(id_t id);

void (*Scheme_AddReutersSource)(id_t id, char* path);
void (*Scheme_AddPrinter)(id_t id);
void (*Scheme_AddProject)(id_t in, id_t out, char* expr);
void (*Scheme_AddFilter) (id_t in, id_t out, char* expr);

void (*Scheme_ConnectRemoteOut) (id_t out, char* host, int port);
void (*Scheme_ConnectRemoteIn)  (id_t in,  char* host, int port);

//==============================================================================
/* The functions exposed through the C API are just wrappers for the
   above scheme functions.  The only reason that these are separate
   (annoying I know) is that in our wsq_runtime.h header file we don't
   want to expose the fact that we are using function pointers.
*/

void WSQ_BeginTransaction(id_t id) { Scheme_BeginTransaction(id); }
void WSQ_EndTransaction()          { Scheme_EndTransaction(); }

void WSQ_BeginSubgraph(id_t id) { Scheme_BeginSubgraph(id); }
void WSQ_EndSubgraph()          { Scheme_EndSubgraph(); }
void WSQ_RemSubgraph(id_t id)   { Scheme_RemSubgraph(id); }

void WSQ_AddReutersSource(id_t id, char* path) { Scheme_AddReutersSource(id, path); } 
void WSQ_AddPrinter(id_t id)                   { Scheme_AddPrinter(id); }

void WSQ_AddProject(id_t in, id_t out, char* expr) { Scheme_AddProject(in,out,expr); } 
void WSQ_AddFilter (id_t in, id_t out, char* expr) { Scheme_AddFilter(in,out,expr); }

void WSQ_ConnectRemoteOut (id_t out, char* host, int port) { Scheme_ConnectRemoteOut(out,host,port); }
void WSQ_ConnectRemoteIn  (id_t in, char* host, int port)  { Scheme_ConnectRemoteIn (in,host,port); }

//==============================================================================

typedef void (*intfun) (int);
typedef void (*voidfun) ();

//void foo* (int);
intfun foo_fun;


//int do_scheme(int argc, char* argv[]) {
void WSQ_Init() {

  // void Sscheme_init(void (*abnormal_exit)(void))
  Sscheme_init(0);  
  Sregister_boot_file("~/bin/Linux-i686/csv7.9.2/boot/i3le/petite.boot");
  Sregister_boot_file("~/bin/Linux-i686/csv7.9.2/boot/i3le/scheme.boot");

  Sbuild_heap(".", 0);
  //Sbuild_heap(argv[0], 0);

  Senable_expeditor(0);

  const char* regimentd = getenv("REGIMENTD");
  char script[1000];
  sprintf(script, "%s/apps/reuters/runtime/load_interface.ss", regimentd);

  // NOTE: This is a real oddity.  I must *pad* the argument list??
  // Seems like there might be a bug.
  const char* new_args[] = {"", script};

  //const char* new_args[] = {"", script, "--quiet"};
  //const char* new_args[] = {"", "--quiet", script};

  // Tell chez scheme not to print the greeting:
  // (Doesn't work for me presently.)
  //Scall1(Stop_level_value( Sstring_to_symbol("suppress-greeting")), Sfalse);

  printf(" <WSQ> Starting Scheme runtime system.\n");
  int result = Sscheme_start(2, new_args);
  //int result = Sscheme_start(3, new_args);
  //printf("Scheme_start finished\n");

  // This is awful gross.  Oh well.
  // ========================================

  printf(" <WSQ> Registering WSQ runtime interface with control module.\n");

  // Grab the relevant function entrypoints from the Scheme symbol table.
  ptr tbegin =  Stop_level_value( Sstring_to_symbol("WSQ_BeginTransaction-entry"));
  ptr tend   =  Stop_level_value( Sstring_to_symbol("WSQ_EndTransaction-entry"));

  ptr gbegin =  Stop_level_value( Sstring_to_symbol("WSQ_BeginSubgraph-entry"));
  ptr gend   =  Stop_level_value( Sstring_to_symbol("WSQ_EndSubgraph-entry"));
  ptr grem   =  Stop_level_value( Sstring_to_symbol("WSQ_RemSubgraph-entry"));

  ptr addsrc =  Stop_level_value( Sstring_to_symbol("WSQ_AddReutersSource-entry"));
  ptr addprn =  Stop_level_value( Sstring_to_symbol("WSQ_AddPrinter-entry"));
  ptr addfil =  Stop_level_value( Sstring_to_symbol("WSQ_AddFilter-entry"));
  ptr addpro =  Stop_level_value( Sstring_to_symbol("WSQ_AddProject-entry"));

  ptr con_in  = Stop_level_value( Sstring_to_symbol("WSQ_ConnectRemoteIn-entry"));
  ptr con_out = Stop_level_value( Sstring_to_symbol("WSQ_ConnectRemoteOut-entry"));

  Scheme_BeginTransaction = (intfun) Sinteger_value(tbegin);
  Scheme_EndTransaction   = (voidfun)Sinteger_value(tend);

  Scheme_BeginSubgraph    = (intfun)Sinteger_value(gbegin);
  Scheme_EndSubgraph      = (voidfun)Sinteger_value(gend);
  Scheme_RemSubgraph      = (intfun)Sinteger_value(grem);

  Scheme_AddProject       = (void(*)(int,int,char*))Sinteger_value(addpro);
  Scheme_AddFilter        = (void(*)(int,int,char*))Sinteger_value(addfil);
  Scheme_AddReutersSource = (void(*)(int,char*))Sinteger_value(addsrc);
  Scheme_AddPrinter       = (intfun)Sinteger_value(addprn);

  Scheme_ConnectRemoteIn  = (void(*)(int,char*,int))Sinteger_value(con_in);
  Scheme_ConnectRemoteOut = (void(*)(int,char*,int))Sinteger_value(con_out);


  // ========================================
  printf(" <WSQ> Bringing up WSQ runtime system (forking threads)...\n");

  // fork some threads...

  printf(" <WSQ> Returning to control module...\n");
  
}

void WSQ_Shutdown() {
  Sscheme_deinit();
}


//int main(int argc, char* argv[]) { do_scheme(argc,argv); }
