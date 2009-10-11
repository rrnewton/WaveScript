
#include <stdio.h>

#include "wsq_runtime.h"

// This version connects to Chez.

//const char *S_date_stamp = "10102009080511";
const char *S_date_stamp = "0";

#include "scheme.h"

void WSQ_Init() {}
void WSQ_Shutdown() {}


//void WSQ_EndTransaction() {}

void WSQ_BeginSubgraph(id_t id) {
}

void WSQ_EndSubgraph() {
}

void WSQ_AddSubgraph() {
}

void WSQ_RemSubgraph(id_t id) {
}

void WSQ_AddProject(id_t in, id_t out, char* expr) {
}

void WSQ_AddFilter (id_t in, id_t out, char* expr) {
}

void WSQ_ConnectOutTCP (id_t in, char* host, int port) {
}

void WSQ_ConnectInTCP  (id_t in, int port) {
}

void (*WSQ_BeginTransaction)(id_t id);
void (*WSQ_EndTransaction)();

typedef void (*intfun) (int);
typedef void (*voidfun) ();

//void foo* (int);
intfun foo_fun;


int do_scheme(int argc, char* argv[]) {
  printf("yay\n");

  // void Sscheme_init(void (*abnormal_exit)(void))
  Sscheme_init(0);

  printf("scheme initialized\n");
  
  Sregister_boot_file("~/bin/Linux-i686/csv7.9.2/boot/i3le/petite.boot");
  Sregister_boot_file("~/bin/Linux-i686/csv7.9.2/boot/i3le/scheme.boot");

  printf("boot files registered\n");

  Sbuild_heap(argv[0], 0);

  printf("heap built\n");
  
  Senable_expeditor(0);
  //  Sscheme_start(argc, argv);

  /*
  const char* regimentd = getenv("REGIMENTD");
  char script[1000];
  char* pwd = getcwd(0,0);

  sprintf(script, "%s/src/regiment.ss", regimentd);
  printf("  regiment.ss located: %s\n", script);
  printf("  pwd: %s\n", pwd);

  //const char* new_args[] = {"--script", script, pwd, ""};
  //const char* new_args[] = {pwd, ""};

  // Either of these two work... and that makes no sense:
  //const char* new_args[] = {pwd};
  const char* new_args[] = {"",pwd, "nothing"};

  chdir(regimentd);
  chdir("src");

  printf("Now starting scheme:\n\n");
  //Sscheme_start(3, new_args);
  // RRN: It very much seems that this should be one!
  Sscheme_script(script, 3, new_args);
  */

  const char* regimentd = getenv("REGIMENTD");
  char script[1000];
  sprintf(script, "%s/apps/reuters/runtime/load_interface.ss", regimentd);
  const char* new_args[] = {"", script};
  int result = Sscheme_start(2, new_args);

  printf("Scheme started (and probably finished)\n");


  // ======================================================================
  ptr sym = Sstring_to_symbol("test");

  printf("  sym is a sym %d\n", Ssymbolp(sym));
  ptr test =  Stop_level_value( sym );
  ptr foo =  Stop_level_value( Sstring_to_symbol("foo-entry") );

  printf("TEST\n");

  printf("Is it fixnum %d \n", Sfixnump(test));
  printf("Is it bignum %d \n", Sbignump(test));
  printf("Is it exactnum %d \n", Sexactnump(test));

  printf("Got some values from the toplevel, test: %d.\n", Sfixnum_value(test));

  //printf("Got some values from the toplevel, foo: %d.\n", foo);
  printf("Got some values from the toplevel, foo: %ud.\n", Sinteger_value(foo));

  foo_fun = (intfun)Sinteger_value(foo);

  printf("  Set foo fun: %ud\n", foo_fun);
  foo_fun(993);
  printf("  Finished calling foo fun!\n");
  // ======================================================================
  
  ptr tbegin =  Stop_level_value( Sstring_to_symbol("WSQ_BeginTransaction-entry"));
  ptr tend   =  Stop_level_value( Sstring_to_symbol("WSQ_EndTransaction-entry"));

  WSQ_BeginTransaction = (intfun) Sinteger_value(tbegin);
  WSQ_EndTransaction   = (voidfun)Sinteger_value(tend);

  WSQ_BeginTransaction(99);
  WSQ_EndTransaction();

  Sscheme_deinit();
}


//int main(int argc, char* argv[]) { do_scheme(argc,argv); }
