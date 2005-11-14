/* custom.c
 * Copyright (c) 2000 Cadence Research Systems
 */

/****
  This is the default custom.c file defining main, which must be present
  in order to build an executable file.

  See the file custom/sample.c for a customized variant of this file.
****/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "scheme.h"

/****
  CUSTOM_INIT may be defined as a function with the signature shown to
  perform boot-time initialization, e.g., registering foreign symbols.
****/
#ifndef CUSTOM_INIT
#define CUSTOM_INIT ((void (*)(void))0)
#endif /* CUSTOM_INIT */

/****
  ABNORMAL_EXIT may be defined as a function with the signature shown to
  take some action, such as printing a special error message or performing
  a nonlocal exit with longjmp, when the Scheme system exits abnormally,
  i.e., when an unrecoverable error occurs.  If left null, the default
  is to call exit(1).
****/
#ifndef ABNORMAL_EXIT
#define ABNORMAL_EXIT ((void (*)(void))0)
#endif /* ABNORMAL_EXIT */

int main(int argc, char *argv[]) {
  int n, new_argc = 1;
  char *heap_search_path = (char *)0;
  int booting = 0, compact = 1, savefile_level; char *savefile = (char *)0;
  char *scriptfile = (char *)0;
  int status;
  char *arg;
  int quiet = 0;

  Sscheme_init(ABNORMAL_EXIT);

 /* process command-line arguments, registering boot and heap files */
  for (n = 1; n < argc; n += 1) {
    arg = argv[n];
    if (strcmp(arg,"--") == 0) {
      while (++n < argc) argv[new_argc++] = argv[n];
    } else if (strcmp(arg,"-b") == 0 || strcmp(arg,"--boot") == 0) {
      if (++n == argc) {
        (void) fprintf(stderr,"\n%s requires argument\n", arg);
        exit(1);
      }
      booting = 1;
      Sregister_boot_file(argv[n]);
    } else if (strcmp(arg,"-c") == 0 || strcmp(arg,"--compact") == 0) {
      compact = !compact;
    } else if (strcmp(arg,"-h") == 0 || strcmp(arg,"--heap") == 0) {
      if (++n == argc) {
        (void) fprintf(stderr,"\n%s requires argument\n", arg);
        exit(1);
      }
      Sregister_heap_file(argv[n]);
    } else if (strcmp(arg,"-q") == 0 || strcmp(arg,"--quiet") == 0) {
      quiet = 1;
    } else if (strncmp(arg,"-s",2) == 0 &&
               (savefile_level = -2,
                *(arg+2) == 0 ||
                *(arg+3) == 0 &&
                ((savefile_level = *(arg+2) - '+' - 1) == -1 ||
                  (savefile_level = *(arg+2) - '0') >= 0 &&
                   savefile_level <= 9)) ||
               strncmp(arg,"--saveheap",10) == 0 &&
               (savefile_level = -2,
                *(arg+10) == 0 ||
                *(arg+11) == 0 &&
                ((savefile_level = *(arg+2) - '+' - 1) == -1 ||
                  (savefile_level = *(arg+10) - '0') >= 0 &&
                   savefile_level <= 9))) {
      if (++n == argc) {
        (void) fprintf(stderr,"\n%s requires argument\n", arg);
        exit(1);
      }
      savefile = argv[n];
    } else if (strcmp(arg,"--script") == 0) {
      if (++n == argc) {
        (void) fprintf(stderr,"\n%s requires argument\n", arg);
        exit(1);
      }
      scriptfile = argv[n];
      while (++n < argc) argv[new_argc++] = argv[n];
    } else if (strcmp(arg,"--help") == 0) {
      fprintf(stderr,"usage: %s [options and files]\n", argv[0]);
      fprintf(stderr,"options:\n");
      fprintf(stderr,"  -b <path>, --boot <path>                load boot file\n");
      fprintf(stderr,"  -c, --compact                           toggle compaction flag\n");
      fprintf(stderr,"  -h <path>, --heap <path>                load heap file\n");
      fprintf(stderr,"  -q, --quiet                             suppress greeting and prompt\n");
      fprintf(stderr,"  -s[<n>] <path>, --saveheap[<n>] <path>  save heap file\n");
      fprintf(stderr,"  --script <path>                         run as shell script\n");
      fprintf(stderr,"  --verbose                               trace boot/heap search process\n");
      fprintf(stderr,"  --version                               print version and exit\n");
      fprintf(stderr,"  --help                                  print help and exit\n");
      fprintf(stderr,"  --                                      pass through remaining args\n");
      exit(0);
    } else if (strcmp(arg,"--verbose") == 0) {
      Sset_verbose(1);     
    } else if (strcmp(arg,"--version") == 0) {
      fprintf(stderr,"%s\n", VERSION);
      exit(0);
    } else {
      argv[new_argc++] = arg;
    }
  }

 /* must call Sbuild_heap after registering boot and heap files.
  * Sbuild_heap() completes the initialization of the Scheme system
  * and loads the boot or heap files.  If no boot or heap files have
  * been registered, the first argument to Sbuild_heap must be a
  * non-null path string; in this case, Sbuild_heap looks for
  * a heap or boot file named <name>.boot, where <name> is the last
  * component of the path.  If no heap files are loaded and
  * CUSTOM_INIT is non-null, Sbuild_heap calls CUSTOM_INIT just
  * prior to loading the boot file(s). */
  Sbuild_heap(argv[0], CUSTOM_INIT);

#define CALL0(who) Scall0(Stop_level_value(Sstring_to_symbol(who)))
#define CALL1(who, arg) Scall1(Stop_level_value(Sstring_to_symbol(who)), arg)
#ifdef FunCRepl
  {
    ptr p;

    for (;;) {
        CALL1("display", Sstring("* "));
        p = CALL0("read");
        if (Seof_objectp(p)) break;
        p = CALL1("eval", p);
        if (p != Svoid) CALL1("pretty-print", p);
    }
    CALL0("newline");
    status = 0;
  }
#else
  if (quiet) {
    CALL1("suppress-greeting", Strue);
    CALL1("waiter-prompt-string", Sstring(""));
  }

  if (scriptfile != (char *)0)
   /* Sscheme_script invokes the value of the scheme-script parameter */
    status = Sscheme_script(scriptfile, new_argc, argv);
  else
   /* Sscheme_start invokes the value of the scheme-start parameter */
    status = Sscheme_start(new_argc, argv);
#endif

  if (status == 0 && savefile != (char *)0) {
      if (compact) Scompact_heap();
      Ssave_heap(savefile, savefile_level);
  }

 /* must call Scheme_deinit after saving the heap and before exiting */
  Sscheme_deinit();

  exit(status);
}
