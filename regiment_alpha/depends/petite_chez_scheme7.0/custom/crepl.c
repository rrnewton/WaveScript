/* crepl.c
 * Copyright (c) 2000 Cadence Research Systems
 */

/* test with:
 *   make m=<machine type> Custom=crepl.o
 *   ./scheme
 */

#include "scheme.h"
#include <stdio.h>

/****
   This is a variant of custom.c that implements a Scheme repl in C.
   It's not at all useful, but it highlights how to invoke Scheme
   without going through Sscheme_start.
****/

#define CALL0(who) Scall0(Stop_level_value(Sstring_to_symbol(who)))
#define CALL1(who, arg) Scall1(Stop_level_value(Sstring_to_symbol(who)), arg)

static void custom_init(void) {}

int main(int argc, char *argv[]) {
  int n, new_argc = 1, ignoreflags = 0;
  char *heap_search_path = (char *)0;
  int compact = 1, savefile_level; char *savefile = "";
  ptr p;


  Sscheme_init();

 /* process command-line arguments, registering boot and heap files */
  for (n = 1; n < argc; n += 1) {
    if (!ignoreflags && *argv[n] == '-') {
      switch (*(argv[n]+1)) {
        case '-': /* pass through remaining options */
          if (*(argv[n]+2) != 0) break;
          ignoreflags = 1;
          continue;
        case 'b': /* boot option, expects boot file pathname */
          if (*(argv[n]+2) != 0) break;
          if (++n == argc) {
            (void) fprintf(stderr,"\n-b option requires argument\n");
            exit(1);
          }
          Sregister_boot_file(argv[n]);
          continue;
        case 'c': /* compaction toggle option */
          if (*(argv[n]+2) != 0) break;
          compact = !compact;
          continue;
        case 'h': /* heap option, expects heap file pathname */
          if (*(argv[n]+2) != 0) break;
          if (++n == argc) {
            (void) fprintf(stderr,"\n-h option requires argument\n");
            exit(1);
          }
          Sregister_heap_file(argv[n]);
          continue;
        case 's': /* save heap option, expects heap file pathname */
          if (*(argv[n]+2) != 0) {
            if (*(argv[n]+3) != 0) break;
            savefile_level = *(argv[n]+2) - '0';
            if (savefile_level < 0 || savefile_level > 9) break;
          } else {
              savefile_level = 0;
          }
          if (++n == argc) {
            (void) fprintf(stderr,"\n-s option requires argument\n");
            exit(1);
          }
          savefile = argv[n];
          continue;
        default:
          break;
      }
    }
    argv[new_argc++] = argv[n];
  }

 /* must call Sscheme_heap after registering boot and heap files
  * Sscheme_heap() completes the initialization of the Scheme system
  * and loads the boot or heap files.  Before loading boot files,
  * it calls custom_init(). */
  Sbuild_heap(argv[0], custom_init);

  for (;;) {
      CALL1("display", Sstring("* "));
      p = CALL0("read");
      if (Seof_objectp(p)) break;
      p = CALL1("eval", p);
      if (p != Svoid) CALL1("pretty-print", p);
  }
  CALL0("newline");

  if (*savefile != (char)0) {
      if (compact) Scompact_heap();
      Ssave_heap(savefile, savefile_level);
  }

 /* must call Scheme_deinit after saving the heap and before exiting */
  Sscheme_deinit();

  exit(0);
}
