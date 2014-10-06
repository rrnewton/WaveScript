

See INSTALL for installation.



Documentation
======================================================================

In addition to this README, there are a number of papers published on
Regiment/WaveScript.  Further, there is the code-documentation.
Finally, there is the WaveScript user Manual.

The code-documentation is generated by Schemedoc.  
Please see doc/README.


Overview of implementation
======================================================================

Regiment/WaveScript is written in Scheme.  The system has recently
[2008.05.08] been ported to R6RS (with some additional
language-dependent facilities isolated in compat.sls).

The code is divided into modules ("libraries"), with one module per file.

Intermediate programs within the compiler are simply represented as
S-Expressions -- nested lists of symbols, for example:

    (foo-language '(program '3 Int))

The key tool used by the compiler for deconstructing programs is
"match", a pattern matching macro.  It uses antiquote (",") to
introduce pattern variables within a pattern:

     (match '(1 2)
      [(,x ,y ,z) 9]
      [(88 ,x) (+ w x)]
      [(,w ,x) (+ w x)])

...will evaluate to 3.

There are also a number of other important tools accumulated over the
years.  There is a mechanism for automatically traversing the syntax
of an AST (so that writing a new pass means only handling the few
cases that matter), as well as syntactic sugar for defining passes
(see pass-mechanism.ss).

You can play around with any of the functions exported by the
Regiment/WaveScript code by simply executing "regiment i" to run an
interactive read-eval-print loop.

For editing the source in emacs, emacs/regiment.el is recommended.  In
emacs, run "M-x run-scheme" to open up an interactive scheme session,
and prefix the M-x command with "ctrl-u" to change the executable to
"regiment i".


URGENT note on code: Things in transition.
======================================================================

Certain things are in the process of happening.  As a result the
inconsistencies might be confusing.

 * Reformatting comments for Schemedoc is a work-in-progress.
 * Windows support comes and goes.  I don't use it actively but
    there's no major barrier to it.


Coding Conventions
======================================================================

[2007.09.17] {TEMPTOGGLE}
I'm starting to use the symbol TEMPTOGGLE to tag things that I toggle
and need to remember to *toggle back*.  It's been a recurring problem
that I will turn, say, profiling off, and then check it in without
remembering to turn it off.  Thus, in a good checkin, the symbol
TEMPTOGGLE should not occur in the code.

[2005.02.13]
I have tagged all my little shorthand commands for invoking the system with a
";; shorthand" comment.  By searching for this I should be able to find all the 
shorthands.  These are esoteric, and will make the system confusing to
others, but you should just be able to ignore them.



Basic Summary of Folders
======================================================================

 * `src/`
  There are only a few files in this directory.  Entry points to the
  system, configuration code, a Makefile, and this README.

 * `src/ws/`
  The vast majority of the code, used by all ports of the system.

 * `src/linked_lib/`
  Header files and other code written in external languages and
  linked into the output of the WaveScript compiler.

 * `src/C/`
  C extensions to the Scheme source.


Basic Summary of Files
======================================================================
This doesn't cover everything.

 * `main_r6rs.sls`:
  Automatically generated aggregate of all libraries.

 * `main.sls`:
  This defines the compiler as a composition of the individual
  passes.  It contains run-compiler for invoking the system.  (It
  also, in spite of the name, contains a front-end to the simulator.
  Really, it's the "main" file.)

 * `config.ss`:
  Configuration options factored out because one might want to change
  them manually.

 * `regiment.ss`:
  A top-level program for invoking regiment from the command line.

 * `ws/passes/*`:
  These files are the compilers primary transformations.

 * `ws/langs/*`:
  These files define executable simulations of the between-pass
  languages.  We do have a simple, non-node-level, simulation that can
  be run even before the token machine simulator.  But these lang files
  are not used extensively and will be phased out.

 * `ws/testing/system_tests.ss`:
  The primary system tests.

 * `ws/globals.sls`:
  Widely used constants and tunable parameters for the system.

 * `ws/util/helpers.sls`:
  Library code.  This might have unused stuff in it.

 * `ws/compiler_components/prim_defs.sls`:
  The specification for every primitive in the Regiment/WaveScript systems.

 * `ws/compiler_components/regiment_helpers.sls`:
  Various helper routines specifically for working with Regiment/WS/TML code.
  
 * `ws/compiler_components/hm_type_inference.sls`:
  The type checker.
    

WHERE STUFF IS
======================================================================

Unit Tests:
-----------

The unit test executor itself is in ws/testing/unit_tester.ss

Every file that has unit tests defines its own tester function.  The
function "test-units" runs (or should) all the unit tests in the
system.

I have made some effort to ensure that a unit-tests will execute using
only the file in question.  Most multi-module tests counts as a
"system test", and go in system_tests.ss.  However, in a bunch of
places this doesn't quite make sense, so I believe this convention is
beginning to slip in certain places. -rrn [2005.09.24]
 
NOTE on TESTING: There are a number of toggles and parameters in the
system (mostly concentrated in globals.ss).  I am following a
convention where I sprinkle the exact phrase "TOGGLE FOR UNIT TESTING"
throughout the code.  When unit testing, you should be able to change
the values of these parameters and have it complete its tests.  In
fact, to test thoroughly, you should exercise the system in all of the
major configurations.
( [2007.03.12] Note, with the much newer "supertest" system, I hope to
at some point automatically permute major system parameters during
nightly unit tests. )


Debugging tips
======================================================================

 * Whenever trying to use Chez's interactive debugger (using "(debug)"),
you should remember to go to the top of compiler_chez.ss and set the
optimize-level to 0.  Otherwise inlining might result in you not
seeing stack frames in the inspector that you expect to see.
  [With WaveScript, simply run ws.debug - 2007.03.12]

 * Chez's interactive inspecter is good, use "(break)" or 
"(call/cc inspect)" liberally (sometimes the latter is what you need).

 * If you're using SWL and you get the cryptic "invalid command name "0""
 error, this probably means that you're attempting a method call on a
deleted object.  Alas I can't remember exactly what causes this.  <TODO>


Profiling instructions
======================================================================

Chez Scheme has very good profiling support.  And as of version 7.4,
Chez supports "profile-dump-html", which produces a very nice website
describing profiling results.

    ;; You can turn on profiling for a chunk of code like this:
    (eval-when (compile eval load) (compile-profile #t))
    ;; ... CODE ...
    (eval-when (compile eval load) (compile-profile #f))

    ;; The old way of dumping/viewing profiling data was to do this:
    (with-output-to-file "./pdump_new"  (lambda () (fasl-write (profile-dump)))  'replace)

    ;; Then to view it using SWL:
    (begin 
      (import swl:module-setup)
      (import swl:oop)
      (import swl:macros)
      (import swl:option)
      (import swl:threads)
      (import swl:generics)  
    ;  (load "/usr/lib/swl1.0c/apps/profview/profview.ss")
       (load "~/wavescript/src/chez/rn_profview.ss")
      (cd "~/wavescript/src/"))

    (p-view (with-input-from-file "/tmp/pdump" read))


Environment variables affecting operation
======================================================================

See the file ./bin/ws_opts.txt for a description of environment
variables that affect the WS compiler.

The most important one for setup purposes is "WAVESCRIPTD" which is set
by the "install_environment_vars" script in this directory.

