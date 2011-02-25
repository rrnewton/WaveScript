

WS Query Library (WSQ)
--------------------------------------------------------------------------------

This directory contains an implementation of a higher-level query
interface on top of WaveScript.

The runtime/ directory contains the magic that makes this work.  

To build it:
    cd runtime; make

To test it:
    cd runtime; make; make test


--------------------------------------------------------------------------------
  WSQ_AddOp -- adding operators to the graph
--------------------------------------------------------------------------------

Operators can be added to an executing graph using a single call,
"WSQ_AddOp" that packs the description of the operator into some
string arguments.  

  void WSQ_AddOp(wsid_t id, const char* optype, const char* inputs,
                 const char* outputs, int numargs, const char** args);

id -- integegral ID of the node being added

optype -- operator type such as "Project" or "Filter"

inputs  -- space separated name of integer stream IDs
outputs -- space separated name of integer stream IDs

args -- 
        This is the catch-all string into which everything that
        parameterizes an operator is packed.  It follows the silly
        convention that multiple strings are packed in, delimeted by
        "|".

--------------------------------------------------------------------------------
  Supported stream operators and their syntax.        
--------------------------------------------------------------------------------

"RandomSource" -- a source of stock tick tuples
 0-inputs 1-output 2 string arguments: 
   (1) frequency of source in hertz (floating point)
   (2) file name of schema file containing tuple format (UNUSED CURRENTLY)
Currently the schema of random source is fixed:


"ASCIIFileSource" -- stock ticks read from a ascii TAQ reuters file
Schema:



"Printer" -- print every tuple on a stream
 1-input 0-output 1 string argument:
   (1) String prefix to append to messages printed

"Filter" -- remove some tuples on a stream, forward others
  1-input 1-output 1 string argument:
   (1) comma deliminated list of PREDICATES (syntax described below)
   
"Project" -- map a projection function over the tuple stream, somewhat
             akin to a SELECT statement.
  1-input 1-output, 1 string argument:
   (1) a comma separated list of PROJECTION FIELDS (syntax described below)


"Window" -- 

"FilterWindows" -- 

"Join" --

"MatchRecognize" -- Corresponds roughly to the MATCH_RECOGNIZE
		    operator in CQL.
  1 input 1 output, 3 string arguments:
   (1) The string "ONE" ore "ALL" referring to the "ROWS PER MATCH" setting.
       Currently [2010.06.28], ALL creates a stream of arrays of tuples.
   (2) A "regular expression" of predicates:
       These are encoded as a space separated list of predicate names.
       Currently [2010.06.28] only basic patterns are supported.

   (3) Comma-separated list of predicate definions.  For example:       
         "B AS B.PRICE >= A.PRICE"
                
  FEATURES TO BE SUPPORTED SOON:

     Basic regeexps with "star" and disjunction.  Disjunction is
     written with a double pipe "||".  For example: "A B* (C || D)"

     Support the special function PREV that refers to the value in the
     previous tuple.
  

"ConnectRemoteIn" -- An incoming TCP socket.
  0-input 1-output, 3 string arguments:
   (1) host name
   (2) port
   (3) tuple schema, a comma-separated list of field types.
       For example:
        "STRING SYM, FLOAT TIME, FLOAT PRICE, INT VOLUME"

"ConnectRemoteOut" -- An outgoing TCP socket.
  1-input 0-output, 2 string arguments:
   (1) host name
   (2) port



----------------------------------------
The below are partially working and need some further work.
They also need to be organized better:

"WindowJoin" -- 
TIMESTAMP_WINDOW 
TIMESTAMP_WINDOW_GROUPBY 
TIMESTAMP_WINDOW_GROUPBY_MINSLIDE
REALTIME_WINDOW
REWINDOW_GROUPBY
TIMESTAMP_JOIN



--------------------------------------------------------------------------------
  PREDICATE syntax
--------------------------------------------------------------------------------

A predicate is just an <expression>.  Parentheses are currently
mandatory for nested expressions.  Examples:

  (PRICE > 2) AND ((SYM = "IBM") OR (SYM = "GOOG"))

  (A.TIME > B.TIME) 


--------------------------------------------------------------------------------
  PROJECTION FIELD syntax
--------------------------------------------------------------------------------

  A projection field has one of two forms:
     <var>
or   <expression> AS <var>



--------------------------------------------------------------------------------
  ENVIRONMENT VARS
--------------------------------------------------------------------------------

Aside from passing arguments to the WSQ API explicitly through C API
calls, it is also possible to configure the behavior of the WSQ stream
engine through environment variables:

  WSQ_MAXSPEED -- if set, turn off realtime timers and run all out
  WSQ_OUTEXE   -- base file name to use for generating output 
                  .c/.exe files (in the background).

  WSQ_OPTLVL   -- 0-3, controls optimization of generated C code.

  WSQ_VERBOSE -- 0-2, control ammount of output from runtime system 

  WSQ_BACKEND -- select WS code generator to use, currently only "C" supported.

  WSQ_OUTPUTFILE -- Cause the final output of stream engine queries to be
                    written to a file.  Overrided by the argument to WSQ_Init.

  WSQ_NOFLUSH -- set nonempty to disable flushing after stream
                 elements are printed to a file.

----------------------------------------------------------------------------------------------------



