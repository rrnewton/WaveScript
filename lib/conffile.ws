/*
  [2008.08.19]

  Reading configuration files is a very common need.  This file
  defines utilities for reading text files full of key/value bindings
  that are either space separated or separated by '='.

   KEY VAL
   KEY = VAL

  The files may also contain whitespace lines and comments prefixed with '#'.

  The main export of this library is "read_conffile" which returns a
  table of key/value pairs.  The type of this table should be treated
  as abstract.  Currently it's implemented as an array, but it very
  much should be a hash table, as soon as all the backends uniformly
  support hash tables.

  The client of this library uses "conf_get" to access the table.

 */

include "stdlib.ws"
include "unix.ws"

type ConfTable = Array (String * String);

namespace ConfFile {

  // Is it a newline or tab/space?
  fun is_whitespace(c) {
    num = charToInt(c);
    9  == num ||
    32 == num ||
    10 == num
  }

  fun tokenize(ls) List:splitBy(ls, is_whitespace);

  fun read_conffile(file) {
    str = ""; //fileToString(file);
    //    lines = spl
    #[]
  }

  fun conf_get(table, key) {
    
  }
}

read_conffile :: String -> ConfTable;
read_conffile = ConfFile:read_conffile;

