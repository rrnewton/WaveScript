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

type ConfTable = List (String * String);

namespace ConfFile {

  using List;

  // Is it a newline or tab/space?
  fun is_whitespace(c) {
    num = charToInt(c);
    9  == num ||
    32 == num ||
    10 == num
  }

  fun tokenize(ls) List:splitBy(ls, is_whitespace);

  fun is_comment_or_blank(line) {
    let (_, rest) = scan(line, is_whitespace);
    if rest.is_null
    then true
    else rest.head == '#'
  }

  // Trim whitespace from both sides of a token:
  fun rempadding(ls) {
    let (_,rest) = scan(ls, is_whitespace);
    let (_,rev) = scan(rest.reverse, is_whitespace);
    rev.reverse;
  }

  fun extract_keyvalue(line) {
    tokens1 = split(line, '=');
    if tokens1.length == 2
    then (tokens1.head.rempadding`String:implode, 
          tokens1.tail.head.rempadding`String:implode)
    else {
      tokens2 = filter((!= []), split(line, ' '));
      if tokens2.length == 2
      then (tokens2.head.rempadding . String:implode, 
            tokens2.tail.head.rempadding . String:implode)
      else wserror("extract_keyvalue: couldn't parse line: " ++ String:implode(line));
    }
  }

  fun read_conffile(file) {
    str = fileToString(file);
    ls  = String:explode(str);
    //lines = split(ls, '\n'); // This syntax doesn't work yet.
    lines   = split(ls, intToChar(10)); // Newline.
    entries = filter(compose(not, is_comment_or_blank), lines);
    map(extract_keyvalue, entries)
  }

  fun conf_get(table, key) {
    
  }
}

read_conffile :: String -> ConfTable;
read_conffile = ConfFile:read_conffile;

main = iterate _ in timer(3) {
  List:foreach(fun(x) emit x, read_conffile("camera.conf"))
}
