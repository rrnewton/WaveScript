
// EXPERIMENTAL:

// This is an interface into Lewis's networking code.
// It generates C code to connect to specified hosts/streams

fun entry(host, cnst) ("
  void entry_"++host++"() {
    wsentry_"++host++"("++cnst++");
  }
")

fun init (host) ("
  void init_"++host++"() {
    entry_"++host++"();
  }
")

//     printf(\"Initialization..."++host++"\\n\");

fun netsource(host, files, cnst) {
  ccode = inline_C(entry(host,cnst) ++ init(host), "init_"++host);
  src = foreign_source("wsentry_"++host, files);
  merge(ccode, src)
}

toplevel = inline_C( "
 void wsmain() { }
", "")

strm1 = (netsource("foo", [], "34")   :: Stream Int);
strm2 = (netsource("bar", [], "59.8") :: Stream Float);

s1b = iterate i in strm1 {
  print("        Got int from src1: "++i++"\n");
  emit ();
}

s2b = iterate f in strm2 {
  print("        Got float from src2: "++f++"\n");
  emit ();
}

BASE <- merge(toplevel, merge(s1b,s2b));
