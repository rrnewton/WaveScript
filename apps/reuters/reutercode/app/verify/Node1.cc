#include "Node.h"

int main() {

  // construct the query plan 
  const char* fnames[3];
  FieldType ftypes[3];
  fnames[0] = "id";
  fnames[1] = "name";
  fnames[2] = "price";
  ftypes[0] = INT;
  ftypes[1] = STRING;
  ftypes[2] = FLOAT;
                
  // @Test EventType
  EventType type("Trade", 3, (const char**)fnames, ftypes);
  FileSource src(0, &type, "testdata/mytest.dat");   // File Source

  Node n(2020, "piquin", &src);

  // initialize the catalog: operatormap
  n.catalog()->operatormap[0] = &src;

  n.start();
  pthread_exit(NULL);
}
