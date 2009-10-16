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

  Client c(2020, "128.30.76.165", "1,0", 0, &type);

  Node n(2022, "piquin", &c);
  n.addclient(&c);
  
  n.start();
  pthread_exit(NULL);
}
