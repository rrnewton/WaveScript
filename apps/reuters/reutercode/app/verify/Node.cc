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

  // FPredlist: name = IBM, price > 58.65
  FPredList list0;
  list0.add_fpred(type.fieldtype(1), "IBM", EQUAL, 1);
  float p0 = 32.65;
  list0.add_fpred(type.fieldtype(2), (char*)&p0, GREATER_THAN, 2);

  FileSource src(0, &type, "testdata/mytest.dat");   // File Source
  Filter flt(1, &type, &src);                        // Filter 
  flt.setflist(&list0);  

  //flt.setsockbuffer();

  Node n(2020, "piquin", &flt);
  n.start();
  
  pthread_exit(NULL);
}
