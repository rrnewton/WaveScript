#include "FileSource.h"
#include "Filter.h"


int main() {
  Catalog ct; 
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

  cout << list0.tostring() << endl;

  FPredList list1;
  list1.add_fpred(type.fieldtype(1), "IBM", CONTAINS, 1);
  cout << list1.tostring() << endl;
  
  FileSource src(0, &type, "testdata/mytest.dat");   // File Source
  Filter flt(1, &type, &src);                           // Filter 
  flt.setflist(&list0);
  //flt.setflist(&list1);

  flt.setsockbuffer();

  flt.open(&ct);
  while(flt.hasNext()) {
    EventPtr ep = flt.next();      
    ep->print(cout, &type);
  }

  flt.sockbuffer()->print(cout, &type);
  flt.close(&ct); 

  return 0;

}
 	  
