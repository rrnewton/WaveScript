#include "FileSource.h"

int main() {

  Catalog ct;

  const char* fnames[3];
  FieldType ftypes[3];
  fnames[0] = "id";
  fnames[1] = "name";
  fnames[2] = "price";
  ftypes[0] = FieldType(INT);
  ftypes[1] = FieldType(STRING);
  ftypes[2] = FieldType(FLOAT);
		
  // @Test EventType
  EventType type("Trade", 3, (const char**)fnames, ftypes);
			
  // @Test file source
  FileSource bs(0, &type, "testdata/mytest.dat");
  bs.setsockbuffer();

  bs.open(&ct);

  /*
  EventPtr ep = bs.next();
  ep->print(cout, &type);

  ep = bs.next();
  ep->print(cout, &type);*/

  cout << "while output" << endl;
  while(bs.hasNext()) {
    bs.next()->print(cout, &type);
  }

  bs.sockbuffer()->print(cout, &type);

  bs.close(&ct);

  return 0;

}
 	  
