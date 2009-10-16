#include "FileSource.h"

/* Test for Source, with socketbuffer = 0*/
int main() {

  Catalog ct;
  int count = 0;

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
  //  bs.setsockbuffer();
  bs.open(&ct);
  while(true) {
    while(bs.hasNext()) {
      bs.next()->print(cout, &type);   
      count++;
    }
    usleep(SLEEPTIME);
    printf("count : %d\n", count);
  } 

  // the remaining tuple left in the buffer;
  //while(bs.hasNext()) {
  //  bs.next()->print(cout, &type);
  //}
  
  bs.close(&ct);
  pthread_exit(NULL);
  //return 0;
}
 	  
