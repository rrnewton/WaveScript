#include "Event.h"

int main() {
  //@Test Event
  const char* fnames_e[2];
  FieldType ftypes_e[2];
  fnames_e[0] = "id";
  fnames_e[1] = "name";
  ftypes_e[0] = FieldType(INT);
  ftypes_e[1] = FieldType(STRING);

  /* test for Event(uint ts, EventType* etptr, const char** values)		
   * binary -->
   */
  int id_e = 12;
  const char* name_e = "yuan";
		
  char values_e[S_SIZE+4];
  memcpy(values_e, (char*)&id_e, 4);
  memcpy(values_e+4, name_e, S_SIZE);
		
  EventType type_e("Test", 2, (const char**)fnames_e, ftypes_e);
  cout << type_e.tostring() << endl;
	

  Event e_e(values_e, 34);	
  e_e.print(cout, &type_e);

  /* test for Event(EventType* etptr, string* values)	
   * ascii -->
   */
  string values_t[2];
  values_t[0] = "24";
  values_t[1] = "yuan_t";
  Event e_t(&type_e, values_t);
  e_t.print(cout, &type_e);
  return 0;
}
