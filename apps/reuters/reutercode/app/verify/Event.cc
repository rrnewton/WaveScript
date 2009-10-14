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
  /*int id_e = 12;
  const char* name_e = "yuan";
		
  const char* values_e[2];
  values_e[0] = (char*)&id_e;	
  values_e[1] = name_e;*/
		
  EventType type_e("Test", 2, (const char**)fnames_e, ftypes_e);
  cout << type_e.tostring() << endl;
	

  //Event e_e(&type_e, (const char**)values_e);	
  //cout << e_e.tostring();

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
