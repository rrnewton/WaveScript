
//#include <wsc2.h>
#include <dirent.h>

struct dirent *** ws_namelist_ptr() {
  return malloc(sizeof(struct dirent **));
}

int scandir_sorted(const char* dirstr, struct dirent *** namelst) {
  return scandir(dirstr, namelst, 0, alphasort);
}

// This returns a WS string that 
ws_string_t getname(struct dirent *** namelist, int ind) {
  char* c_str = (*namelist)[ind]->d_name;
  int len = strlen(c_str);
  ws_char_t* ws_str = WSARRAYALLOC(len+1, ws_char_t);
  // This assumes that WS chars are the same as C chars:
  memcpy(ws_str, c_str, len+1); // Copy the null terminator.
  return ws_str; // Give it over to the WS heap.
}

void freenamelist(struct dirent *** namelist, int len) {
  int i;
  for(i=0; i<len; i++)
    free((*namelist)[i]);
  free((*namelist));
  free(namelist);
}
