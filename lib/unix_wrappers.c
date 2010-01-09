
//#include <wsc2.h>
#include <dirent.h>
#include <stdio.h>
#include <sys/stat.h>

#include "ws.h"

struct dirent *** ws_namelist_ptr() {
  return malloc(sizeof(struct dirent **));
}

int scandir_sorted(const char* dirstr, struct dirent *** namelst) {
  return scandir(dirstr, namelst, 0, alphasort);
}

char* dirent_getname(struct dirent *** namelist, int ind) {
  return (*namelist)[ind]->d_name;
}

/*
// This returns a WS string that is deallocated by the WS GC.
// [2008.10.23] WSARRAYALLOC_CHAR is having problems under mlton on 64-bit platforms...
ws_string_t getname(struct dirent *** namelist, int ind) {
  char* c_str = (*namelist)[ind]->d_name;
  int len = strlen(c_str);
  // Allocate on the WS heap.
  ws_char_t* ws_str = WSARRAYALLOC_CHAR(len+1);
  // This assumes that WS chars are the same as C chars:
  memcpy(ws_str, c_str, len+1); // Copy the null terminator.
  // The pointer to a WS object loses validity as soon as we let go of
  // it (it may be moved by WS).
  return ws_str; // Give it over to the WS heap.
}
*/

void freenamelist(struct dirent *** namelist, int len) {
  int i;
  for(i=0; i<len; i++)
    free((*namelist)[i]);
  free((*namelist));
  free(namelist);
}

void* arrayToPointer (void* arr) { return arr; }
char* stringToPointer(char* str) { return str; }

struct stat* ws_make_stat() {
  return malloc(sizeof(struct stat));
}

int ws_st_size(struct stat* ptr) {  return ptr->st_size; }


