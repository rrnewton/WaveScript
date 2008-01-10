

// [2007.12.06] This is the header that goes with my new C backend. -Ryan

//int* arrayMake(size_t size, int len, ) { }

#define TRUE  1
#define FALSE 0

#define CAR(ptr) (*ptr)
#define CDR(ptr) (((void**)ptr)[-2])

int outputcount = 0;
int tuplimit = 10;

void BASE(char x) { 
  outputcount++;
  if (outputcount >= tuplimit) exit(0);
}

void wserror(char* msg) {
  //error(msg);
  printf("Failed with error: %s\n", msg);
  exit(-1);
}


/*
// TODO:
int Listlength(void* list) {
  int acc = 0;
  printf("List len... %p\n", list);
  while (list != 0) {
    list = CDR(list);
    acc++;
  }
  return acc; 
}
*/

/*
// TODO:
void* Listappend(void* ls1, void* ls2) {
  printf("List append... %p and %p\n", ls1, ls2);
  return ls1;
}

// TODO:
void* Listreverse(void* ls) {
  printf("List reverse... %p\n", ls);
  return ls;
}
*/


// This won't work:
/*
int Listref(void* list, int n) {
  return 0; 
}
*/
