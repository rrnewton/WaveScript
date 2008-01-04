

// [2007.12.06] This is the header that goes with my new C backend. -Ryan

//int* arrayMake(size_t size, int len, ) { }

int outputcount = 0;
int tuplimit = 10;

void BASE(char x) { 
  outputcount++;
  if (outputcount >= tuplimit) exit(0);
}
