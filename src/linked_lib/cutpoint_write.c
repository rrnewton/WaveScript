


void WRITE_CUTPOINT_0(uint8_t* arr, int len) {
  //printf("WRITE CUTPOINT: %p %d\n", arr, len);
  //fwrite((void*)&len, 4, 1, stderr);
  fwrite(arr, 1, len, stderr);
  fflush(stderr);
}
