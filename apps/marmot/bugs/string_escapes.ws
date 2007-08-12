

BASE <- iterate x in timer(3.0) {
  str1 = "x";
  str = "TEST \""++str1++"\" TEST";
  emit(str);
};