#include "Seq.h"

#define LEFT
//#define RIGHT

int main(int argc, char* argv[]) {

  int args[3] = { 0, 1, 200 };
  for(int i=1; i<argc; i++)
    args[i] = atoi(argv[i]);
    
  // @Test pattern:
  // ................................
  // [IBM, Sun, Oracle]  

  // parameters
  int bunch = args[1];
  uint tw = args[2];  

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
  
  // SeqPattern
  // ................................
  // [IBM, Sun, Oracle]
  //.................................

  EventTypePtr pattern[3];
  pattern[0] = &type;
  pattern[1] = &type;
  pattern[2] = &type;
  int len = 3;
  SeqPattern mypattern = SeqPattern(pattern, len, tw);  
  
  // FPredlist
  FPredList list0;
  list0.add_fpred(type.fieldtype(1), "IBM", EQUAL, 1);
  FPredList list1;
  list1.add_fpred(type.fieldtype(1), "Sun", EQUAL, 1);
  FPredList list2;
  list2.add_fpred(type.fieldtype(1), "Oracle", EQUAL, 1);
  
  FileSource s(&type, "testdata/mytest.dat");
  RecBuffer recbuffers[mypattern.patlen()];

  recbuffers[0].setflist(&list0);
  recbuffers[1].setflist(&list1);
  recbuffers[2].setflist(&list2);
 
#ifdef LEFT  	  
  // @Left depth plan
  MLJoin llp0(&recbuffers[0], &recbuffers[1]);
  MLJoin lroot(&llp0, &recbuffers[2]);
  Seq lseq(mypattern, &s, recbuffers, &lroot);
  lseq.open();
  while(!s.to_end()) {
    Rec_iter iter = lseq.next(bunch);
    //cout << lseq.nextstring() << endl;
    //cout << "Result: " << endl;
    /*for(; iter != lroot.end(); iter++) {    
      //if((*iter)->filtertw(tw))
      cout << (*iter)->tostring() << endl;
      }*/
    lroot.clear();
  } 
  lseq.close();
#else

#ifdef RIGHT	    
  // @right depth plan
  // add PPredList
  MLJoin rlp0(&recbuffers[1], &recbuffers[2]);
  MLJoin rroot(&recbuffers[0], &rlp0);
  Seq rseq(mypattern, &s, recbuffers, &rroot);
  rseq.open();
  while(!s.to_end()) {
    Rec_iter iter = rseq.next(bunch);    
    //cout << rseq.nextstring() << endl;  
    //cout << "Result: " << endl;
    /*for(; iter != rroot.end(); iter++) {
      //if((*iter)->filtertw(tw))
      cout << (*iter)->tostring() << endl;
      }*/
    rroot.clear();
  } 
  rseq.close();
#endif // RIGHT
#endif // LEFT  
  return 1;
}
