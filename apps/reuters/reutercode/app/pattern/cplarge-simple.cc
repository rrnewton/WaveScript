#include "Seq.h"

#define LEFT
//#define RIGHT

int main(int argc, char* argv[]) {

  int args[3] = { 0, 1, 100 };
  for(int i=1; i<argc; i++)
    args[i] = atoi(argv[i]);
    
  // @Test pattern:
  // ................................
  // [IBM, Sun, Oracle, Google, Yahoo]
  //  WHERE Google.price > Yahoo.price
  

  // parameters
  int bunch = args[1];
  uint tw = args[2];  

  char* fnames[3];
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
  // [IBM, Sun, Oracle, Google, Yahoo]
  //  WHERE Google.price > Yahoo.price
  //.................................

  EventTypePtr pattern[5];
  pattern[0] = &type;
  pattern[1] = &type;
  pattern[2] = &type;
  pattern[3] = &type;
  pattern[4] = &type;

  int len = 5;
  SeqPattern mypattern = SeqPattern(pattern, len, tw);  
  
  // FPredlist
  FPredList list0;
  list0.add_fpred(type.fieldtype(1), "IBM", EQUAL, 1);
  FPredList list1;
  list1.add_fpred(type.fieldtype(1), "Sun", EQUAL, 1);
  FPredList list2;
  list2.add_fpred(type.fieldtype(1), "Oracle", EQUAL, 1);
  FPredList list3;
  list3.add_fpred(type.fieldtype(1), "Google", EQUAL, 1);
  FPredList list4;
  list4.add_fpred(type.fieldtype(1), "Yahoo", EQUAL, 1);
  
  FileSource s(&type, "testdata/mytest.dat");
  RecBuffer recbuffers[mypattern.patlen()];

  recbuffers[0].setflist(&list0);
  recbuffers[1].setflist(&list1);
  recbuffers[2].setflist(&list2);
  recbuffers[3].setflist(&list3);
  recbuffers[4].setflist(&list4);
 
#ifdef LEFT	    
  // @left depth plan
  MLJoin llp0(&recbuffers[0], &recbuffers[1]);
  MLJoin llp1(&llp0, &recbuffers[2]);
  MLJoin llp2(&llp1, &recbuffers[3]);

  // add PPredList
  Opr lopr00(3, 2), lopr01(0, 2);  // (eventIdx, attriIdx)
  PPredList llist0;
  llist0.add_ppred(lopr00, GREATER_THAN, lopr01);
  MLJoin lroot(&llp2, &recbuffers[4], &llist0);
  
  Seq lseq(mypattern, &s, recbuffers, &lroot);
  lseq.open();
  while(!s.to_end()) {
    Rec_iter iter = lseq.next(bunch);    
    //cout << rseq.nextstring() << endl;  
    //cout << "Result: " << endl;
    /*for(; iter != lroot.end(); iter++) {
      //if((*iter)->filtertw(tw)) ;
      cout << (*iter)->tostring() << endl;
      }*/
    lroot.clear();
  } 
  lseq.close();

#else

#ifdef RIGHT	    
  // @right depth plan
  // add PPredList
  Opr ropr20(0, 2), ropr21(0, 2);  // (eventIdx, attriIdx)
  PPredList rlist2;
  rlist2.add_ppred(ropr20, GREATER_THAN, ropr21);
  MLJoin rlp0(&recbuffers[3], &recbuffers[4], &rlist2);

  MLJoin rlp1(&recbuffers[2], &rlp0);
  MLJoin rlp2(&recbuffers[1], &rlp1);  
  MLJoin rroot(&recbuffers[0], &rlp2);

  Seq rseq(mypattern, &s, recbuffers, &rroot);
  rseq.open();
  while(!s.to_end()) {
    Rec_iter iter = rseq.next(bunch);    
    //cout << rseq.nextstring() << endl;  
    //cout << "Result: " << endl;
    /*for(; iter != rroot.end(); iter++) {
      //      if((*iter)->filtertw(tw)) ;
      cout << (*iter)->tostring() << endl;
      }*/
    rroot.clear();
  }
  rseq.close();


#endif // RIGHT
#endif // LEFT  
  return 1;
}
