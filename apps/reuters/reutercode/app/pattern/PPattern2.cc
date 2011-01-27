#include "Seq.h"

//#define BUSHY
//#define LEFT
#define RIGHT

int main() {
  
  // @Test pattern:
  // ................................
  // [IBM, Sun, Oracle, Google]
  //  WHERE IBM.price > Sun.price
  //  AND   Oracle.price < Google
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
  // [IBM, Sun, Oracle, Google]
  //  WHERE IBM.price > Sun.price + 1
  //  AND   Oracle.price < Google.price - 1
  //.................................
  EventTypePtr pattern[4];
  pattern[0] = &type;
  pattern[1] = &type;
  pattern[2] = &type;
  pattern[3] = &type;
  int len = 4;
  SeqPattern mypattern = SeqPattern(pattern, len, 200);  
  
  // FPredlist
  FPredList list0;
  list0.add_fpred(type.fieldtype(1), "IBM", EQUAL, 1);
  FPredList list1;
  list1.add_fpred(type.fieldtype(1), "Sun", EQUAL, 1);
  FPredList list2;
  list2.add_fpred(type.fieldtype(1), "Oracle", EQUAL, 1);
  FPredList list3;
  list3.add_fpred(type.fieldtype(1), "Google", EQUAL, 1);
  
  FileSource s(&type, "testdata/mytest.dat");
  RecBuffer recbuffers[mypattern.patlen()];
  //  uint tw = mypattern.tw();
  recbuffers[0].setflist(&list0);
  recbuffers[1].setflist(&list1);
  recbuffers[2].setflist(&list2);
  recbuffers[3].setflist(&list3);
 
#ifdef BUSHY  
  // @Bushy plan 
  // add PPredlist
  Opr bopr0(0, 2), bopr1(0, 2), bopr2(0, 2), bopr3(0, 2);
  float bp = 1;
  PPredList blist0, blist1;
  blist0.add_ppred(bopr0, GREATER_THAN, bopr1, type.fieldtype(2), (char*)&bp, PLUS);
  blist1.add_ppred(bopr2, LESS_THAN, bopr3, type.fieldtype(2), (char*)&bp, SUB);
      
  MLJoin blp0(&recbuffers[0], &recbuffers[1], &blist0);
  MLJoin blp1(&recbuffers[2], &recbuffers[3], &blist1);
  MLJoin broot(&blp0, &blp1);
  Seq bseq(mypattern, &s, recbuffers, &broot);
  bseq.open();
  while(!s.to_end()) {
    Rec_iter iter = bseq.next(10);      
    /*for(; iter != broot.end(); iter++) {
      //  if((*iter)->filtertw(tw))
      cout << (*iter)->tostring() << endl;
      }*/
    broot.clear();
  } 
  bseq.close(); 
#else 

#ifdef LEFT  	  
  // @Left depth plan
  // PPredList
  Opr lopr0(0, 2), lopr1(0, 2), lopr2(2, 2), lopr3(0, 2);
  float lp = 1;
  PPredList llist0, llist1;
  llist0.add_ppred(lopr0, GREATER_THAN, lopr1, type.fieldtype(2), (char*)&lp, PLUS);
  llist1.add_ppred(lopr2, LESS_THAN, lopr3, type.fieldtype(2), (char*)&lp, SUB);

  MLJoin llp0(&recbuffers[0], &recbuffers[1], &llist0);
  MLJoin llp1(&llp0, &recbuffers[2]);
  MLJoin lroot(&llp1, &recbuffers[3], &llist1);
  Seq lseq(mypattern, &s, recbuffers, &lroot);
  lseq.open();
  while(!s.to_end()) {
    Rec_iter iter = lseq.next(10);      
    /*for(; iter != lroot.end(); iter++) {
      //  if((*iter)->filtertw(tw))
      cout << (*iter)->tostring() << endl;
      }*/
    lroot.clear();
  } 
  lseq.close();
#else

#ifdef RIGHT	    
  // @right depth plan
  // add PPredList
  Opr ropr0(0, 2), ropr1(0, 2), ropr2(0, 2), ropr3(0, 2);
  float rp = 1;
  PPredList rlist0, rlist1;
  rlist0.add_ppred(ropr0, LESS_THAN, ropr1, type.fieldtype(2), (char*)&rp, SUB);
  rlist1.add_ppred(ropr2, GREATER_THAN, ropr3, type.fieldtype(2), (char*)&rp, PLUS);

  MLJoin rlp0(&recbuffers[2], &recbuffers[3], &rlist0);
  MLJoin rlp1(&recbuffers[1], &rlp0);
  MLJoin rroot(&recbuffers[0], &rlp1, &rlist1);
  Seq rseq(mypattern, &s, recbuffers, &rroot);
  rseq.open();
  while(!s.to_end()) {
    Rec_iter iter = rseq.next(10);      
    /*for(; iter != rroot.end(); iter++) {
      //  if((*iter)->filtertw(tw))
      cout << (*iter)->tostring() << endl;
      }*/
    rroot.clear();
  } 
  rseq.close();
#endif // RIGHT
#endif // LEFT
#endif // BUSHY
  
  return 1;
}
