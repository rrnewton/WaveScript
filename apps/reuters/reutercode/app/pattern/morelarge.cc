#include "Seq.h"

//#define LEFT_PRED
//#define RIGHT_PRED
//#define LEFT
#define RIGHT

int main(int argc, char* argv[]) {

  int args[3] = { 0, 1, 100 };
  for(int i=1; i<argc; i++)
    args[i] = atoi(argv[i]);
    
  // @Test pattern:
  // ................................
  // [IBM, Sun, Oracle, Google, Yahoo]
  //  WHERE Google.price > Oracle.price
  //  AND Google.price > Yahoo.price

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
  //  WHERE Google.price > Oracle.price
  //  AND Google.price > Yahoo.price
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
 
#ifdef LEFT_PRED  	  
  // @evaluate the left-pred first [[IBM; Sun]; [[Oracle; Google]; Yahoo]]
  // PPredList
  Opr opr02(0, 2), opr03(0, 2);  // (eventIdx, attriIdx)
  PPredList list23;
  list23.add_ppred(opr02, LESS_THAN, opr03);
  MLJoin lp0(&recbuffers[2], &recbuffers[3], &list23);

  Opr opr13(1, 2), opr14(0, 2);  // (eventIdx, attriIdx)
  PPredList list34;
  list34.add_ppred(opr13, GREATER_THAN, opr14);
  MLJoin lp1(&lp0, &recbuffers[4], &list34);

  MLJoin lp2(&recbuffers[0], &recbuffers[1]);
  MLJoin root(&lp2, &lp1);
  Seq seq(mypattern, &s, recbuffers, &root);
  seq.open();
  while(!s.to_end()) {
    Rec_iter iter = seq.next(bunch);
    for(; iter != root.end(); iter++) {    
      //if((*iter)->filtertw(tw));
      //cout << (*iter)->tostring() << endl;
    }
    root.clear();
  } 
  seq.close();
#else

#ifdef RIGHT_PRED 
  //@evaluate the right-pred first [[IBM; Sun]; [Oracle; [Google; Yahoo]]]
  
  Opr opr03(0, 2), opr04(0, 2);  // (eventIdx, attriIdx)
  PPredList list34;
  list34.add_ppred(opr03, GREATER_THAN, opr04);
  MLJoin lp0(&recbuffers[3], &recbuffers[4], &list34);

  Opr opr12(0, 2), opr13(0, 2);  // (eventIdx, attriIdx)
  PPredList list23;
  list23.add_ppred(opr12, LESS_THAN, opr13);
  MLJoin lp1(&recbuffers[2], &lp0, &list23);

  MLJoin lp2(&recbuffers[0], &recbuffers[1]);
  MLJoin root(&lp2, &lp1);
  Seq seq(mypattern, &s, recbuffers, &root);
  seq.open();
  while(!s.to_end()) {
    Rec_iter iter = seq.next(bunch);
    for(; iter != root.end(); iter++) {    
      //if((*iter)->filtertw(tw));
      //cout << (*iter)->tostring() << endl;
    }
    root.clear();
  } 
  seq.close();
#else

#ifdef LEFT	    
  // @left depth plan  [[[[IBM; Sun]; Oracle]; Google]; Yahoo]
  // add PPredList
  MLJoin lp0(&recbuffers[0], &recbuffers[1]);
  MLJoin lp1(&lp0, &recbuffers[2]);

  Opr opr02(2, 2), opr03(0, 2);  // (eventIdx, attriIdx)
  PPredList list23;
  list23.add_ppred(opr02, LESS_THAN, opr03);
  MLJoin lp2(&lp1, &recbuffers[3], &list23);

  Opr opr13(3, 2), opr14(0, 2);  // (eventIdx, attriIdx)
  PPredList list34;
  list34.add_ppred(opr13, GREATER_THAN, opr14);
  MLJoin root(&lp2, &recbuffers[4], &list34);
  
  Seq seq(mypattern, &s, recbuffers, &root);
  seq.open();
  while(!s.to_end()) {
    Rec_iter iter = seq.next(bunch);    
    for(; iter != root.end(); iter++) {
      //if((*iter)->filtertw(tw)) ;
      //cout << (*iter)->tostring() << endl;
    }
    root.clear();
  } 
  seq.close();
#else

#ifdef RIGHT	    
  // @right depth plan    [IBM; [Sun; [Oracle; [Google; Yahoo]]]]
  // add PPredList
  Opr opr03(0, 2), opr04(0, 2);  // (eventIdx, attriIdx)
  PPredList list34;
  list34.add_ppred(opr03, GREATER_THAN, opr04);
  MLJoin lp0(&recbuffers[3], &recbuffers[4], &list34);
  
  Opr opr12(0, 2), opr13(0, 2);  // (eventIdx, attriIdx)
  PPredList list23;
  list23.add_ppred(opr12, LESS_THAN, opr13);
  MLJoin lp1(&recbuffers[2], &lp0, &list23);

  MLJoin lp2(&recbuffers[1], &lp1);
  MLJoin root(&recbuffers[0], &lp2);
  Seq seq(mypattern, &s, recbuffers, &root);
  seq.open();
  while(!s.to_end()) {
    Rec_iter iter = seq.next(bunch);    
    for(; iter != root.end(); iter++) {
      //      if((*iter)->filtertw(tw)) ;
      //cout << (*iter)->tostring() << endl;
    }
    root.clear();
  } 
  seq.close();

#endif // LEFT-PRED
#endif // RIGHT-PRED
#endif // LEFT
#endif // RIGHT  
  return 1;
}
