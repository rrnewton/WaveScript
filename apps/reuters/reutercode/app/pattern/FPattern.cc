#include "Seq.h"

#define BUSHY
//#define LEFT
//#define RIGHT

int main() {

  // @Test simple pattern comparison	
  // ................................
  //  [IBM1, Sun, Oracle, IBM2]
  //   WHERE IBM1.price > 58.65
  //   AND   Sun.price < 34.85
  //   AND   Oracle.price < 34.85
  //   AND   IBM2.price > 58.65
  //.................................
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
  //  [IBM1, Sun, Oracle, IBM2]
  //   WHERE IBM1.price > 58.65
  //   AND   Sun.price < 34.85
  //   AND   Oracle.price < 34.85
  //   AND   IBM2.price > 58.65
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
  float p0 = 58.65;
  list0.add_fpred(type.fieldtype(2), (char*)&p0, GREATER_THAN, 2);
	  
  FPredList list1;
  list1.add_fpred(type.fieldtype(1), "Sun", EQUAL, 1);
  float p1 = 34.85;
  list1.add_fpred(type.fieldtype(2), (char*)&p1, LESS_THAN, 2);
  
  FPredList list2;
  list2.add_fpred(type.fieldtype(1), "Oracle", EQUAL, 1);
  float p2 = 34.85;
  list2.add_fpred(type.fieldtype(2), (char*)&p2, LESS_THAN, 2);
	    
  FPredList list3;
  list3.add_fpred(type.fieldtype(1), "IBM", EQUAL, 1);
  float p3 = 58.65;
  list3.add_fpred(type.fieldtype(2), (char*)&p3, GREATER_THAN, 2);

  FileSource s(&type, "testdata/mytest.dat");
  RecBuffer recbuffers[mypattern.patlen()];
  uint tw = mypattern.tw();
  recbuffers[0].setflist(&list0);
  recbuffers[1].setflist(&list1);
  recbuffers[2].setflist(&list2);
  recbuffers[3].setflist(&list3);

#ifdef BUSHY
  // @Bushy plan
  MLJoin blp0(&recbuffers[0], &recbuffers[1]);
  MLJoin blp1(&recbuffers[2], &recbuffers[3]);
  MLJoin broot(&blp0, &blp1);
  Seq bseq(mypattern, &s, recbuffers, &broot);
  bseq.open();
  while(!s.to_end()) {
    Rec_iter iter = bseq.next(10);      
    for(; iter != broot.end(); iter++) {
      if((*iter)->filtertw(tw))
	cout << (*iter)->tostring() << endl;
    }
    broot.clear();
  } 
  bseq.close(); 
#else 

#ifdef LEFT  
  // @Left depth plan
  MLJoin llp0(&recbuffers[0], &recbuffers[1]);
  MLJoin llp1(&llp0, &recbuffers[2]);
  MLJoin lroot(&llp1, &recbuffers[3]);
  Seq lseq(mypattern, &s, recbuffers, &lroot);
  lseq.open();
  while(!s.to_end()) {
    Rec_iter iter = lseq.next(10);      
    for(; iter != lroot.end(); iter++) {
      if((*iter)->filtertw(tw))
	cout << (*iter)->tostring() << endl;
    }
    lroot.clear();
  }
  lseq.close();
#else

#ifdef RIGHT
  // @right depth plan
  MLJoin rlp0(&recbuffers[2], &recbuffers[3]);
  MLJoin rlp1(&recbuffers[1], &rlp0);
  MLJoin rroot(&recbuffers[0], &rlp1);
  Seq rseq(mypattern, &s, recbuffers, &rroot);
  rseq.open();
  while(!s.to_end()) {
    Rec_iter iter = rseq.next(10);      
    for(; iter != rroot.end(); iter++) {
      if((*iter)->filtertw(tw))
	cout << (*iter)->tostring() << endl;
    }
    rroot.clear();
  } 
  rseq.close();
#endif // RIGHT
#endif // LEFT
#endif // BUSHY

  return 1;

}
 	  
