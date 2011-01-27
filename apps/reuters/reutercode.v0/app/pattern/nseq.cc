#include "Seq.h"

#define PUSH_DOWN
//#define ON_TOP

int main(int argc, char* argv[]) {

  int args[3] = { 0, 1, 200 };
  for(int i=1; i<argc; i++)
    args[i] = atoi(argv[i]);
    
  // @Test pattern:
  // ................................
  // [IBM, !Sun, Oracle]
  

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
  //  WHERE IBM.price > Sun.price + 1
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
  recbuffers[1].setneg(true);
  recbuffers[2].setflist(&list2);
 
#ifdef PUSH_DOWN

  NSeq neg(&recbuffers[1], &recbuffers[2]);
  NJoin root(&recbuffers[0], &neg);
  Seq nseq(mypattern, &s, recbuffers, &root);
  nseq.open();
  while(!s.to_end()) {
    Rec_iter iter = nseq.next(bunch);
    //cout << lseq.nextstring() << endl;
    //cout << "Result: " << endl;
    /*for(; iter != root.end(); iter++) {    
      //if((*iter)->filtertw(tw));
      cout << (*iter)->tostring() << endl;
      }*/
    root.clear();
  } 
  nseq.close();
#else

#ifdef ON_TOP
  MLJoin join(&recbuffers[0], &recbuffers[2]);
  Neg root(&recbuffers[1], &join);
  Seq nseq(mypattern, &s, recbuffers, &root);
  nseq.open();
  while(!s.to_end()) {
    Rec_iter iter = nseq.next(bunch);
    //cout << lseq.nextstring() << endl;
    //cout << "Result: " << endl;
    /*for(; iter != root.end(); iter++) {    
      //if((*iter)->filtertw(tw));
      cout << (*iter)->tostring() << endl;
      }*/
    root.clear();
  } 
  nseq.close();

#endif
#endif

  return 1;
}
