#include "Seq.h"
#include <sys/time.h>

//#define LEFT
//#define RIGHT
//#define BUSHY
#define INNERRIGHT
//#define INNERLEFT

int main(int argc, char* argv[]) {

  int args[4] = {0, 1, 100, 20000};
  for(int i=1; i<argc; i++)
    args[i] = atoi(argv[i]);
    
  // @Test pattern:
  // ................................
  // [IBM, Sun, Oracle, Google]
  //  WHERE Oracle.price > Google.price
  //  AND Oracle.price > Sun.price

  // parameters
  int bunch = args[1];
  uint tw = args[2];  
  int num = args[3];

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
  //  WHERE Oracle.price > Google.price
  //  AND Oracle.price > Sun.price
  //.................................
  EventTypePtr pattern[4];
  pattern[0] = &type;
  pattern[1] = &type;
  pattern[2] = &type;
  pattern[3] = &type;  

  int len = 4;
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
    
  FileSource s(&type, "testdata/mytest.dat");
  RecBuffer recbuffers[mypattern.patlen()];

  recbuffers[0].setflist(&list0);
  recbuffers[1].setflist(&list1);
  recbuffers[2].setflist(&list2);
  recbuffers[3].setflist(&list3);  
 
#ifdef LEFT	    
  // @left depth plan  [[[IBM; Sun]; Oracle]; Google]
  // add PPredList
  MLJoin lp0(&recbuffers[0], &recbuffers[1]);
  
  Opr opr02(1, 2), opr03(0, 2);  // (eventIdx, attriIdx)
  PPredList list23;
  list23.add_ppred(opr02, LESS_THAN, opr03);
  MLJoin lp1(&lp0, &recbuffers[2], &list23);

  Opr opr13(2, 2), opr14(0, 2);  // (eventIdx, attriIdx)
  PPredList list34;
  list34.add_ppred(opr13, GREATER_THAN, opr14);
  MLJoin root(&lp1, &recbuffers[3], &list34);
  
  Seq seq(mypattern, &s, recbuffers, &root);

  int interval = num; int times = 0; 
  timeval start, end; int next = 0;
  gettimeofday(&start, NULL);

  seq.open();
  while(!s.to_end()) {
   
    Rec_iter iter = seq.next(bunch);        
    /*for(; iter != root.end(); iter++) {
      //if((*iter)->filtertw(tw)) ;
      cout << (*iter)->tostring() << endl;
      }*/
    root.clear();

    if(times < 2 && s.to_end(interval)) {
      times++; interval += num; 
      gettimeofday(&end, NULL);
      double t1 = start.tv_sec+(start.tv_usec/1000000.0);
      double t2 = end.tv_sec+(end.tv_usec/1000000.0);
      cout << "time:" << t2 - t1 << endl;
      cout << "datanum: " << s.current()-next << endl;  
      cout << "throughput: " << (s.current()-next)/(t2-t1) << endl;
      next = s.current();
      gettimeofday(&start, NULL);
    }    
  } 

  seq.close();
  gettimeofday(&end, NULL);
  double t1 = start.tv_sec+(start.tv_usec/1000000.0);
  double t2 = end.tv_sec+(end.tv_usec/1000000.0);
  cout << "time:" << t2 - t1 << endl;
  cout << "datanum: " << s.current()-next << endl;  
  cout << "throughput: " << (s.current()-next)/(t2-t1) << endl;

  //while(true) {}

#else

#ifdef RIGHT	    
  // @right depth plan    [IBM; [Sun; [Oracle; Google]]]
  // add PPredList
  Opr opr03(0, 2), opr04(0, 2);  // (eventIdx, attriIdx)
  PPredList list34;
  list34.add_ppred(opr03, GREATER_THAN, opr04);
  MLJoin lp0(&recbuffers[2], &recbuffers[3], &list34);
  
  Opr opr12(0, 2), opr13(0, 2);  // (eventIdx, attriIdx)
  PPredList list23;
  list23.add_ppred(opr12, LESS_THAN, opr13);
  MLJoin lp1(&recbuffers[1], &lp0, &list23);

  MLJoin root(&recbuffers[0], &lp1);
  Seq seq(mypattern, &s, recbuffers, &root);

  int interval = num; int times = 0;   
  timeval start, end; int next = 0;
  gettimeofday(&start, NULL);

  seq.open();
  while(!s.to_end()) {
    Rec_iter iter = seq.next(bunch);    
    /*for(; iter != root.end(); iter++) {
      //      if((*iter)->filtertw(tw)) ;
      cout << (*iter)->tostring() << endl;
      }*/
    root.clear();

    if(times < 2 && s.to_end(interval)) {
      times++; interval += num; 
      gettimeofday(&end, NULL);
      double t1 = start.tv_sec+(start.tv_usec/1000000.0);
      double t2 = end.tv_sec+(end.tv_usec/1000000.0);
      cout << "time:" << t2 - t1 << endl;
      cout << "datanum: " << s.current()-next << endl;  
      cout << "throughput: " << (s.current()-next)/(t2-t1) << endl;
      next = s.current();
      gettimeofday(&start, NULL);
    }   
  } 
  seq.close();
  gettimeofday(&end, NULL);
  double t1 = start.tv_sec+(start.tv_usec/1000000.0);
  double t2 = end.tv_sec+(end.tv_usec/1000000.0);
  cout << "time:" << t2 - t1 << endl;
  cout << "datanum: " << s.current()-next << endl;  
  cout << "throughput: " << (s.current()-next)/(t2-t1) << endl;

  //while(true) {}
#else
  
#ifdef BUSHY 
  //@evaluate the right-pred first [[IBM; Sun]; [Oracle; Google]]
  MLJoin lp0(&recbuffers[0], &recbuffers[1]);
  
  Opr opr03(0, 2), opr04(0, 2);  // (eventIdx, attriIdx)
  PPredList list34;
  list34.add_ppred(opr03, GREATER_THAN, opr04);
  MLJoin lp1(&recbuffers[2], &recbuffers[3], &list34);

  Opr opr12(1, 2), opr13(0, 2);  // (eventIdx, attriIdx)
  PPredList list23;
  list23.add_ppred(opr12, LESS_THAN, opr13);
  MLJoin root(&lp0, &lp1, &list23);
  
  Seq seq(mypattern, &s, recbuffers, &root); 

  seq.open();
  while(!s.to_end()) {
    Rec_iter iter = seq.next(bunch);
    /*for(; iter != root.end(); iter++) {    
      //if((*iter)->filtertw(tw));
      cout << (*iter)->tostring() << endl;
      }*/
    root.clear();
  } 
  seq.close();

  cout << "done" << endl;
  //while(true) {}
#else

#ifdef INNERRIGHT
  //@evaluate the right-pred first [IBM; [[Sun; Oracle]; Google]]

  Opr opr03(0, 2), opr04(0, 2);  // (eventIdx, attriIdx)
  PPredList list34;
  list34.add_ppred(opr03, LESS_THAN, opr04);
  MLJoin lp0(&recbuffers[1], &recbuffers[2], &list34);


  Opr opr12(1, 2), opr13(0, 2);  // (eventIdx, attriIdx)
  PPredList list23;
  list23.add_ppred(opr12, GREATER_THAN, opr13);
  MLJoin lp1(&lp0, &recbuffers[3], &list23);
  MLJoin root(&recbuffers[0], &lp1);
  
  Seq seq(mypattern, &s, recbuffers, &root);

  int interval = num; int times = 0;   
  timeval start, end; int next = 0;
  gettimeofday(&start, NULL);

  seq.open();
  while(!s.to_end()) {
    Rec_iter iter = seq.next(bunch);
    /*for(; iter != root.end(); iter++) {    
      //if((*iter)->filtertw(tw));
      cout << (*iter)->tostring() << endl;
      }*/
    root.clear();
    if(times < 2 && s.to_end(interval)) {
      times++; interval += num; 
      gettimeofday(&end, NULL);
      double t1 = start.tv_sec+(start.tv_usec/1000000.0);
      double t2 = end.tv_sec+(end.tv_usec/1000000.0);
      cout << "time:" << t2 - t1 << endl;
      cout << "datanum: " << s.current()-next << endl;  
      cout << "throughput: " << (s.current()-next)/(t2-t1) << endl;
      next = s.current();
      gettimeofday(&start, NULL);
    }    
  }

  seq.close();
  gettimeofday(&end, NULL);
  double t1 = start.tv_sec+(start.tv_usec/1000000.0);
  double t2 = end.tv_sec+(end.tv_usec/1000000.0);
  cout << "time:" << t2 - t1 << endl;
  cout << "datanum: " << s.current()-next << endl;  
  cout << "throughput: " << (s.current()-next)/(t2-t1) << endl;
  //while(true) {}

#else

#ifdef INNERLEFT
  //@evaluate the right-pred first [[IBM; [Sun; Oracle]]; Google]

  Opr opr03(0, 2), opr04(0, 2);  // (eventIdx, attriIdx)
  PPredList list34;
  list34.add_ppred(opr03, LESS_THAN, opr04);
  MLJoin lp0(&recbuffers[1], &recbuffers[2], &list34);

  MLJoin lp1(&recbuffers[0], &lp0);

  Opr opr12(2, 2), opr13(0, 2);  // (eventIdx, attriIdx)
  PPredList list23;
  list23.add_ppred(opr12, GREATER_THAN, opr13);

  MLJoin root(&lp1, &recbuffers[3], &list23);

  Seq seq(mypattern, &s, recbuffers, &root);
  seq.open();
  while(!s.to_end()) {
    Rec_iter iter = seq.next(bunch);
    /*for(; iter != root.end(); iter++) {    
    //if((*iter)->filtertw(tw));
    cout << (*iter)->tostring() << endl;
    }*/
    root.clear();
  } 
  seq.close();

#endif // INNERLEFT
#endif // INNERRIGHT
#endif // LEFT
#endif // RIGHT
#endif // BUSHY  
  return 1;
}
