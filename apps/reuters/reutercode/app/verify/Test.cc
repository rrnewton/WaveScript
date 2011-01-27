#include <iostream>
#include <cstring>
#include <deque>
#include <list>
#include <string>
#include <map>
#include <algorithm>


using namespace std;

class Test {

public:

  int _a;
  Test(int a) { _a = a; }
  Test(){ _a = -1; }
  int& a() { return _a; }

};

template <typename Pair>
struct Less : public std::binary_function<Pair, Pair, bool>
{
    bool operator()(const Pair &x, const Pair &y) const
    {
        return x.first < y.first;
    }
};


int main() {

  /*  list<Test**> list1;
  for(int i=1; i<20; i++) {
    Test* p = new Test(i);
    list1.push_back(&p);
  }

  list<Test**>::iterator iter = list1.begin();
  iter++;
  iter++;

  list<Test**>::iterator myiter = iter;

  cout << *iter << "\t" << *myiter << \n;
  delete **(iter);
  iter = list1.erase(iter);

  cout << (*iter)->_a << endl;
  cout << (*myiter) << endl;
  cout << (*(list1.end())) << endl;

  return -1;*/
  
  /*deque<int> l;
  for(int i=0; i<20000000; i++)
    l.push_back(i);


  for(int i=0; i<20000000; i++) {
    l[i];
    //cout << *iter << endl;    
    }*/

  /*list<int> l;
  for(int i=0; i<20000000; i++)
    l.push_back(i);

  for(list<int>::iterator iter=l.begin(); iter !=l.end(); iter++) {
    //cout << *iter << endl;
    *iter;
    }*/

  /*deque<Test*> l;

  Test* i = l.front();
  if (i==NULL)
  cout << i;*/

  /*Predicates pred(3);
    if (pred._predicates[1] == NULL) cout << "yes";*/
  /*  deque<int> l;
  deque<int>::iterator iter = l.begin();
 
  for(int i=10; i<20; i++)
    l.push_back(i);
 
  
  cout << *iter++ << endl;
  deque<int>::iterator curr = iter;
  iter = l.end(); iter--;
  cout << *iter << endl;
  cout << *curr << endl;

  l.push_back(100);
  iter++;
  cout << *iter << endl;

  if(iter == l.end()) cout << "in the end;" << endl;
  //  iter--;
  //cout << *iter << endl;*/
  //char buf[10] = "hello";
  //cout << (string)buf << endl;

  ///-------------map-----------------------------------
  /*map<int, double> mymap;
  map<int, double>::iterator iter = mymap.find(1);
  mymap.insert(pair<int, double>(10, 100.88));
  mymap[10] = 183.88;
  mymap.insert(pair<int, double>(10, 100.88));
  mymap.erase(10);

  if(iter == mymap.end()) { cout << "hello"; }
  cout << iter->second << endl;

  iter = mymap.find(10);  
  if(iter == mymap.end()) { cout << "hello"; }
  cout << iter->second << endl;*/

  multimap<int, string> mymultimap;
  multimap<int, string>::iterator multiiter = mymultimap.find(1);
  if(multiiter == mymultimap.end()) { cout << "hello2\n"; }

  pair<multimap<int, string>::iterator, multimap<int, string>::iterator> ret;
  ret = mymultimap.equal_range(1);
  if(ret.first == ret.second) cout << "hello2\n";


  mymultimap.insert(pair<int, string>(1, "myname"));
  mymultimap.insert(pair<int, string>(1, "mynameis"));
  mymultimap.insert(pair<int, string>(2, "mynameisyuan"));
  mymultimap.insert(pair<int, string>(3, "mynameisyuanmei"));

  multimap<int, string>::iterator first = mymultimap.upper_bound(0);
  while(first != mymultimap.end()) {
    cout << first->first << endl;
    //multimap<int, string>::iterator next = first;
    first = mymultimap.upper_bound(first->first);
  }
  //  multimap<int, string>::iterator last = adjacent_find(first, mymultimap.end(), Less<pair<int, string> >());

  //for(; first != last; first++) {
  // cout << first->first << endl;
  //}

  //cout << last->first;
  

  //-------------------substring--------------------
  /*  char test1[10] = "IBM";
  char* test2 = (char*)malloc(10);
  cout << strlen(test1) << endl;
  memcpy(test2, test1, strlen(test1));
 
  string test3(test2) ;
  cout << test3 << endl;
  free(test2);

  char test4[10] = "myIBM";
  size_t found = string(test4).find(test3);
  if (found == string::npos) cout << false;
  else cout << true;*/
    
   
  return 0;

}


