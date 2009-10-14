#ifndef EVENTSTREAM_H_
#define EVENTSTREAM_H_

#define S_SIZE	        30 		// size of string type
#define BUFTEMP         2048            // maximal schema size

//#define Events          deque<Event*>   // used in RecBuffer and Predicate

#include <iostream>
#include <sstream>

using namespace std;

enum Operator { EQUAL, GREATER_THAN, LESS_THAN, GREATER_EQUAL, LESS_EQUAL, 
		ONULL, CONTAINS };
enum AOperator { PLUS, SUB, MUL, DIV, ANULL };
using namespace std;

/* convert all serializable class to string */
template <class T>
inline string _to_string (T t) {
  stringstream _ss;
  _ss << t;
  return _ss.str();
}

#endif /*EVENTSTREAM_H_*/
