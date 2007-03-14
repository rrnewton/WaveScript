
// These are the headers & #defines that occur at the beginning of all
// wavescript-generated header files.
#include <WaveScope.h>
#include <Heartbeat.hpp>
#include <PrintBox.hpp>
#include <RawFileSource.hpp>
#include <AsciiFileSink.hpp>
#include <Boxes.hpp>

/* for boost smart pointers */
#include <boost/shared_ptr.hpp>
#include <boost/enable_shared_from_this.hpp>

#include <boost/functional/hash.hpp>

#include <stdio.h>
#include <math.h>

#include <list>
#include <vector>
#include <string>
#include <iostream>
#include <ext/hash_map>
#include <complex>

// using boost::enable_shared_from_this;
// using namespace std;
// using namespace __gnu_cxx;

#define TRUE 1
#define FALSE 0

#define WSNULL 0
#define WSNULLSEG (RawSeg::NullRef)
#define WSNULLTIMEBASE 0

typedef bool wsunit_t;

typedef int16_t wsint16_t;
typedef uint16_t wsuint16_t;

typedef int wsint_t;
typedef float wsfloat_t; 
typedef bool wsbool_t;
typedef string wsstring_t;
typedef _Complex float wscomplex_t;

#define WS_DEFINE_OUTPUT_TYPE(type)                \
  inline void emit(const type &tuple) {         \
    uint i;                                     \
    for (i=0; i<m_outputs.size(); i++) {        \
      m_outputs[i]->enqueue(new type (tuple), this);  \
    } \
    totalEmits++; \
  }

// Global setting:
bool WSOUTPUT_PREFIX = TRUE;


// This is a lame work-around:
static char global_show_buffer[500];
static ostringstream global_show_stream(ostringstream::out);


//==============================================================================
// These are built-in WSBoxes. 
// Most of these are intended to go away at some point.
class WSBuiltins {
   
public:

  /* This takes Signal(T) to Signal(SigSeg(T)) */
  class Timer : public WSSource{
   
    public:
    Timer(wsfloat_t freq) : WSSource("Timer") {
      period = (int)(1000000.0 * (1.0 / freq));
      Launch();
    }

    DEFINE_SOURCE_TYPE(wsunit_t);

    private:
    int period; 

    void *run_thread() {
      while (!Shutdown()) {
	source_emit(0);
	usleep(period);
      }
      return NULL;
    }
  };
  
};

/* These structs represent tuples in the WS program. */


class PrintQueryOutput : public WSBox {
  public:
  PrintQueryOutput(const char *name) : WSBox("PrintQueryOutput") {}
  
  private:
  DEFINE_NO_OUTPUT_TYPE;
  
  bool iterate(uint32_t port, void *input) {
    wsunit_t *element = (wsunit_t *)input;
    if(WSOUTPUT_PREFIX) printf("WSOUT: ");
    printf("<object of type %s>", "#()");
    printf("\n");
    return false;
  } 
} 
;



int main(int argc, char ** argv)
{
  /* set global variable(s) */
  if (misc_parse_out_switch(&argc, argv, "no_prefix", 0))
    WSOUTPUT_PREFIX = FALSE;
  else WSOUTPUT_PREFIX = TRUE;

  /* initialize subsystems */ 
  WSInit(&argc, argv);

  /* begin constructing operator graph */
   WSSource* tmp_1 = new WSBuiltins::Timer((wsfloat_t)30.0);
  WSBox* toplevel = tmp_1;

  /* dump output of query -- WaveScript type = (Stream #()) */
  PrintQueryOutput out = PrintQueryOutput("WSOUT");
  out.connect(toplevel);

  /* now, run */
  WSRun();

  return 0;
}

