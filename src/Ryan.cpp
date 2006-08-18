
#include <WaveScope.h>
#include <Heartbeat.hpp>
#include <PrintBox.hpp>
#include <RawFileSource.hpp>
#include <AsciiFileSink.hpp>
#include <Boxes.hpp>

#include <stdio.h>

#define DECIMATE
//#define BRANCHES
#undef BRANCHES

class RyanFilter : public WSBox {
public:
  //  : _decimTB(Unitless) {      
  RyanFilter() {
    printf("Created filter...\n");
    
  }
  
private:
  DEFINE_OUTPUT_TYPE(SigSeg<float>);
 
     //Timebase _decimTB;
  //  Signal<float> _outsig;

  //getTimebase()
    
  bool iterate(WSQueue *in)
  {    
    void *input = in->dequeue();
    SigSeg<float> *casted = (SigSeg<float> *)input;        
    if (input) {

      float *input_buf = casted->getDirect();
      
      printf("Running my box!, input buf size %d, first %f last %f\n", casted->length(), 
	     input_buf[0], input_buf[1023]);
            
      emit(*casted);

      delete casted;
    }
    return false;
  }
};



int main(int argc, char ** argv)
{
  /* initialize subsystems */
  MiscLogInit(&argc, argv);
  TimebaseMgrInit(&argc, argv);
 
  /* a "real" app */
 
  /*load data from file */
  RawFileSource ch_foo = RawFileSource("/tmp/100.raw", 0, 4, 48000*30, WSSched::findcpuspeed());
   
  RyanFilter ch0 = RyanFilter();
  ch0.connect(&ch_foo);

  printf("Ryan Test, running...\n");

#ifdef BRANCHES
  /* also print the segs */
  PrintBox< SigSeg<float> > pb4 = PrintBox< SigSeg<float> >("channel0");
  pb4.connect(&ch0);
#endif 

#ifdef DECIMATE
  /* decimate by 2 */
  Decimate2 decim = Decimate2();
  decim.connect(&ch0);

  /* window by 32 samples and skip 3 of 4 windows */
  Rewindow rw = Rewindow(32, 128);
  rw.connect(&decim);
#else
  /* window by 256 */ 
  Rewindow rw1 = Rewindow(256, 128);
  rw1.connect(&ch0);
#endif
 
#ifdef BRANCHES
  /* also print the segs */
  PrintBox< SigSeg<float> > pb3 = PrintBox< SigSeg<float> >("rewindowed");
  pb3.connect(&rw);
#endif
 
  /* fft */
  FFT ft = FFT(24000);
  ft.connect(&rw);

  /* magnitude */
  Magnitude m = Magnitude();
  m.connect(&ft);

  /* dump output specgram to file */
  AsciiFileSink<float> fs = AsciiFileSink<float>("/tmp/specgram.out");
  fs.connect(&m);

#ifdef BRANCHES
  /* also print the segs */
  PrintBox< SigSeg<float> > pb2 = PrintBox< SigSeg<float> >("psd");
  pb2.connect(&m);
#endif


  /* now, run */
  WSSource::StartThreads();
  WSSched::run();

  return 0;
}
