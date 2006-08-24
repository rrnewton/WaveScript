
#include <WaveScope.h>
#include <Heartbeat.hpp>
#include <PrintBox.hpp>
#include <RawFileSource.hpp>
#include <AsciiFileSink.hpp>
#include <Boxes.hpp>

#include <stdio.h>
#include <vector>

// TODO
#define TRUE 1
#define FALSE 0


// New types may not be declared in a return type!! HAH
class WSPrim {
public:
  // TODO
  static const vector<complex> failed_fft( const vector<complex> input) {
//     printf("In fft.\n");
//     int i;
//     complex *fft_buf = new complex[input.length()];
//     for (i=0; i<input->length(); i++)
//       fft_buf[i] = input[i];

//     float *fft_flt = (float *)fft_buf;    

//     /* copy input over to output buffer */
//     float *cbuf = casted->getDirect();
//     memmove(fft_flt, cbuf, sizeof(float)*input->length());

//     /* do the fft */
//     realft(fft_flt-1, casted->length(), +1);    

//     /* copy back over to an STL vec */
//     vector<complex> output = new vector<complex>(input->length());
//     for (i=0; i<input->length(); i++)
//       output[i] = [i];

//     return output;
    return input;
  }


  //   static const vector<complex> fft( const vector<complex> input) {


  static SigSeg<complex> fft(SigSeg<float> *casted)
    {
      /* Currently we just use the unitless timebase: */ 
      Timebase _freq = Unitless;
      
      /* alloc buffer for FFT */
      Signal<complex> s = Signal<complex>(_freq);
      complex *fft_buf = s.getBuffer(casted->length()/2);
      float *fft_flt = (float *)fft_buf;

      /* copy input over to output buffer */
      float *cbuf = casted->getDirect();
      memmove(fft_flt, cbuf, sizeof(float)*casted->length());
      casted->release(cbuf);
      
      /* do the fft */
      FFT::realft(fft_flt-1, casted->length(), +1);

      /* return the sigseg */
      SigSeg<complex> output = s.commit(casted->length()/2);
      delete casted;
      return(output);
  }

  // TODO
  static vector<complex> to_array( SigSeg<complex>* input) {
    printf("In to_array.\n");
    //    complex* buf = input->getDirect();
    vector<complex> out = vector<complex>(input->length());
    for(uint i=0; i < input->length(); i++) {
      printf("Filling stl vector: (sizeof_complex %d), %d\n", sizeof(complex), i);
      //       out[i] = buf[i];
      complex c;
      c.real = 3.8;
      c.imag = 0.0;
      out[i] = c; 
    }
    printf("Done to_array.\n");
    //return vector<complex>();
    return out;
  }
};


class Iter_s2 : public WSBox {
  public:
  //   DEFINE_OUTPUT_TYPE(vector<complex>);
  inline void emit(const SigSeg<complex> &tuple) {         
    uint i;                                     
    for (i=0; i<m_outputs.size(); i++) {        
      m_outputs[i]->enqueue(new SigSeg<complex> (tuple));
    } }

  private:
  bool iterate(WSQueue *inputQueue) {
    printf("Ryan iterate for s2!\n");

    void *input = inputQueue->dequeue();
    SigSeg<float>* w = (SigSeg<float>*)input;
    //    emit(WSPrim::fft(WSPrim::fft(WSPrim::to_array(w))));
    //emit(WSPrim::to_array(w));
    emit(WSPrim::fft(w));
    
    printf("Finished iterate for s2!\n");
    return TRUE; // TODO
  } 
};


class Iter_s3 : public WSBox {
  public:
  DEFINE_OUTPUT_TYPE(float);
  
  private:
  bool iterate(WSQueue *inputQueue) {
    printf("Ryan iterate for S3!!!\n");

    void *input = inputQueue->dequeue();
    SigSeg<complex>* win = (SigSeg<complex>*)input;
    
    //float casted = (float)((*win)[3]);
    printf("Got input: %f+%fi\n", (*win)[3].real, (*win)[3].imag);

    emit(3.4);
    return TRUE;
  }
};


/*
class Iter_s3 : public WSBox {
  public:
  DEFINE_OUTPUT_TYPE(float);
  
  private:
  bool iterate(WSQueue *inputQueue) {

    printf("Ryan iterate for s3!\n");

    void *input = inputQueue->dequeue();
    complex[] arr0 = (complex[])input;
    int x;
    complex[] arr;
    if ((WSPrim::realpart(arr[100]) > 224192.0);
    ) {
      emit(0.0);
      emit(WSPrim::imagpart(arr[100]));
    } else {
      WSPrim::tuple();
    }
  } 
} 
*/


int main(int argc, char ** argv)
{
  printf("Ryan hand test.\n");

  // TODO:
  //toplevel;

  // initialize subsystems 
  MiscLogInit(&argc, argv);
  TimebaseMgrInit(&argc, argv);

  RawFileSource s1 = RawFileSource("/tmp/archive/4/marmots/meadow1vxp.all_8_100_973449798.903759_0.raw", 
                         0, 4, 48000*30, WSSched::findcpuspeed());
  Iter_s2 s2 = Iter_s2();
  s2.connect(&s1);
  Iter_s3 s3 = Iter_s3();
  s3.connect(&s2);

  // dump output specgram to file 
  //  AsciiFileSink<float> fs = AsciiFileSink<float>("/tmp/specgram.out");
  //  fs.connect(&s1);

  // now, run 
  WSSource::StartThreads();
  WSSched::run();

  return 0;
}

