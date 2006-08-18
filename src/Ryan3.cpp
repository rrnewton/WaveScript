
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
  static const vector<complex> fft( const vector<complex> input) {
    printf("In fft.\n");
    return input;
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
  DEFINE_OUTPUT_TYPE(vector<complex>);
  
  private:
  bool iterate(WSQueue *inputQueue) {
    printf("Ryan iterate for s2!\n");

    void *input = inputQueue->dequeue();
    SigSeg<complex>* w = (SigSeg<complex>*)input;
    emit(WSPrim::fft(WSPrim::fft(WSPrim::to_array(w))));

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

    //void *input = inputQueue->dequeue();
    //vector<complex>* arr0 = (vector<complex>*)input;

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

