#include <WaveScope.h>
#include <Heartbeat.hpp>
#include <PrintBox.hpp>
#include <RawFileSource.hpp>
#include <MemorySource.hpp>
#include <AsciiFileSink.hpp>
#include <Boxes.hpp>

#include <EEGAcquireBox.hpp>

#define POST_IO_SLEEP 0

/* eugene's constants */
#include <string>
#include <deque>
#include <iostream>
#include <fstream>

/* using a c header too for strtok */
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <math.h>
#include <sys/time.h>
#include <sys/stat.h>

// #include "my_fixed.h"

/* for the socket stuff */
#include "ClientSocket.h"
#include "SocketException.h"

#define SAMPLING_RATE_IN_HZ         256
#define NUM_CHANNELS                22
// #define COMM_FILENAME               "/var/run/dir_post.pid"

using namespace std;

// get the global PID of the other process handling
// don't know if this is the best way to initiate IPC at this point
// pid_t comm_pid;

// for switching types
typedef float REAL;

bool fromSerial = false;
bool debug = false;
bool sendMsg = false;

/* ZipN operator: takes N input streams of a single type T and emits zipped
   tuples, each containing N elements */ 
template <class T> class ZipN : public WSBox {
public:
  ZipN<T>(uint32_t N) : WSBox("ZipN") { _N = N; _inputs = new list<T>[_N]; };
  ~ZipN<T>() { delete [] _inputs; }

  struct Output
  {
    uint32_t _N;
    T* _field;

    Output() {}

    Output(uint32_t N) : _N(N) { _field = new T[_N]; }

    ~Output() { delete [] _field; }

    Output(uint32_t N, T* in) : _N(N) {
      _field = new T[_N]; 
      for(uint32_t i = 0; i < _N; i++) _field[i] = in[i];
    }

    Output(const Output& o) : _N(o._N), _field(new T[o._N]) {
      for(uint32_t i = 0; i < o._N; i++) _field[i] = o._field[i];
    }

    Output& operator =(const Output& o) {
      if (_field) delete [] _field; _N = o._N;
      _field = new T[_N];
      for(uint32_t i = 0; i < o._N; i++) _field[i] = o._field[i];

      return *this;
    }

  };

private:
  DEFINE_OUTPUT_TYPE(Output);

  uint32_t _N;
  list<T> *_inputs;

  bool isDataReady() {
    uint32_t total = 0;

    // all lists have something in it?
    for(uint32_t i = 0; i < _N; i++) {
      if(!(_inputs[i].empty())) {
	total++;
      }
    }

    if(total == _N) {
      return true;
    }

    return false;
  }

  bool iterate(uint32_t port, void *item)
  {
    uint32_t i = 0;

    // just enqueue it
    SigSeg<T> *casted = (SigSeg<T> *) item;
    T *buf = casted->getDirect();
    uint32_t length = casted->length();
    
    for(i = 0; i < length; i++) {
      _inputs[port].push_back(buf[i]);
    }
    casted->release(buf);

    while(isDataReady()) {
      // pop element off all lists and generate an output
      Output outTuple(_N);
      for(i = 0; i < _N; i++) {
 	T tmp = _inputs[i].front();
	outTuple._field[i] = tmp;
 	_inputs[i].pop_front();
      }
      emit(outTuple);
    }

    return false;
  }

};

template <class T> class SVMOutput : public WSBox {
public:
  SVMOutput(const char *sv_filename, const char *svcoeff_filename, 
	    uint32_t nVectors, T svmKernelPar) : WSBox("SVMOutput")
  {

    string line;
    uint32_t nLines = 0;
    ifstream myfile(sv_filename);
    deque<string> tokens;
    deque<T> values;

    // SHOULD CHECK TO SEE IF THE FILE EXISTS!!!!!
    struct stat buffer;
    if ( stat (sv_filename, &buffer) != 0 ) {
      cout << sv_filename << " DOES NOT EXIST! " << endl;
    }
    // load the svmVector in filename into svmVector

    if (myfile.is_open()) {
      while (!myfile.eof() && nLines <= nVectors ) {
	getline(myfile, line);
	nLines++;
	tokenize(line, tokens, ",");
	for(uint32_t i = 0; i < tokens.size(); i++) {
	  values.push_back(atof(tokens[i].c_str()));
	}
	_svmVectors.push_back(values);
	tokens.clear();
	values.clear();
      }
    } 
    myfile.close();

    // read coefficients and the bias
    ifstream myOtherfile(svcoeff_filename);
    if (myOtherfile.is_open()) {
      while (!myOtherfile.eof()) {
	getline(myOtherfile, line);

	tokenize(line, tokens, ",");
	for(uint32_t i = 0; i < tokens.size() - 1; i++) {
	  _svmCoeff.push_back(atof(tokens[i].c_str()));
	}
	_svmBias = atof(tokens[tokens.size()-1].c_str());
      }
    }
    myOtherfile.close();

    _svmKernelPar = svmKernelPar; 

//     // let's look at the coefficients
//     for(uint32_t i = 0; i < _svmCoeff.size(); i++) {
//       cout << _svmCoeff[i] << endl;
//     }
//     cout << endl;
//     cout << _svmBias << endl;
//     _svmCoeff = new double[nVectors];
//     for(uint32_t i = 0; i < nVectors; i++) {
//       _svmCoeff[i] = svmCoefficients[i];
//     }

//     // let's look at the svmVectors
//     for(uint32_t i = 0; i < _svmVectors.size(); i++) {
//       deque<double> temp = _svmVectors[i];
//       for(uint32_t j = 0; j < temp.size(); j++) {
// 	cout << temp[j] << endl;
//       }
//       cout << endl;
//     }

  }

private:
  DEFINE_OUTPUT_TYPE(T);

  T _svmBias;
  T _svmKernelPar;

  // *** MIGHT THINK ABOUT USING SOMETHING BESIDES A DEQUE  
  deque<T> _svmCoeff;
  deque<deque<T> > _svmVectors;

  bool iterate(uint32_t port, void *input) 
  {
    // the input should be a pointer to a deque of T
    deque<T> *casted = (deque<T> *)input;
    REAL norm = 0;
    REAL ySVM = 0;

    for(uint32_t i = 0; i < _svmVectors.size(); i++) {
      norm = diff_norm_squared(*casted, _svmVectors[i]);

      ySVM = ySVM + _svmCoeff[i] * exp( ( (-norm) / (_svmKernelPar * int(casted->size())) ) );
    }
    ySVM += _svmBias;
    emit(ySVM); // wonder if this will work 

    return false;
  }

  REAL diff_norm_squared(deque<T> a, deque<T> b)
  {
    // vectors should be the same length
    REAL result = 0;
    REAL x, y;

    for(uint32_t i = 0; i < a.size(); i++) {
      x = log(a[i]);
      y = b[i];
      result = result + (x - y) * (x - y);
    }

    return result;
  }


  // helper function
  void tokenize(const string& str, deque<string>& tokens,
		const string& delimiters = " ") 
  {
    // Skip delimiters at beginning.
    string::size_type lastPos = str.find_first_not_of(delimiters, 0);
    // Find first "non-delimiter".
    string::size_type pos     = str.find_first_of(delimiters, lastPos);
    
    while (string::npos != pos || string::npos != lastPos) {
      {
        // Found a token, add it to the vector.
        tokens.push_back(str.substr(lastPos, pos - lastPos));
        // Skip delimiters.  Note the "not_of"
        lastPos = str.find_first_not_of(delimiters, pos);
        // Find next "non-delimiter"
        pos = str.find_first_of(delimiters, lastPos);
      }
    }
  }

};

template <class T> class BinaryClassify : public WSBox {
public:
  BinaryClassify(T threshold, uint16_t consWindows) : WSBox("BinaryClassify") 
  {
    _threshold = threshold;
    for(uint32_t i = 0; i < consWindows; i++) {
      _detect.push_back(false);
    }

//     try {
//       _client_socket = new ClientSocket( "localhost", 8040 );
//     } catch ( SocketException& e ) {
//       std::cout << "Exception was caught:" << e.description() << "\n";
//     }

  }

  ~BinaryClassify()
  {
//     delete _client_socket;
  }

private:
  DEFINE_OUTPUT_TYPE(bool);

  deque<bool> _detect;
  T _threshold;
  ClientSocket *_client_socket;

  bool iterate(uint32_t port, void *input) 
  {
    uint16_t consWindows = 0;
    T *casted = (T *)input;
    REAL ySVM = *casted;

//     cout << "svm output: " << ySVM << " ";
    if(ySVM > _threshold) {
      _detect.push_back(true);
      _detect.pop_front();

      for(uint32_t i = 0; i < _detect.size(); i++) {
	if(_detect[i] == true) {
	  consWindows++;
	}
      }
      
      // if both detections were true, then we trigger
      // we can do this more intelligently, but just saving previous info
      if(consWindows == _detect.size()) { 
	emit(true);

	/* Grab the time */
// 	struct timeval seizure_time;
// 	gettimeofday(&seizure_time, NULL);
// 	char time_buffer[50];
// 	sprintf(time_buffer, "%u.%u", (unsigned int)(seizure_time.tv_sec), 
// 		(unsigned int)(seizure_time.tv_usec));
// 	try { 
// 	    *_client_socket << time_buffer << "\n\n";
// 	} catch ( SocketException& e ) {
// 	  std::cout << "Exception was caught:" << e.description() << "\n";
// 	}

      } else {
	emit(false);
	
// 	try { 
// 	    *_client_socket << "F" << "\n\n";
// 	} catch ( SocketException& e ) {
// 	  std::cout << "Exception was caught:" << e.description() << "\n";
// 	}

      }
      
    } else {
      _detect.push_back(false);
      _detect.pop_front();
      
      emit(false);
      
//       try { 
// 	  *_client_socket << "F" << "\n\n";
//       } catch ( SocketException& e ) {
// 	std::cout << "Exception was caught:" << e.description() << "\n";
//       }
    }
    
    return false;
  }
};

// take a big tuples of tuples
// and makes into one huge deque of that type
// customized to what I need
template <class T> class FlattenZip : public WSBox {
public:
  FlattenZip(uint32_t N) : WSBox("FlattenZip") { 
    _N = N; _inputs = new list<T>[_N];
  };
  ~FlattenZip() { delete [] _inputs; };

  typedef typename ZipN<T>::Output InternalZipType;  

private:

  DEFINE_OUTPUT_TYPE(deque<T>);

  uint32_t _N; // number of channels
  list<T> *_inputs;
  uint32_t tupleSize;

  bool isDataReady() {
    uint32_t total = 0;

    // all lists have something in it?
    for(uint32_t i = 0; i < _N; i++) {
      if(!(_inputs[i].empty())) {
	total++;
      }
    }

    if(total == _N) {
      return true;
    }

    return false;
  }

  bool iterate(uint32_t port, void *item)
  {
    deque<T> output;

    InternalZipType *casted = (InternalZipType *)item;
    tupleSize = casted->_N;

    // push all the points here
    for(uint32_t i = 0; i < tupleSize; i++) {
      _inputs[port].push_back(casted->_field[i]);
    }

    if(isDataReady()) {
      // pop element off all lists and generate an output
      for(uint32_t i = 0; i < _N; i++) {
	for(uint32_t j = 0; j < casted->_N; j++) {
	  T temp = _inputs[i].front();
	  output.push_back(temp);
	  _inputs[i].pop_front();
	}
      }
      emit(output);
    }

    return false;
  }

};

// only supports doubles
class MergeZip : public WSBox {
public:
  MergeZip() : WSBox("MergeZip") { 
    _stored_value = 0.0; 
    _doubled = new Signal<REAL>;
  }

  typedef Zip2< SigSeg<REAL>, SigSeg<REAL> >::Output ZipType;  

  ~MergeZip() {
    delete _doubled;
  }

private:
  DEFINE_OUTPUT_TYPE(SigSeg<REAL>);

  REAL _stored_value;
//   Timebase _derivedTB;
  Signal<REAL> *_doubled;

  bool iterate(uint32_t port, void *input) {
    ZipType *casted = (ZipType*)input;
    uint32_t length = casted->_first.length();

    // we delay the odd signal and add to the even
//     if(_derivedTB == Unitless) {
//       _derivedTB = DerivedTimebase(casted->_first.getTimebase(), 0, 0);
//       _doubled = new Signal<double>(_derivedTB);
//     }

    // this is a hack... let's try this
    REAL *buf = _doubled->getBuffer(length);
    REAL *first = casted->_first.getDirect();
    REAL *second = casted->_second.getDirect();

    for(uint32_t i = 0; i < length; i++) {
      buf[i] = first[i] + _stored_value;
//       cout << first[i] << " " << _stored_value << " " << second[i] << " " << buf[i] << endl;
      _stored_value = second[i]; // we don't add the last odd guy, but store
    }
//     cout << endl;

    casted->_first.release(first);
    casted->_second.release(second);

    SigSeg<REAL> output = _doubled->commit(length);
    emit(output);

    return false;
  }
};

static inline WSBox *AddOddAndEven(WSBox* input1, WSBox* input2) 
{
  Zip2<SigSeg<REAL>, SigSeg<REAL> > *zip2 = 
    new Zip2<SigSeg<REAL>, SigSeg<REAL> >;
  zip2->connect(input1);
  zip2->connect(input2);

  MergeZip *mZip = new MergeZip; 
  mZip->connect(zip2);

  return mZip;
};


// get odd part of the signal, assume signal has even # of samples
template <class T> class GetOdd : public WSBox { 
public:
  GetOdd() : WSBox("GetOdd") { }

private:
  DEFINE_OUTPUT_TYPE(SigSeg<T>);

//   Timebase _derivedTB;
  Signal<T> _decimated;

  bool iterate(uint32_t port, void *input) {
    SigSeg<T> *casted = (SigSeg<T> *)input;
//     if(_derivedTB == Unitless) {
//       _derivedTB = DerivedTimebase(casted->getTimebase(), 1, 0.5);
//       _decimated = Signal<double>(_derivedTB);
//     }

    // get input and allocate space for the output
    T *samples = casted->getDirect();
    int length = casted->length()/2;

    T *odd_signal = _decimated.getBuffer(length);
    for(int i = 0; i < length; i++) {
      odd_signal[i] = samples[i*2+1];
//       cout << odd_signal[i] << endl;
    }
    casted->release(samples);

    SigSeg<T> output = _decimated.commit(length);
    emit(output);

    return false;
  }
};

// get even part of the signal, assume signal has even # of samples
template <class T> class GetEven : public WSBox {
public:
  GetEven() : WSBox ("GetEven") { }

private:
  DEFINE_OUTPUT_TYPE(SigSeg<T>);

  Timebase _derivedTB;
  Signal<T> _decimated;

  bool iterate(uint32_t port, void *input) {
    SigSeg<T> *casted = (SigSeg<T> *)input;
//     if(_derivedTB == Unitless) {
//       _derivedTB = DerivedTimebase(casted->getTimebase(), 0, 0.5);
//       _decimated = Signal<double>(_derivedTB);
//     }

    // get input and allocate space for the output
    T *samples = casted->getDirect();
    int length = (casted->length()) / 2;

    T *even_signal = _decimated.getBuffer(length);
    for(int i = 0; i < length; i++) {
      even_signal[i] = samples[i*2];
//       cout << even_signal[i] << endl;
    }
    casted->release(samples);

    SigSeg<T> output = _decimated.commit(length);
    emit(output);

    return false;
  }
};

/* implementation of an FIR filter using convolution */
/* you have to provide an array of coefficients */
template <class T> class FIRFilter : public WSBox {
public:
  FIRFilter(T* filter_coeff, uint32_t nCoeff) : WSBox("FIRFilter") {
    _nCoeff = nCoeff;
    _flipped_filter_coeff = new T[nCoeff];
    
    for(uint32_t i = 0; i < nCoeff; i++) {
      _flipped_filter_coeff[nCoeff-1-i] = filter_coeff[i];
    }
    
    for(uint32_t i = 0; i < nCoeff - 1; i++) {
      _memory.push_back((T)0);
    }
  }
  
  ~FIRFilter() {
    delete [] _flipped_filter_coeff;
  }
  
private :
  DEFINE_OUTPUT_TYPE(SigSeg<T>);

  T* _flipped_filter_coeff; // array of _filter_coefficients
  uint32_t _nCoeff; // number of coefficients in the filter array
  deque<T> _memory; // remembers the previous points needed for convolution
  
  bool iterate(uint32_t port, void *input) 
  {
    SigSeg<T>* casted = (SigSeg<T>*)input;
    T *buf = casted->getDirect();
    
    /* need something to output */
    Signal<T> s = Signal<T>(casted->getTimebase());
    T *outputBuf = s.getBuffer(casted->length());

    for(uint32_t j = 0; j < casted->length(); j++) { 
      // add the first element of the input buffer into the array
      _memory.push_back(buf[j]);
      outputBuf[j] = 0;
      for(uint32_t i = 0; i < _nCoeff; i++) {
	outputBuf[j] = outputBuf[j] + _flipped_filter_coeff[i] * _memory[i];
      }

      // DEBUG
//       cout << "input: ";
//       for(uint32_t i = 0; i < _nCoeff; i++) {
// 	cout << _memory[i] << " ";
//       }

//       cout << ", filter: ";
//       for(uint32_t i = 0; i < _nCoeff; i++) {
// 	cout << _flipped_filter_coeff[i] << " ";
//       }
//       cout << ", output: " << outputBuf[j] << endl;
      _memory.pop_front();
    }
    casted->release(buf);
    
    /* emit the sigseg */
    SigSeg<T> output = s.commit(casted->length());
    emit(output);
    
    return false;
  }
};

template <class T> class MagWithScale : public WSBox {
public:
  MagWithScale() : WSBox("MagWithScale") { _scale = 1.0; }
  MagWithScale(T scale) : WSBox("MagWithScale") { _scale = scale; }
  
private:
  DEFINE_OUTPUT_TYPE(SigSeg<T>);

  T _scale;

  bool iterate(uint32_t port, void *input)
  {
    SigSeg<T> *casted = (SigSeg<T> *)input;
    T *cbuf = casted->getDirect();
    /* here we could determine whether we are seeing a continuation
     * of the previous signal or a new signal.  */
    /* alloc buffer */

    /* magnitude */
    Signal<T> s = Signal<T>(casted->getTimebase());
    T *buf = s.getBuffer(1);
    buf[0] = 0;
    for (uint32_t i = 0; i<casted->length(); i++) {
      buf[0] = buf[0] + abs(cbuf[i])/_scale;
    }
//     cout << buf[0] << endl;
//     assert(buf[0] != 0);
//     buf[0] = log(buf[0]);  // NOTE THIS STEP MIGHT BE DONE LATER!
    casted->release(cbuf);
    
    /* emit the sigseg */
    SigSeg<T> output = s.commit(1);
    emit(output);
    
    return false;
  }
};

REAL hHigh_Even[] = {-0.2304, -0.6309, 0.1870, -0.0329};             
REAL hHigh_Odd[] = {0.7148, -0.0280, 0.0308, -0.0106};   

static inline WSBox *HighFreqFilter(WSBox *input)
{
  GetEven<REAL> *evenSignal = new GetEven<REAL>();
  GetOdd<REAL> *oddSignal = new GetOdd<REAL>();

  // get even and odd parts of signal
  evenSignal->connect(input);
  oddSignal->connect(input);

  // now filter
  FIRFilter<REAL> *highFreqEven = new FIRFilter<REAL>(hHigh_Even, sizeof(hHigh_Even)/sizeof(REAL));
  FIRFilter<REAL> *highFreqOdd = new FIRFilter<REAL>(hHigh_Odd, sizeof(hHigh_Odd)/sizeof(REAL));

  highFreqEven->connect(evenSignal);
  highFreqOdd->connect(oddSignal);

  // now recombine
  WSBox *combine = AddOddAndEven(highFreqEven, highFreqOdd);
  return combine;
};

// Filter coefficients
REAL hLow_Even[] = {-0.0106, 0.0308, -0.0280, 0.7148};    
REAL hLow_Odd[] = {0.0329, -0.1870, 0.6309, 0.2304};                 
  
static inline WSBox *LowFreqFilter(WSBox *input)
{

  GetEven<REAL> *evenSignal = new GetEven<REAL>();
  GetOdd<REAL> *oddSignal = new GetOdd<REAL>();

  // get even and odd parts of signal
  evenSignal->connect(input);
  oddSignal->connect(input);

  // now filter
  FIRFilter<REAL> *lowFreqEven = new FIRFilter<REAL>(hLow_Even, sizeof(hLow_Even)/sizeof(REAL));
  FIRFilter<REAL> *lowFreqOdd = new FIRFilter<REAL>(hLow_Odd, sizeof(hLow_Odd)/sizeof(REAL));

  //   PrintBox<SigSeg<double> >*debugBox = new PrintBox<SigSeg<double> >("debug");
  //   debugBox->connect(lowFreqEven);

  lowFreqEven->connect(evenSignal);
  lowFreqOdd->connect(oddSignal);

  // now recombine them
  WSBox *combine = AddOddAndEven(lowFreqEven, lowFreqOdd);
  return combine;
};


REAL filterGains[8]  = {1.4142, 1.8684, 2.6412, 3.7352, 5.2818, 
			7.4668, 10.5596, 11.3137 };

static inline WSBox *GetFeatures(WSBox* input)
{

//  WSBox *highFreq1 = HighFreqFilter(input);
  WSBox *lowFreq1 = LowFreqFilter(input);

  //WSBox *highFreq2 = HighFreqFilter(lowFreq1);
  WSBox *lowFreq2 = LowFreqFilter(lowFreq1);

  //WSBox *highFreq3 = HighFreqFilter(lowFreq2);
  WSBox *lowFreq3 = LowFreqFilter(lowFreq2);

  WSBox *highFreq4 = HighFreqFilter(lowFreq3); // we want this one
  WSBox *lowFreq4 = LowFreqFilter(lowFreq3); 
  MagWithScale<REAL> *level4 = new MagWithScale<REAL>(filterGains[3]); 
  level4->connect(highFreq4);

  WSBox *highFreq5 = HighFreqFilter(lowFreq4); // and this one 
  WSBox *lowFreq5 = LowFreqFilter(lowFreq4); 
  MagWithScale<REAL> *level5 = new MagWithScale<REAL>(filterGains[4]); 
  level5->connect(highFreq5);

  WSBox *highFreq6 = HighFreqFilter(lowFreq5); // and this one
  //  WSBox *lowFreq6 = LowFreqFilter(lowFreq5); 
  MagWithScale<REAL> *level6 = new MagWithScale<REAL>(filterGains[5]); 
  level6->connect(highFreq6);

//   WSBox *highFreq7 = HighFreqFilter(lowFreq6); // and this one
//   //  WSBox *lowFreq7 = LowFreqFilter(lowFreq6); 
//   MagWithScale *level7 = new MagWithScale(filterGains[6]); 
//   level7->connect(highFreq7);
  
  // now zip all these together.
  ZipN<REAL> *chanFeatures = new ZipN<REAL>(3);
  chanFeatures->connect(level4);
  chanFeatures->connect(level5);
  chanFeatures->connect(level6);
  //  chanFeatures->connect(level7);

  return chanFeatures;
  //  return level6;
};

int main(int argc, char ** argv)
{
  char port_name[256];

  string channelNames[] = {"FT10-T8","FT9-FT10","T7-FT9","P7-T7",
			   "CZ-PZ","FZ-CZ","P8-O2","T8-P8","F8-T8",
			   "FP2-F8","P4-O2","C4-P4","F4-C4","FP2-F4",
			   "P3-O1","C3-P3","F3-C3","FP1-F3","P7-O1",
			   "T7-P7","F7-T7", "FP1-F7"};

  int rate = SAMPLING_RATE_IN_HZ;

  /* initialize subsystems */
  WSInit(&argc, argv);

  // Read from file or the serial port
  if(misc_parse_option_as_string(&argc, argv, "port", 'p', port_name) == 0) {
    fromSerial = true;
  }

  if(misc_parse_out_switch(&argc, argv, "debug", 'd')) {
    debug = true;
  }

//   if(misc_parse_out_switch(&argc, argv, "enable_net", 'e')) {
//     sendMsg = true;
//   }


  // detector values
  REAL svmKernelPar = 1.00;
  REAL threshold = 0.1;
  uint16_t consWindows = 3; // number of consecutive windows of detections;

  // get the SVM box that we're going to use

  // *** MAKE THIS PARAMETERIZABLE *** 
  SVMOutput<REAL> svmClass = SVMOutput<REAL>("./Applications/SeizureDetection/patient_sv/Patient_36_SVs.txt", 
					     "./Applications/SeizureDetection/patient_sv/Patient_36_SVCoeff.txt",
					     30, svmKernelPar);
  BinaryClassify<REAL> detect = BinaryClassify<REAL>(threshold, consWindows);

  /* REAL CODE */
  RawFileSourceT<REAL> *ch[NUM_CHANNELS];
  EEGAcquireSource<REAL> *serial_ch[NUM_CHANNELS];
  Rewindow<REAL> *rw[NUM_CHANNELS];
  WSBox *filter_results[NUM_CHANNELS];

  // load data from file
  FlattenZip<REAL> flatten = FlattenZip<REAL>(NUM_CHANNELS);

  for(uint32_t j = 0; j < NUM_CHANNELS; j++) {
    if(!fromSerial) {
//       string temp = string("./Applications/SeizureDetection/data_subsets/" + 
// 			   channelNames[NUM_CHANNELS-j-1] + ".txt");
//       string temp = string("./Applications/SeizureDetection/patient36_file16/" + 
// 			   channelNames[NUM_CHANNELS-j-1] + ".txt");
          string temp = string("./Applications/SeizureDetection/patient36_file16/" + 
			       channelNames[NUM_CHANNELS-j-1] + "-short.txt");
      ch[j] = new RawFileSourceT<REAL>(temp.c_str(), 0, 1, rate);
      rw[j] = new Rewindow<REAL>(512, 512);
      rw[j]->connect(ch[j]);

    } else {
      serial_ch[j] = new EEGAcquireSource<REAL>(j, port_name);
      rw[j] = new Rewindow<REAL>(512, 512);
      rw[j]->connect(serial_ch[j]);
    }

    filter_results[j] = GetFeatures(rw[j]);
    flatten.connect(filter_results[j]); 
  }

//   AsciiFileSink<REAL> fs = AsciiFileSink<REAL>("/tmp/rewindow.out");
//   if(!fromSerial) {
//     fs.connect(ch[0]);
//   } else {
//     fs.connect(serial_ch[0]);
//   }

  svmClass.connect(&flatten);

//   PrintBox<REAL>debugBox = PrintBox<REAL>("svmOutput");
//   debugBox.connect(&svmClass);

  detect.connect(&svmClass);
  PrintBox<bool>debugDetectBox = PrintBox<bool>("Detect");
  debugDetectBox.connect(&detect);

  // get time
  struct timeval start_time, end_time;
  gettimeofday(&start_time, NULL);

  /* now, run */
  WSRun();

  gettimeofday(&end_time, NULL);

  int total_secs = end_time.tv_sec - start_time.tv_sec;
  cout << total_secs << " seconds" << endl;

//     don't forget to delete the rewindows
  for(uint32_t i = 0; i < NUM_CHANNELS; i++) {
    if(!fromSerial) {
      delete ch[i];
    } else {
      delete serial_ch[i];
    }
    delete rw[i];
  }

//    delete rw;

  return 0;
}
