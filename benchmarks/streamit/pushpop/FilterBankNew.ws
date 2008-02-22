
// This is a WS port of FilterBankNew.str in the StreamIT distribution.

include "streamit.ws";
include "matrix.ws";

using Mutable;

fun FirFilter(N, COEFF)
  filter(N, 1, 1, fun(peek,pop,push) {
    sum = ref(0);
    for i = 0 to N-1 {
      sum += peek(i) * COEFF[N-1-i];
    }
    pop(1);
    push(sum);
  })


fun Delay_N(N) {
  arr = Array:make(N, 0.0);
  place_holder = ref(0);
  filter(1,1,1, fun(peek,pop,push) {
    push(arr[place_holder]);
    arr[place_holder] := peek(0);
    pop(1);
    place_holder += 1;
    //place_holder := (place_holder + 1).mod(N);
    if (place_holder == N) 
    then place_holder := 0;
  })
}


fun DownSamp(N)
  filter(N,N,1, fun(peek,pop,push) {
    push(peek(0));
    pop(N);
  })

fun UpSamp(N) 
  filter(1,1,N, fun(peek,pop,push) {
    push(peek(0));
    pop(1);
    for i = 1 to N-1 { push(0) }
  })

fun Bank(N,L,H,F) 
 pipeline$ [
   Delay_N(L-1),
   FirFilter(L, H),
   DownSamp(N),
   UpSamp(N),
   Delay_N(L-1),
   FirFilter(L, F)
 ]

//TODO -- need to add splitjoins.
fun Branches(N_samp, N_ch, N_col, H, F) 
   Bank(N_samp, N_col, 
	Array:make(N_col,0.0), Array:make(N_col,0.0))

/*

float->float splitjoin Branches(int N_samp, int N_rows, int N_col,
			      float[N_rows][N_col] H,
			      float[N_rows][N_col] F)
{
  split duplicate;
  for (int i = 0; i < N_rows; i++)
  {
    float[N_col] H_ch;
    float[N_col] F_ch;
    for (int j = 0; j < N_col; j++)
    {
      H_ch[j] = H[i][j];
      F_ch[j] = F[i][j];
    }
    add Bank(N_samp, N_col, H_ch, F_ch);
  }
  join roundrobin;
}
*/


fun Combine(N) 
  filter(N,N,1, fun(peek,pop,push) {
    sum = ref(0);
    for i = 0 to N-1 {
      sum += peek(0); pop(1);
    }
    push(sum);
  })

fun FilterBank(N_samp, N_ch, N_col, H, F) 
  pipeline $ [
    Branches(N_samp, N_ch, N_col, H, F),
    Combine(N_samp)
  ]

fun source() {
  raw = iterate _ in timer$ 1 {
    state {
      max = 1000.0;
      current = 0.0;
    }
    emit current;
    if current > max 
    then current := 0.0
    else current += 1.0;
  };
  window(raw, 10);
}


fun FilterBankNew() {
   N_sim  = 1024 * 2;
   N_samp = 8;
   N_ch   = N_samp;
   N_col  = 32;
   r = Array:make(N_sim, 0.0);
   H = Matrix:create(N_ch, N_col, 0.0);
   F = Matrix:create(N_ch, N_col, 0.0);
   /*
  for (int i = 0; i < N_col; i++)
    for (int j = 0; j < N_ch; j++) {
      H[j][i] = i*N_col + j*N_ch + j + i + j + 1;
      F[j][i] = i*j + j*j + j + i;
    }
*/
 FilterBank(N_samp, N_ch, N_col, H, F) 
 $ source();
}

main = dewindow $ FilterBankNew();
