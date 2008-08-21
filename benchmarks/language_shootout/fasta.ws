
/* fasta.ws
 *
 *   Generate and write random "DNA" sequences.
 *   Ported from C variant.
 * 
 * Author: Ryan Newton (newton@mit.edu)
 *
 */

include "stdlib.ws";
include "unix.ws";

using Unix;
using Array;

type Aminoacid = (Float * Char);
p = fst;
c = snd;

stdout = fopen("/dev/stdout", "a");

WIDTH = 60
newline = intToChar(10) // \n

let (IM, IA, IC) = (139968, 3877, 29573)

alu= 
     "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG"
  ++ "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA"
  ++ "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT"
  ++ "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA"
  ++ "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG"
  ++ "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC"
  ++ "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

iub =  #[
	( 0.27, 'a' ),
	( 0.12, 'c' ),
	( 0.12, 'g' ),
	( 0.27, 't' ),
	( 0.02, 'B' ),
	( 0.02, 'D' ),
	( 0.02, 'H' ),
	( 0.02, 'K' ),
	( 0.02, 'M' ),
	( 0.02, 'N' ),
	( 0.02, 'R' ),
	( 0.02, 'S' ),
	( 0.02, 'V' ),
	( 0.02, 'W' ),
	( 0.02, 'Y' )
        ]

homosapiens = #[
	( 0.3029549426680, 'a' ),
	( 0.1979883004921, 'c' ),
	( 0.1975473066391, 'g' ),
	( 0.3015094502008, 't' ) ]

last = 42 // Mutated

//M_ = 1.0 / Double! M

fun myrandom(mx) {
  last := moduloI(last * IA + IC, IM);
  mx * Float! last / Float! IM;
}

fun accumulate_probabilities(genelist) {
  cp = 0.0;
  for i = 0 to genelist.length - 1 {
    cp += genelist[i].p;
    genelist[i] := (cp, genelist[i].c);
  }
}

fun __repeat_fasta(s, count) {
  pos = 0;
  len = s.length;
  // Make a duplicated copy so we can blast away:
  s2  = makeUNSAFE(len + WIDTH);
  blit(s2, 0,   s, 0, len);
  blit(s2, len, s, 0, WIDTH);
    //memcpy (s2, s, len);
    //memcpy (s2 + len, s, WIDTH);
  cnt = count;
  while cnt > 0 {
    line = min(WIDTH, cnt);
    //fwrite_arr (s2 + pos, 1, line, stdout);
    print ('\n');
    pos += line;
    if (pos >= len) then pos -= len;
    cnt -= line;    
  };
  // free s2 
}

//fun String:toArray(s)   List:toArray   $ String:explode $ s
//fun String:fromArray(a) String:implode $ Array:toList   $ a

//String:ref :: (String, Int) -> Char;
//fun String:ref(s,i) List:ref(String:explode(s), i)
fun String:sub(s,i,l)  String:fromArray$ Array:sub(String:toArray(s),i,l)

break_counter = 0; // Mutated:
// TODO, block write:
fun print_breaks(s::String) {
  i = 0;
  len = s.String:length;
  while i < len {
    print(String:ref(s,i));
    i += 1;
    break_counter += 1;
    if break_counter == WIDTH then {
      break_counter := 0;
      print(intToChar(10));
    }
  }
}

fun repeat_fasta(s, count) {
  len = String:length(s);
  cnt = count;
  // First print whole copies, subject to line breaks.
  while cnt > len {
    print_breaks(s);
    cnt -= len;
  };
  print_breaks(String:sub(s,0,cnt));
  print("\n");
}


fun random_fasta(genelist, count) {
  using Array;
  buf :: Array Char = makeUNSAFE(WIDTH + 1);
  //buf :: Array Char = make(WIDTH + 1, '_');
  cnt = count;
  while cnt > 0 {
    line = min(WIDTH, cnt);
    pos = 0;
    while pos < line {
      r = myrandom(1.0);
      i = 0;
      while genelist[i].p < r { i+=1 }; // Linear search
      pos += 1;
      buf[pos] := genelist[i].c;
    }
    buf[line] := newline;
    fwrite_arr (buf, 1, line + 1, stdout);
    cnt -= line;
  }
}

OutBuf = makeUNSAFE(128 * 1024);

//n = 1000;
n = 25000000;
//n = 25000;

main = iterate _ in timer(1) {
  state { _ = {
    accumulate_probabilities (iub);
    accumulate_probabilities (homosapiens);
  }}

  print(">ONE Homo sapiens alu\n");
  repeat_fasta (alu, 2 * n);

  print(">TWO IUB ambiguity codes\n");
  random_fasta (iub, 3 * n);

  print(">THREE Homo sapiens frequency\n");
  random_fasta (homosapiens, 5 * n);

/*     ; */

/*     setvbuf(stdout, OutBuf, _IOFBF, sizeof OutBuf); /\* buffer output *\/ */
/*     fputs_unlocked (">ONE Homo sapiens alu\n", stdout); */
/*     repeat_fasta (alu, 2 * n); */
/*     fputs_unlocked (">TWO IUB ambiguity codes\n", stdout); */
/*     random_fasta (iub, 3 * n); */
/*     fputs_unlocked (">THREE Homo sapiens frequency\n", stdout); */
/*     random_fasta (homosapiens, 5 * n); */

  emit ();
  wserror("run for only one tuple!");
  //emit iub
}
