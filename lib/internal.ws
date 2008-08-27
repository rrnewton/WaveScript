
// This is automatically included by all WS compiles.  It contains
// definitions for the "special-rewrite-libfuns".  Basically,
// primitives that are defined externally in a library, but whose
// names are specially recognized so that rewrite-optimizations can
// process them.

// [2007.12.11] Note, this should also include routines that the
// compiler itself depends on (for example, because other primitives
// desugar to use them).

println     :: a -> ();
assert      :: (String, Bool) -> ();
assert_eq   :: (String, a,a)  -> ();

Array:blit      :: (Array t, Int, Array t, Int, Int) -> ();
Array:append    :: (Array t, Array t) -> Array t;
Array:sub       :: (Array t, Int, Int) -> Array t;

List:length     :: List t -> Int;
List:ref        :: (List t,Int) -> t;
List:build      :: (Int, Int -> t) -> List t;


// Rewindow:

// Due to polymorphism/metaprogramming problems I may need to move towards this:
fun make_nullseg() { nullseg }

/*===============================================================================*/

fun println(s) {
  print(s);
  print("\n");
};


//fun assert(b)      if not(b)    then wserror("Assert failed.");
//fun asserteq(a,b)  if not(a==b) then wserror("Assert failed: "++ a ++" not equal "++ b);

// Named asserts:
fun assert(s,bool)   if not(bool) then wserror("Assert failed in '"++s++"' ");
fun assert_eq(s,a,b) if not(a==b) then wserror("Assert failed in '"++s++"' : "++ a ++" not equal "++ b);

/*===============================================================================*/

// I haven't renamed them internally yet, but we've obviously become a
// very impure language, so there's no reason to have the effectful
// Hashtable update segregated to a different name.
HashTable:set_pure = HashTable:set;
HashTable:rem_pure = HashTable:rem;
HashTable:set      = HashTable:set_BANG;
HashTable:rem      = HashTable:rem_BANG;

// This fills out the String namespace for consistency:
fun String:append(a,b) a++b

// Similarly, flesh out the conversion functions:
// Array:fromList   = List:toArray
List:fromArray   = Array:toList
Array:toString   = String:fromArray
Array:fromString = String:toArray
List:toString    = String:fromList
List:fromString  = String:toList

// This is a name change for consistent casing:
List:isNull = List:is_null
//foreignSource = foreign_source
// inline_C gnuplot_array etc...

String:explode = String:toList
String:implode = String:fromList

namespace Array {
  // Should maybe call "blockcopy", but that's less fun.
  // Like memcpy... might want to make this primitive so it can call memcpy.
  // TODO: add some defense!!  
  // [2007.12.10] TODO: unroll this loop manually!?
  fun blit(dst, dstpos, src, srcpos, len) {
    for i = 0 to len - 1 {
      dst[i+dstpos] := src[srcpos+i];      
    }
  }

  // Append just two arrays.  
  fun append(aa,bb) {
    // Don't know if it's better to blit here, or just use Array:build.
    len1 = length(aa);
    len2 = length(bb);
    newarr = makeUNSAFE(len1 + len2);
    blit(newarr, 0, aa, 0, len1);
    blit(newarr, len1, bb, 0, len2);
    newarr
  }
  
  // Extract a sub-array.
  // This could be more efficient if it were implemented as a primitive.
  // You could do bounds check once, etc.
  fun sub(arr, pos, len)
     build(len, fun(i) arr[pos+i])
  
}

namespace List {
  fun make(n, x) {
    ptr = Mutable:ref([]);
    for i = 1 to n {
      ptr := x ::: ptr;
    };
    ptr
  }
  fun length(ls) {
    using List; using Mutable;
    count :: Ref Int = ref$ 0;
    ptr   = ref$ ls;
    while ptr != [] {
      ptr := ptr.tail;
      count += 1;
    };
    count
  }
  fun ref(ls, ind) {
    using List; using Mutable;
    i   = ref(0);
    ptr = ref(ls);
    while i < ind {
      i += 1;
      ptr := tail(ptr);
    };
    head(ptr);
  }
  fun reverse(ls) {
    using List; using Mutable; 
    ptr = ref(ls);
    acc = ref([]);
    while not(is_null(ptr)) {
      acc := head(ptr) ::: acc;
      ptr := tail(ptr);
    };
    acc;
  }
  fun append(ls1, ls2) {
    using List; using Mutable; 
    copy = ref(List:reverse(ls1));
    acc  = ref(ls2);
    while not(is_null(copy)) {    
      acc := head(copy) ::: acc;
      copy := tail(copy);
    };
    acc;
  }  
  fun map(fn, ls) {
    using List; using Mutable;
    ptr = ref$ ls;
    acc = ref$ [];
    while ptr != [] {
      acc := fn(ptr.head) ::: acc;
      ptr := ptr.tail;
    };
    reverse(acc);
  }
  fun build(len, fn) {
    using List; using Mutable;
    acc = ref$ [];
    i   = ref$ len;
    while i > 0 {
      i -= 1;
      acc := fn(i) ::: acc;
    };
    acc
  }
  fun fold(fn, zer, ls) {
    using List; using Mutable;
    acc = ref$ zer;
    ptr = ref$ ls;
    while ptr != [] {
      acc := fn(acc, ptr.head);
      ptr := ptr.tail;
    };
    acc
  }
}

// Some aliases for the "prelude":

head   = List:head;
tail   = List:tail;
map    = List:map;
append = List:append;

// No reason to add a primitive for this.  It is defined in wsc2.h
lshiftC :: (Char,Int)  -> Char = foreign("ws_lshiftC", []);
lorC    :: (Char,Char) -> Char = foreign("ws_lorC", []);

wsexit :: (Int) -> a = foreign("wsexit_fun", []);

error = wserror;
exit = wsexit;

// HACK: A stub so that these variables are not unbound

/* namespace TOS { */
/*   //fun sensor_uint16(name, rate) iterate _ in timer(1) { wserror("sensor_uint16 not implemented except in wstiny") }; */
/*   //fun led0Toggle() wserror("led0Toggle not implemented except in wstiny"); */
/*   //fun led1Toggle() wserror("led1Toggle not implemented except in wstiny"); */
/*   //fun led2Toggle() wserror("led2Toggle not implemented except in wstiny"); */
/*   fun sensor_uint16(name, rate) iterate _ in timer(0.01) { }; */
/*   fun led0Toggle() {} */
/*   fun led1Toggle() {} */
/*   fun led2Toggle() {} */
/* } */

