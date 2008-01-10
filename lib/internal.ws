
// This is automatically included by all WS compiles.  It contains
// definitions for the "special-rewrite-libfuns".  Basically,
// primitives that are defined externally in a library, but whose
// names are specially recognized so that rewrite-optimizations can
// process them.

// [2007.12.11] Note, this should also include routines that the
// compiler itself depends on (for example, because other primitives
// desugar to use them).

Array:blit      :: (Array t, Int, Array t, Int, Int) -> ();


// Rewindow:



// Should maybe call "blockcopy", but that's less fun.
// Like memcpy... might want to make
// TODO: add some defense!!  
// [2007.12.10] TODO: unroll this loop manually!
fun Array:blit(dst, dstpos, src, srcpos, len) {
  for i = 0 to len - 1 {
    dst[i+dstpos] := src[srcpos+i];      
  }
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
    count = ref$ 0;
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
