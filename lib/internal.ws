
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

