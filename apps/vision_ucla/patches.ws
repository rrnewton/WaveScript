
//include "matrix-rowmajor.ws"

//====================================================================================================
//   Functions for decomposing images into streams of patches.
//====================================================================================================

// A patch is a piece of a matrix together with metadata to tell us where it came from.
// A patch includes:
//  (1) a matrix slice
//  (2) patch origin on original matrix
//  (3) original image dimensions
type Patch t = (Matrix t * (Int * Int) * (Int * Int));

//stream_patches :: Stream Image -> Stream Patch;

//regroup_images :: Stream Patch -> Stream Image;

// Cut out a piece of an image.
cut_patch :: (Matrix t, Int, Int, Int, Int) -> Patch t;
fun cut_patch(mat, x,y, wid, hght) {
  (Matrix:submatrix(mat,x,y,wid,hght), (x,y), Matrix:dims(mat));
}

// Would be nice to block-copy:
fun Matrix:blit_patch(dst, (mat, (x,y), _)) {
  Matrix:foreachi(fun(i,j,val) {
    Matrix:set(dst, i+x, j+y, val);
  }, mat);
}

/* 
 *  This function creates X*Y workers, each of which handles a region
 *  of the input image.  The patch_transform is applied to each patch,
 *  and also may maintain state between invocation, so long as that
 *  state is encapsulated in one mutable value and passed as an
 *  argument to the transform.
 *
 *  This assumes the 'overlap' is the same in both dimensions, it
 *  could be different in x and y.
 */
make_patch_kernel :: (Int, Int, Int, ((st, Patch a) -> Patch b), (Patch a) -> st) -> 
                      Stream (Matrix a) -> Stream (Matrix b);
fun make_patch_kernel(iworkers, jworkers, overlap, patch_transform, init_state)
 fun (images)
 {
  total = iworkers * jworkers;
  using Matrix;
  patches = iterate mat in images {
    let (rows,cols) = mat.dims;
    //println("  Processing new image with dimensions: " ++ mat.dims);
    iwid = rows / iworkers;
    jwid = cols / jworkers;
    assert_eq("make_patch_kernel: evenly divides rows", rows, iwid * iworkers);
    assert_eq("make_patch_kernel: evenly divides cols", cols, jwid * jworkers);

    for i = 0 to iworkers-1 {
    for j = 0 to jworkers-1 {
      //println("  Cutting out patch "++i++" "++j);
      desiredi = i*iwid - overlap;
      desiredj = j*jwid - overlap;           
      desiredsz1 = iwid + 2*overlap;
      desiredsz2 = jwid + 2*overlap;
      origi = if desiredi < 0 then { desiredsz1 -= 1; 0 } else desiredi; 
      origj = if desiredj < 0 then { desiredsz2 -= 1; 0 } else desiredj;
      size1 = min(rows - origi, desiredsz1); 
      size2 = min(cols - origj, desiredsz2);
      emit (i,j , cut_patch(mat, origi, origj, size1, size2));
    }}
  };
  
  // round robin patchstream to the workers
  workerstreams = [];
  for i = 0 to iworkers-1 {
  for j = 0 to jworkers-1 {  
    filtered = iterate (_i,_j,pat) in patches {
      if i == _i && j == _j
      then emit pat;
    };
    worker = iterate pat in filtered {
      //state { s = init_state(i,j) }
      state { s = Array:null }
      let (mat, (_i,_j), origdims) = pat;
      let (r,c) = mat.dims;
      // Moving the state initialization to runtime:
      // init_state is only passed the coordinates of the patch proper, NOT the extra halo around it.
      if s == Array:null then s := Array:make(1, init_state(pat));
      //if s == Array:null then s := Array:make(1, init_state());

      //println("  Worker "++ (i,j) ++" processing patch... dims "++ mat.dims  ++" size "++ Array:length(Matrix:toArray(mat)));
      emit patch_transform(s[0], pat);
    };
    workerstreams := worker ::: workerstreams;
  }};
    
  allworkers = List:fold1(merge, workerstreams);

  // Now, this must take a stream of patches and stitch them together.
  // We assume that each worker produces exactly one output patch.
  // These output patches should not overlap.
  assembled = iterate pat in allworkers {
    state { assembly = Matrix:null; //create(0,0,0);
            pieces   = 0	    
          }
    let (mat, (i,j), (sz1, sz2)) = pat;
    if assembly.dims != (sz1,sz2) then assembly := create(sz1,sz2, get(mat,0,0));
    
    let (psz1, psz2) = mat.dims;
    //println("  Assembling: piece # "++ pieces ++" blitting patch at "++i++" "++j);
    Matrix:blit_patch(assembly, pat);
    pieces += 1;
    if pieces == total then {
      emit assembly; 
      pieces := 0;
      // ASSUMPTION: we don't bother clearing or reallocating assembly
      // itself unless we have to. (Unless the img changes size).
      // TEMP:
      //Matrix:fill(assembly, 0);
      // CURRENTLY REALLOCATING THIS EVERY TIME BECAUSE EMIT DOESN'T SAFELY COPY YET (e.g. under Scheme backend)
      assembly := Matrix:null; // assembly := create(sz1,sz2,0);
    }
  };
  assembled
 }

//====================================================================================================
//   Peforming transforms on each element of a matrix (but with a peek at the neighborhood).
//====================================================================================================

/* This function builds on top of the make_patch_kernel interface,
 * allowing us to focus on the per-pixel tranfsorm, with the
 * additional privilege of being able to query the neighborhood around
 * the pixel.
 * 
 * Currently this uses the REFLECT policy for all out-of-bounds
 * accesses.  It could however, use different policies, such as a
 * TORUS, or filling in a CONSTANT for the out-of-bounds regions.
 */

//pixel_transform_with_neighborhood  :: (Int, Int, Int, (st, a, (Int, Int) -> a) -> b, (Int, Int) -> st) -> 
//                                      Stream (Matrix a) -> Stream (Matrix b);

// This variant of the function has a complex interface.  One of the
// reasons its complex is that it has additional hook to store
// per-worker state as well as per-pixel state.
fun pixel_transform_with_neighborhood_and_patch_hooks
    (iworkers, jworkers, nbrhood_size, pixel_transform, init_state, patch_hook, patch_state) {
 using Matrix;

 // Takes the global coordinates of the patch.
 // [2008.08.28] TEMPORARY: takes the first patch itself.
 fun patch_init_state(pat) {
   let (mat, (g_i,g_j), (sz1, sz2)) = pat;
   //println(" -=- Initializing patch, global coords "++(g_i,g_j));
   //Array:build(r*c, fun(n) init_state(g_i,0, ))
   // Build a matrix of state, one state entry per pixel:

   base_size1 = sz1 / iworkers;
   base_size2 = sz2 / jworkers;
   expected_size1 = base_size1 + 2*overlap;
   expected_size2 = base_size2 + 2*overlap;
   offseti = overlap;
   offsetj = overlap;
   let (r,c) = mat.dims;
   if expected_size1 != r && g_i == 0  then offseti -= expected_size1 - r; 
   if expected_size2 != c && g_j == 0  then offsetj -= expected_size2 - c; 	 

   (patch_state(pat), 
    Matrix:build(base_size1,base_size2, fun(i,j) init_state(g_i+i+offseti, g_j+j+offsetj)))
 };

 fun patch_transform((patch_st,pix_st), pat) {    
     patch_hook(patch_st, pat);
     let (mat, (g_i,g_j), (sz1, sz2)) = pat;

     // The size expected if we don't have some cropped off (because of borders):
     base_size1 = sz1 / iworkers;
     base_size2 = sz2 / jworkers;

     expected_size1 = base_size1 + 2*overlap;
     expected_size2 = base_size2 + 2*overlap;

     //println(" --- Processing patch rooted at "++(g_i,g_j)++" with base size "++(base_size1, base_size2)++
     //        " expected total size "++ (expected_size1, expected_size2)++ " and received "++mat.dims);

     let (r,c) = mat.dims;
    
     fun neighborhood_access(i,j) fun (di, dj) get(mat, i+di, j+dj);    
     fun neighborhood_access_reflected(centeri, centerj)
     fun (di,dj) {
       i = centeri + di;
       j = centerj + dj;
       i2 = if i < 0  then -1 - i else 
            if i >= r then 2*r - i - 1 else i;
       j2 = if j < 0  then -1 - j else 
            if j >= c then 2*c - j - 1 else j;
       //println("   Reflected access to pos "++(i,j) ++" bounds "++(r,c)++" reflected "++(i2,j2));
       get(mat, i2, j2);
     };
     
     offseti = overlap;
     offsetj = overlap;

     // Adjust our offset if the overlap goes out of bounds on the left/top
     // ASSUMPTION! A tile cannot be off the left & right or top & bottom borders simultaneously!
     if expected_size1 != r && g_i == 0  then offseti -= expected_size1 - r; 
     if expected_size2 != c && g_j == 0  then offsetj -= expected_size2 - c; 	 

     mat2 = 
       build(base_size1, base_size2, fun(i,j) {
         //ind = i * base_size2 + j;

	 _i = i + offseti; 
	 _j = j + offsetj;
	 px  = get(mat, _i, _j);

         //println("* Adjusted position, from "++(i+overlap,j+overlap)++" to "++ (_i,_j));

	 // If this patch doesn't borders then we can blast away without bounds checks.
	 // BUT, we should verify that this has a performance benefit.. no preemature optimization.

	 //if r == expected_size1 && c == expected_size2
	 //then pixel_transform(st[ind], px, neighborhood_access(_i,_j,))
	 //else 
	 pixel_transform(patch_st, Matrix:get(pix_st,i,j), px, neighborhood_access_reflected(_i,_j));
       });

     (mat2, (g_i + offseti, g_j + offsetj), (sz1,sz2))
     //pixel_transform(st, px, neighborhood_access);
 };

 overlap = nbrhood_size;
 make_patch_kernel(iworkers, jworkers, overlap, patch_transform, patch_init_state);
}

// This simplified interface doesn't bother with the patch-level hooks:
fun pixel_transform_with_neighborhood(iworkers, jworkers, nbrhood_size, pixel_transform, init_state) {
  pixel_transform_with_neighborhood_and_patch_hooks
    (iworkers, jworkers, nbrhood_size, fun(_,st,px,nbr) pixel_transform(st,px,nbr), init_state, fun(_,_) {}, fun(_) ());
}

// This even more simplified interface doesn't allow access to the neighborhood aroudn a pixel.
fun pixel_transform(iworkers, jworkers, pixel_transform, init_state) {
  pixel_transform_with_neighborhood_and_patch_hooks
    (iworkers, jworkers, 0, fun(_,st,px,_) pixel_transform(st,px), init_state, fun(_,_) {}, fun(_) ());
}
