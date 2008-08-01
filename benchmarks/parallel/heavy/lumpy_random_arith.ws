
// This benchmark puts together random arithmetic transforms that perform various amounts of work.
// Everything operators on scalars, nothing needs windows of data (though batching maybe a good idea).

include "stdlib.ws";

using Mutable;

//type TransformAddOn (a, b) = (a -> b) -> (a -> b);
//type Transform (a, b) = (a -> b);

num_boxes = 10;
num_transforms = 10;

//transforms :: Array TransformAddOn(#a,#a);
//transforms :: Array (#a ->#a);
transforms = 
#[
/*    (+ 1),  */
/*    (+ -2),  */
/*    (* 3),  */
/*    (/ 5) */

  fun(f) fun(x) f(x)+1,
  fun(f) fun(x) f(x)-2,
  fun(f) fun(x) f(x)*3,
  fun(f) fun(x) f(x)/5,

  // This puts the existing transform in a loop.
  fun(f) fun(x) {
    acc = ref$ x;
    for i = 1 to 10 { 
      acc := f(acc)
    };
    acc
  },
  
 ]

using Array;


main = {

  // This returns a single aggregate transform.
  //chain_random_transforms :: (Int -> (#a -> #a));
  //chain_random_transforms :: (Int -> TransformAddOn (#a, #a));
  fun chain_random_transforms(n) {
    if n == 0 then fun(n) n
    //else compose(transforms[randomI(transforms.length)],
    else (transforms[randomI(transforms.length)])(
		   chain_random_transforms(n-1))
  };

  fun chain_boxes(n, s) {
    if n == 0 then s
    else {
      fn = chain_random_transforms(num_transforms);
      iterate n in chain_boxes(n-1, s) { emit fn(n) }
    }
  };

  chain_boxes(num_boxes, COUNTUP(10));
}

