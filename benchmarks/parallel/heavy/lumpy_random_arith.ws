
// This benchmark puts together random arithmetic transforms that perform various amounts of work.
// Everything operators on scalars, nothing needs windows of data (though batching maybe a good idea).

num_boxes = 100;
num_transforms = 100;

transforms = 
#[
  fun(f) fun(x) f(x)+1, 
  fun(f) fun(x) f(x)-2,
  fun(f) fun(x) f(x)*3,  
  fun(f) fun(x) f(x)/5,
 ]

using Array;

// This returns a single aggregate transform.
fun chain_random_transforms(n) {
  if n == 0 then fun(n) n
  else compose(transforms[randomI(transforms.length)],
               chain_random_transforms(n-1))
}

fun chain_boxes(n) {
  if n == 0 then iterate _ in timer(1000) { emit 1 }
  else { 
    fn = chain_random_transforms(num_transforms);
    iterate n in chain_boxes(n-1) { emit fn(n) }
  }
}

main = chain_boxes(num_boxes);
