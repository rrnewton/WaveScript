
include "matrix3D.ws"
type Array3D t = Matrix3D t; 
Array3D:get          = Matrix3D:get
Array3D:set          = Matrix3D:set
Array3D:null         = Matrix3D:null
Array3D:fill         = Matrix3D:fill
Array3D:dims         = Matrix3D:dims;
Array3D:make         = Matrix3D:create
Array3D:build        = Matrix3D:build
Array3D:foreach2     = Matrix3D:foreach2
Array3D:fold         = Matrix3D:fold
Array3D:fold2        = Matrix3D:fold2
Array3D:map_inplace  = Matrix3D:map_inplace
Array3D:map_inplace2 = Matrix3D:map2_inplace

fun Array3D:iter(mat, fn) {
  let (r,c,h) = Matrix3D:dims(mat);
  for i = 0 to r-1 {
  for j = 0 to c-1 {
  for k = 0 to h-1 {
    fn(i,j,k)
  }}}}

