
include "matrix.ws";
include "matrix_extras.ws";

// Uses "doas" that we get from the network.

fun normalize(x) "TODO"
/*
    // calculate normalised J (AML vector) values
    for (i=0; i < args->coord_length; i++) { // used to use args->aml_length
      // be sneaky here, and copy the detection times and node id at this point
      aml_results_norm[i].node_id = args->coords[i].node;
      aml_results_norm[i].det_times = args->coords[i].last_aml.det_times;

      aml_total = 0; // reset this each iteration

      // calculate the total
      for (j=0;j<360;j++) {
	//aml_total += args->aml_results[i].lhood_vector[j];
	aml_total += args->coords[i].last_aml.lhood_vector[j];
      }

      // then normalise based on the total we just made
      for (j=0;j<360;j++) {
	aml_results_norm[i].lhood_vector[j] = args->coords[i].last_aml.lhood_vector[j]/aml_total;
      }
    }

 */

// First we temporally cluster AML messages to get sets that we can project on a plane.
/*
clustered = iterate x in doas {
  emit [normalize(x)];
}
*/

/*
// represents a node in a node list
typedef struct aml_node_entry {
  node_id_t node;
  float coord[3];
  float rpy[3];
  aml_result_t last_aml;
  unsigned char stale;
} aml_node_entry_t;

// represents arguments to a doa fuse calculation
typedef struct aml_fuse_args {
  int c_scale;
  aml_plot_axes_t axes;
  aml_node_entry_t *coords; // node coordinates and aml results
  int coord_length;
  // results are included in node_entry_t's now
  //  aml_result_t *aml_results;
  //  int aml_length;
  int angle_num;
} aml_fuse_args_t;


*/

angle_num = 360.0

FP_NAN = 0.0 / 0.0;
fun floor(f) intToFloat(floatToInt(f))
fun ceiling(f) roundF(f+0.5);

/**************************************************************/

// calculate normalised J (AML vector) values
fun normalize_doas(doas) {
  total = Array:fold((+), 0.0, doas);
  Array:map((/ total), doas);  
}

/**************************************************************/

type AxesBounds = (Float * Float * Float * Float);
type NodeRecord = ((Int * Float * Float * Float) * Array Float);
type Settings   = (AxesBounds * (Float * Float));
type Converter  = Int -> Float; // Coordinate converter.
// Xbound, Ybound, and conversion procs.
type CoordSystem = (Float * Float * Converter * Converter);

// Manifest our coordinate system in the form of conversion procedures.
coord_converters :: (AxesBounds, Float) -> CoordSystem;
fun coord_converters(axes, grid_scale) {
  let (x_min, x_max, y_min, y_max) = axes;
  //if (y_max - y_min) != (x_max - x_min) then wserror("not a square");
  //assert("Won't do sub-centimeter", grid_scale > 1.0);
  {

  // Our original coordinate system is centimeters.
  x_width = x_max - x_min;
  y_width = y_max - y_min;

  // Our new coordinate system is called "chunks" for lack of a better name.
  x_chunks = x_width / grid_scale;
  y_chunks = y_width / grid_scale;

  // Return x/y conversion functions.
  (x_chunks, y_chunks,
   fun (n) (n`i2f + 0.5) / x_chunks * x_width + x_min,
   fun (n) (n`i2f + 0.5) / y_chunks * y_width + y_min)
  }
}

//create the plot 'canvas' - a 2d array where each pixel is a likelihood
doa_fuse :: (CoordSystem, List NodeRecord) -> Matrix Float;
fun doa_fuse((xdim, ydim, xchunks_to_cm, ychunks_to_cm), noderecords) {
  xd = xdim`ceiling`f2i;
  yd = ydim`ceiling`f2i;
  println("Starting DOA fuse.  Gridsize "++xd++" x "++yd);

  // Build the likelihood map:
  Matrix:build(xd,yd,
    fun(i,j) {
      // Coordinates in centimeter space:
      c_x :: Float = xchunks_to_cm(j);
      c_y :: Float = ychunks_to_cm(i);

      // Trace out from each node:
      // Should use List:foldi
      List:foldi(
	// Each node record contains location/orientation as well as doas likelihood vector:
        fun(k, sum, ((id,x,y,yaw), doavec)) {

	  nr = yaw * const_PI / 180.0; // node rotation	       
	  theta = atan2(c_y - y, c_x - x);
	  assert("theta shuold not be NAN!", theta != FP_NAN);
	
	  // Convert to degrees:
	  r_th_deg = floor( modF((2.0 * const_PI + theta - nr) * 180.0 / const_PI, 360.0) );
	  _deg = f2i$ floor(r_th_deg / (360.0 / angle_num));

	  // modF not necessary:
	  //pos2 = if pos < 0 then modF(pos + 360, 360) else pos;
	  dir = if _deg < 0 then _deg + 360 else _deg; 

	  // Access the likelihood vector for that node in appropriate direction.
	  // Add it into our total for this grid square:
	  sum + doavec[dir]
        },
	0.0, noderecords)})
}


//

fun getmax(heatmap, convx, convy) {

  let (max_val, max_i, max_j) = 
   Matrix:foldi(
    fun(i,j, acc, elm) {
      let (winner,wini,winj) = acc;
      if max(elm, winner) == elm
      then (elm,i,j)
      else acc
    },
    (Matrix:get(heatmap,0,0), 0, 0), 
    heatmap);

  // Return the maximum and it's location (in cm):
  (max_val, convx(max_j), convy(max_i))
}
