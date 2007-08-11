
include "matrix.ws";
include "matrix_extras.ws";

// Uses "doas" that we get from the network.

fun normalize(x) x 
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
clustered = iterate x in doas {
  emit [normalize(x)];
}


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

FP_NAN = 0.0 / 0.0;

//create the plot 'canvas' - a 2d array where each pixel is a likelihood
fun doa_fuse(axes, (c_scale, angle_num), nodepos_arr, normdoas_ls) {
  let (x_min, x_max, y_min, y_max) = axes;
  if (y_max - y_min) != (x_max - x_min)) then wserror("not a square");
  assert("Won't do sub-centimeter", c_scale > 1);

  x_width = x_max - x_min;
  y_width = y_max - y_min;
  x_size = x_width / c_scale;
  y_size = y_width / c_scale;

  // allocate memory for the 2d array
  output = Matrix:create(x_size, y_size);

  // These are for...?
  x_c = Array:makeUNSAFE(x_size);
  y_c = Array:makeUNSAFE(y_size);
 
  // Fill in each point in the map:
  // Should use Matrix:map_inplace...
  for i = 0 to y_size - 1 {
    for j = 0 to x_size - 1 {

      // Coordinates in centimeter space:
      c_x = (j`i2f + 0.5) / x_size * x_width + x_min;
      c_y = (i`i2f + 0.5) / y_size * y_width + y_min;
          
      //      x_c[j] = x;
      //      y_c[i] = y;

      // Trace out from each node:
      // Should use List:foldi
      List:foreachi(
        fun(k, normdoas) {
	  let (x,y,yaw) = nodepos_arr[k];
	  nr = yaw * CONST_PI / 180`gint; // node rotation	       
	  theta = atan2(c_y - y, c_x - x);
	  assert("theta shuold not be NAN!", theta != FP_NAN);
	
	  // Convert to degrees:
	  r_th_deg = floor( modF((2.0 * CONST_PI + theta - nr) * 180.0 / CONST_PI, 360.0) );

	  _deg = f2i$ floor(r_th_deg / (360.0 / angle_num));

	  // modF not necessary:
	  //pos2 = if pos < 0 then modF(pos + 360, 360) else pos;
	  dir = if _deg < 0 then _deg + 360 else _deg; 

	  // Access the likelihood vector for that node in the correct direction.
	  output[i][j] := output[i][j] + normdoas[dir];
	  
        },
	normdoas_ls);
    }; // end j for-loop
  }; // end i for-loop
  
  // Now output is all filled up, get back out the maxes.
  let (max_val, max_i, max_j) = 
   Matrix:foldi(
    fun(i,j, acc, elm) {
      let (winner,wini,winj) = acc;
      if max(elm, winner) == elm
      then (elm,i,j)
      else tup
    }, 
    (output[0][0],0,0)
     output);
  
  

  /*  
  double *x_c;
  double *y_c;
  int i,j,k,l,x_size,y_size;
  aml_result_t *aml_results_norm; // normalised aml results (used to be aml_res_t)
  double aml_total; // used to normalise the data (the running total)
  double max_val = 0;
  double min_val = 0;
  double max_x = 0;
  double max_y = 0;
  */


}
