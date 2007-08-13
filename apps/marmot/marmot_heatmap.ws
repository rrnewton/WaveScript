
include "matrix.ws";
include "matrix_extras.ws";


// ID, X,Y, YAW, and AML result.
type NodeRecord = (Int * Float * Float * Float);
type TaggedAML = (NodeRecord * Array Float);

type AxesBounds = (Float * Float * Float * Float);
type Settings   = (AxesBounds * (Float * Float));
type Converter  = Int -> Float; // Coordinate converter.
// Xbound, Ybound, and conversion procs.
type CoordSystem = (Int * Int * Converter * Converter);


angle_num = 360.0

FP_NAN = 0.0 / 0.0;
fun floor(f) intToFloat(floatToInt(f))
fun ceiling(f) roundF(f+0.5);

/**************************************************************/

// Temporal clustering of real time AML results.
temporal_cluster_amls :: (Stream TaggedAML) -> Stream (List TaggedAML);
fun temporal_cluster_amls(amls) {
  iterate x in union2(amls, timer(2.0)) {
    state { 
      acc = [];
      counter = 0;
    }
    case x {
      Left(arr): {
        acc := arr ::: acc;
	counter := 0;
      }
      Right(_): {
        counter += 1;
	if counter > 1 
	then {
	  if acc != [] then {
            emit List:reverse(acc);
	    acc := [];
	  }
	}
      }
    }
  }
}

/**************************************************************/

// calculate normalised J (AML vector) values
fun normalize_doas(doas) {
  total = Array:fold((+), 0.0, doas);
  Array:map((/ total), doas);  
}

/**************************************************************/

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

  // Our new coordinate system:
  x_pixels = x_width / grid_scale;
  y_pixels = y_width / grid_scale;

  // Return x/y conversion functions.
  (x_pixels`ceiling`f2i, y_pixels`ceiling`f2i,
   fun (n) (n`i2f + 0.5) / x_pixels * x_width + x_min,
   fun (n) (n`i2f + 0.5) / y_pixels * y_width + y_min)
  }
}

/**************************************************************/

//create the plot 'canvas' - a 2d array where each pixel is a likelihood
doa_fuse :: (CoordSystem, List TaggedAML) -> Matrix Float;
fun doa_fuse((xpixels, ypixels, xchunks_to_cm, ychunks_to_cm), noderecords) {
  { let (xmin,xmax,ymin,ymax) = axes;
    println("Starting DOA fuse.  Gridsize "++xpixels++" x "++ypixels++
          "  Axes: ("++xmin`f2i++", "++ymin`f2i++") to ("++xmax`f2i++", "++ymax`f2i++")");
    assert("xmax > xmin", xmax>xmin);
    assert("ymax > ymin", ymax>ymin);
  };

  // Build the likelihood map:
  Matrix:build(xpixels, ypixels,
    fun(u,v) {

      // Coordinates in centimeter space:
      c_x :: Float = xchunks_to_cm(u);
      c_y :: Float = ychunks_to_cm(v);

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
	0.0, noderecords)
  })
}


getmax :: (Matrix Float, CoordSystem) -> (Float * Int * Int);
fun getmax(heatmap, (_,_, convx, convy)) {
  let (max_val, max_x, max_y) = 
   Matrix:foldi(
    fun(x,y, acc, elm) {
      let (winner,winx,winy) = acc;
      if max(elm, winner) == elm
      then (elm,x,y)
      else acc
    },
     (Matrix:get(heatmap,0,0), 0, 0), 
     heatmap);

  // Return the maximum and it's location (in cm):
  (max_val, max_x, max_y)
}

//******************************************************************************//
// Functions for writing an image to disk

colormap = List:toArray$
 [(0,     0,   143),
  (0,     0,   159),
  (0,     0,   175),
  (0,     0,   191),
  (0,     0,   207),
  (0,     0,   223),
  (0,     0,   239),
  (0,     0,   255),
  (0,    16,   255),
  (0,    32,   255),
  (0,    48,   255),
  (0,    64,   255),
  (0,    80,   255),
  (0,    96,   255),
  (0,   112,   255),
  (0,   128,   255),
  (0,   143,   255),
  (0,   159,   255),
  (0,   175,   255),
  (0,   191,   255),
  (0,   207,   255),
  (0,   223,   255),
  (0,   239,   255),
  (0,   255,   255),
  (16,   255,   239),
  (32,   255,   223),
  (48,   255,   207),
  (64,   255,   191),
  (80,   255,   175),
  (96,   255,   159),
  (112,   255,   143),
  (128,   255,   128),
  (143,   255,   112),
  (159,   255,    96),
  (175,   255,    80),
  (191,   255,    64),
  (207,   255,    48),
  (223,   255,    32),
  (239,   255,    16),
  (255,   255,     0),
  (255,   239,     0),
  (255,   223,     0),
  (255,   207,     0),
  (255,   191,     0),
  (255,   175,     0),
  (255,   159,     0),
  (255,   143,     0),
  (255,   128,     0),
  (255,   112,     0),
  (255,    96,     0),
  (255,    80,     0),
  (255,    64,     0),
  (255,    48,     0),
  (255,    32,     0),
  (255,    16,     0),
  (255,     0,     0),
  (239,     0,     0),
  (223,     0,     0),
  (207,     0,     0),
  (191,     0,     0),
  (175,     0,     0),
  (159,     0,     0),
  (143,     0,     0),
  (128,     0,     0)]

// Takes, file, width, height, red-array, green-array, blue-array
c_write_ppm_file 
  = (foreign("write_ppm_file", ["ppm_write.c"]) 
  :: (String, Int, Int, Array Int, Array Int, Array Int) -> Int)


type RGB = (Int * Int * Int);
write_ppm_file :: (String, Matrix RGB) -> Int;
fun write_ppm_file(file, mat) {
  using Matrix;
  //let (wid,hght) = dims(mat);
  //dat = toArray(mat);
  // We store in column-major so we flip it just once on the way out.
  let (hght,wid) = dims(mat);
  dat = toArray(transpose(mat));
  R = Array:map(fun((r,g,b)) r, dat);
  G = Array:map(fun((r,g,b)) g, dat);
  B = Array:map(fun((r,g,b)) b, dat);
  cstr = file ++ String:implode([intToChar(0)]);
  c_write_ppm_file(cstr, wid, hght, R,G,B);
}

colorize_likelihoods :: Matrix Float -> Matrix RGB;
fun colorize_likelihoods(lhoods) {
  using Matrix;
  fst = get(lhoods,0,0);
  mx = fold(max, fst, lhoods);
  mn = fold(min, fst, lhoods);
  //println("Color max/min: "++mx++" / "++mn);
  //  Matrix:map(fun(v) colormap[f2i$ roundF(v / mx * 63.0)], 
  Matrix:map(fun(v) {
          colorind = f2i((v-mn) / (mx - mn) * 63.0);
          //println("ColorInd: "++ colorind);
          colormap[colorind]
	},
	lhoods);
}


fun draw_marmot(pic, center_x, center_y, size) {
  using Matrix;
  //println("\nDRAWING MARMOT AT "++center_x++", "++center_y++"\n");
  let (xmx,ymx) = dims(pic);
  fun draw(x,y,col) {
    //println("   Drawing pixel: "++x++", "++y);
    if x >= 0 && x < xmx && 
       y >= 0 && y < ymx 
    then set(pic, x, y, col)
    //else println("Pixel out of bounds! "++x++", "++y)
  };

  half = size / 2;
  for x = center_x-half to center_x+half {
    for y = center_y-half to center_y+half {
      draw(x,y, (255,255,255))
    }
  };
  for x = center_x-half+1 to center_x+half-1 {
    for y = center_y-half+1 to center_y+half-1 {
      draw(x,y, (0,0,0))
    }
  };
  pic

  /*
  set(pic, i  , j  , (0,0,0));
  set(pic, i  , j+1, (0,0,0));
  set(pic, i+1, j+1, (0,0,0));
  set(pic, i+1, j,   (0,0,0));
  set(pic, i+1, j-1, (0,0,0));
  set(pic, i  , j-1, (0,0,0));
  set(pic, i-1, j-1, (0,0,0));
  set(pic, i-1, j  , (0,0,0));
  set(pic, i-1, j+1, (0,0,0));
  */
}


//******************************************************************************//

