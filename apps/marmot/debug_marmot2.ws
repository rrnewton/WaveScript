include "marmot2.ws";
include "plot.ws";

// rrn: This doesn't do anything different yet.
// But the intention is to inspect some intermediate stream values.

//========================================
// Main query:

/* define array geometry */
sensors = list_to_matrix([[ 0.04,-0.04,-0.04],
			  [ 0.04, 0.04, 0.04],
			  [-0.04, 0.04,-0.04],
			  [-0.04,-0.04, 0.04]]);

// 'synced' is defined in marmot_first_phase.ws
//doas = FarFieldDOAb(synced, sensors);
doas = oneSourceAMLTD(synced, sensors,4096);

BASE <- doas
