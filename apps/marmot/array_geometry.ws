

/* define array geometry */
sensor_list = [[ 0.04,-0.04,-0.04],
	       [ 0.04, 0.04, 0.04],
	       [-0.04, 0.04,-0.04],
	       [-0.04,-0.04, 0.04]]

micgeometry = Matrix:Float:fromList2d(sensor_list);
