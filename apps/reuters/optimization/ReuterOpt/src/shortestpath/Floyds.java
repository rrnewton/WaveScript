package shortestpath;

import reuters.*;

public class Floyds {
	public static int NOTCONNECTED = -1; 
	
	public static NWMatrix FloydsAlgorithm (NWMatrix g) {
		int n = g.getDim ();
		
		double[][] distance = new double[n][n];		
		for(int v=0; v<n; v++)
		    for(int w=0; w<n; w++) {
		    	//if(v == w) distance[v][w] = 0;    // the same node does not have 
		    	distance[v][w] = Double.MAX_VALUE;
		    }
		
		int[][] path = new int[n][n];
		for(int v=0; v<n; v++)
			for(int w=0; w<n; w++) {
				//if(v == w) path[v][w] = v;
				//else path[v][w] = NOTCONNECTED;
				path[v][w] = NOTCONNECTED;
			}				

		// setup distance and path information based on the matrix, all are directed edge link
		for(int v=0; v<n; v++)
			for(int w=0; w<n; w++) {
				if(g.get(v, w) != null) {
					distance[v][w] = g.get(v, w).getPath(0).latency();
					path[v][w] = v;		// record the starting point if linked directly
				}
			}
		

		for (int i=0; i<n; i++)
		    for (int v=0; v<n; v++)
			for (int w=0; w<n; w++) {
			    if (distance[v][i] < Double.MAX_VALUE && distance [i][w] < Double.MAX_VALUE) { 
			    	double d = distance[v][i] + distance[i][w];			    
			    	if (distance[v][w] > d) {
			    		distance[v][w] = d;
			    		path[v][w] = i;
			    	}
				}			    
			}

		// generate new results
		NWMatrix matrix = new NWMatrix(n);
		for(int v=0; v<n; v++)
			for(int w=0; w<n; w++) {
				
				if(distance[v][w] < Double.MAX_VALUE && v != w)  {// it is connected
					NWElement ele = null;
					NWPath nwp = new NWPath(distance[v][w]);
					findpath(path, nwp, v, w, g);
					ele = new NWElement(nwp);
					matrix.set(v, w, ele);
				}				
			}
		return matrix;
	}
	
	// find the path
	public static void findpath(int[][] paths, NWPath path, int v, int w, NWMatrix g) {
		
		if(paths[v][w] == v) { path.insertedge(g.get(v, w).getPath(0).link(0)); return;	}
		findpath(paths, path, v, paths[v][w], g);
		findpath(paths, path, paths[v][w], w, g);
	}
}
