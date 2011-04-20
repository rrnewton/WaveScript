package reuters;

import java.io.IOException;
import java.io.OutputStream;
import java.util.*;

import shortestpath.Floyds;

public class NWMatrix {
	ArrayList<NWElement> matrix;
	int dim;
	
	// construct
	public NWMatrix(int dim){ 
		this.dim = dim;
		matrix = new ArrayList<NWElement>();
		
		// initialize the matrix
		for(int i=0; i<dim*dim; i++)
			matrix.add(null);
	}
	
	
	public int getDim() { return dim; }
	public void set(int i, int j, NWElement ob) { matrix.set(i*dim+j, ob); }
	public NWElement get(int i, int j) { return matrix.get(i*dim+j); }
	public NWElement get(int index) { return matrix.get(index); }
	//public boolean isconnected(int i, int j) { return get(i, j) != null ; }
	
	// automatically set the path between each node based on direct link
	public NWMatrix setPath() {
		return Floyds.FloydsAlgorithm(this);
	}
	
	
	public void print(OutputStream op, String start, String end) {
		NWVertex snode = null, enode = null;
		if (!start.equalsIgnoreCase("all")) {
			snode = ParseInput.VSTable.get(start);
			if (snode == null) { 
				System.err.println("No Start Node named: " + start);
				return;
			}
		}
		
		if(!end.equalsIgnoreCase("all")) {
			enode = ParseInput.VSTable.get(end);
			if(enode == null) {
				System.err.println("No End Node named: " + start);
				return;
			}
		}
		
		for(int i=0; i<ParseInput.DIM*ParseInput.DIM; i++) {			
			if(matrix.get(i) != null) {
				if(start.equalsIgnoreCase("all") && end.equalsIgnoreCase("all")) {
					printIdx(op, i);
					matrix.get(i).print(op);					
				}else if(start.equalsIgnoreCase("all")) {
					if(i%ParseInput.DIM == enode.id()) {
						printIdx(op, i);
						matrix.get(i).print(op);
					}
				}else if(end.equalsIgnoreCase("all")) {
					if(i/ParseInput.DIM == snode.id()) {
						printIdx(op, i);
						matrix.get(i).print(op);
					}
				}else { // start and end have specific requirement
					if(i/ParseInput.DIM == snode.id() && i%ParseInput.DIM == enode.id()) {
						printIdx(op, i);
						matrix.get(i).print(op);
					}
				}
			}
		}
	}
	
	private void printIdx(OutputStream op, int i) {
		try {
			op.write(("(" + String.valueOf(i/ParseInput.DIM) + ", " +
					String.valueOf(i%ParseInput.DIM) + ")\t ").getBytes());
		}catch (IOException e) { e.printStackTrace(); }
	}
	
}
