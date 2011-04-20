package reuters;

import java.io.IOException;
import java.io.OutputStream;
import java.util.Vector;

public class NWPath {
	Vector<NWLink> path;
	double latency;
		
	public NWPath(double latency) {
		this.path = new Vector<NWLink>();
		this.latency = latency;
	}
	
	NWPath(NWLink e){ 
		// add the edge
		this.path = new Vector<NWLink>();
		this.path.addElement(e);
		this.latency = e.resource().latency;
	}
	
	public double latency() { return latency; }
	public Vector<NWLink> path() { return path; }
	public NWLink link(int index) { return path.get(index); }
	
	// just insert the edge, no latency update
	public void insertedge(NWLink nwl) { path.add(nwl); }
	
	public void print(OutputStream op) {
		try{
			String s = "Length: " + path.size() + ", latency: " + latency;
			op.write(s.getBytes());			
			for(int i=0; i<path.size(); i++) { path.get(i).print(op); }
			op.write('\n');			
			
		} catch(IOException e) { e.printStackTrace(); }
	}
}
