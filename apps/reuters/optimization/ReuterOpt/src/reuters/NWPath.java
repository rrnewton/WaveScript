package reuters;

import java.io.IOException;
import java.io.OutputStream;
import java.util.Vector;

public class NWPath {
	Vector<NWLink> path;
	double latency;
		
	NWPath(NWLink e){ 
		// add the edge
		this.path = new Vector<NWLink>();
		this.path.addElement(e);
		this.latency = e.resource().latency;
	}
	
	public double latency() { return latency; }
	public Vector<NWLink> path() { return path; }
	public NWLink link(int index) { return path.get(index); }
	
	public void print(OutputStream op) {
		try{
			String s = "Length: " + path.size() + ", ";
			op.write(s.getBytes());			
			for(int i=0; i<path.size(); i++) { path.get(i).print(op); }
			op.write('\n');			
			
		} catch(IOException e) { e.printStackTrace(); }
	}
}
