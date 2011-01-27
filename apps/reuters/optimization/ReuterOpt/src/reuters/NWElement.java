package reuters;

import java.io.IOException;
import java.io.OutputStream;
import java.util.Vector;

public class NWElement {
	private Vector<NWPath> paths = null;
		
	// Used for reading from the file
	NWElement(NWLink e) {		
		paths = new Vector<NWPath>();
		NWPath p = new NWPath(e);
		paths.addElement(p);
	}
	
	public NWElement(NWPath p) {
		paths = new Vector<NWPath>();
		paths.add(p);
	}
		
	public int pathNumber() { return paths.size(); }
	public NWPath getPath(int index) { return paths.get(index); }
	public Vector<NWPath> paths() { return paths; }
		
	// to print
	public void print(OutputStream op){
		for(int i=0; i<paths.size(); i++) {
			try{
				op.write(("Path: " + String.valueOf(i) + " ").getBytes());
				paths.get(i).print(op);
			}catch(IOException e) { e.printStackTrace(); }
		}
	}
}
