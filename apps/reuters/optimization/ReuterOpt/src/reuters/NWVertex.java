package reuters;

import java.io.IOException;
import java.io.OutputStream;

public class NWVertex{
	private static int index = 0;		
	int id;     // internal use
	String name;    // node name
	
	NWVertexResource resource;		// cpu capacity
			
	NWVertex(String nm, NWVertexResource resource) { 
		this.name = nm;	this.resource = resource;
		id = index; 
		index++; 
	}
	
	public int id() { return id; }
	public String name() { return name; }
	public NWVertexResource resource() { return resource; }
	
	public void print(OutputStream op) {
		try {
			op.write(("ID: "+ String.valueOf(id) + ";; Name: " + name + ", " + resource.toString() 
					+ "]").getBytes());			
		}catch(IOException e) { e.printStackTrace(); }
	}

}
