package reuters;

import java.io.IOException;
import java.io.OutputStream;

public class NWLink {
	
	int start, end;	
	NWLinkResource resource;
	
	NWLink(int start, int end, NWLinkResource resource) { 
		this.start = start;
		this.end = end;
		this.resource = resource;
	}
	
	public NWLinkResource resource() { return resource; }
	
	public void print(OutputStream op) {
		try {
			op.write(("["+ String.valueOf(start) + "->" + 
					String.valueOf(end) + ", " + resource.toString() + "]").getBytes());			
		}catch(IOException e) { e.printStackTrace(); }
	}
}
