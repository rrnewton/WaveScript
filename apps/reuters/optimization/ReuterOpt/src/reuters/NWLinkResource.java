package reuters;

public class NWLinkResource {
	private static int index;
	
	double bw;			// bandwidth
	double latency; 	// latency
	
	int id;
	
	// The real linkage resource
	NWLinkResource(double bw, double latency) {
		this.bw = bw;
		this.latency = latency;
		this.id = index++;
	}
	
	// The linkage resource copy
	public NWLinkResource(NWLinkResource r) {
		this.bw = r.bw;
		this.latency = r.latency;
		this.id = -1;
	}
	
	// The linkage resource copy
	public NWLinkResource(double bw) {
		this.bw = bw;
		this.latency = Double.MAX_VALUE;
		this.id = -1;
	}
	
	public int id() { return id; }
	public double bw() { return bw; }
	public double latency() { return latency; }
	
	// bandwidth is added up, latency is the same for the same linkage
	public void add(NWLinkResource r) { if (r!=null) { bw += r.bw; } }
	public void addbw(double addbw) { bw += addbw; }
	public void setbw(double setbw) { bw = setbw; }
	
	
	public String toString() {
		return "ID: " + String.valueOf(id) + "BW: " + String.valueOf(bw) 
		+ " Latency: " + String.valueOf(latency);
	}
}
