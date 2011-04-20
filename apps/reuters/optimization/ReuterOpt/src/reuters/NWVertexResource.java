package reuters;

public class NWVertexResource {
	double cpu;
	
	public NWVertexResource(double cpu) { this.cpu = cpu; }	
	public NWVertexResource(NWVertexResource r) { this.cpu = r.cpu; }	
	
	public double cpu() { return cpu; }
	
	public void add(NWVertexResource r) { if(r != null) cpu += r.cpu; }
	//public void addcpu(double addcpu) { cpu += addcpu; }
	public void setcpu(double setcpu) { cpu = setcpu; }
	
	public String toString() { return "CPU: " + String.valueOf(cpu); }
}
