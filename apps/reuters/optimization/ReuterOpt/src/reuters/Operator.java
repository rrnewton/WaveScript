package reuters;

public class Operator {
	String name;	// operator name
		
	double bw;	// output bandwidth
	double cpu;	// cpu units
	
	public double bw() { return bw; }
	public double cpu() { return cpu; }
	public String name() { return name; }
		
	public Operator(String name, double cpu) {
		this.name = name;	this.cpu = cpu;
	}
		
	public void setbw(double bw) { this.bw = bw; }	
	public String toString() {
		return "Name:" + name + "\tBW: " + String.valueOf(bw) 
		+ ", CPU: " + String.valueOf(cpu);
	}
}
