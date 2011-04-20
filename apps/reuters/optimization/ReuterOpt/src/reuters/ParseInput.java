package reuters;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class ParseInput {
	private static ParseInput input = new ParseInput();	
	private static final String DEPLOY = "test1.qopt"; // node file
	private ParseInput() {}

	public static ParseInput getinstance() { return input; }
	
	// operator table
	public static HashMap<String, OPNode<Operator>> OPTable = 
		new HashMap<String, OPNode<Operator>>();	
	public static List<OPNode<Operator>> roots = new ArrayList<OPNode<Operator>>();
	
	// network vertex table
	public static HashMap<String, NWVertex> VSTable = new HashMap<String, NWVertex>();
	public static HashMap<Integer, NWVertex> VITable = new HashMap<Integer, NWVertex>();
	
	// network link resource table
	public static HashMap<Integer, NWLinkResource> LITable = new HashMap<Integer, NWLinkResource>();
	
	// network graph as matrix
	public static NWMatrix network;
	public static NWMatrix networkpath;
	
	public static HashMap<OPNode<Operator>, List<NWVertex>>	PINTable = 
		new HashMap<OPNode<Operator>, List<NWVertex>>();
	
	public static int DIM;				// Number of network node
	public static int LINKNUM;			// Number of linkages
	public static int OPDIM;			// Number of operator node
	public static double LATENCY = 100;
	
	
	// ---------------------------------------------
	//  main function
	public static void main(String args[]) {
		
		ParseInput.getinstance().parseinput();
		network.print(System.out, "all", "all");
		System.out.println("\n");
		networkpath.print(System.out, "all", "all");
		
		System.out.println("OP  ");
		
		for(OPNode<Operator> root : roots) {
			OPTree<Operator> tree = new OPTree<Operator>(root);
			tree.pirnt(System.out);
		}
		
	}

	// parsing input file
	public void parseinput() {		
		try {
			BufferedReader bufRead = new BufferedReader(new FileReader(DEPLOY));
			String line; int count = 0;
		
			// Read the first line
			line = bufRead.readLine(); count++;
		
			// Read through the file
			while(line != null) {
				if (line.startsWith("network")) {
					DIM = dimparse(line);
					network = new NWMatrix(DIM);
				}
				else if (line.startsWith("node"))	nwvertexparse(line);
				else if (line.startsWith("link"))	nwlinkparse(line);
				else if (line.startsWith("query"))	OPDIM = queryparse(line);
				else if (line.startsWith("op"))		opparse(line);
				else if (line.startsWith("edge"))	edgeparse(line);
				else if (line.startsWith("pin"))	pinparse(line);
				//?????? do not have latency passing right now
				line = bufRead.readLine(); count++;
			}
			bufRead.close();
		}catch(IOException e) { e.printStackTrace(); }
		
		// setup number of links
		LINKNUM = LITable.size();
		networkpath = network.setPath();
				
		// search for all the roots
		Iterator<OPNode<Operator>> iter = OPTable.values().iterator();
		while(iter.hasNext()) {
			OPNode<Operator> op = iter.next();
			if (op.isroot()) roots.add(op);
		}
		
	}
	
	// network dimension parsing
	public static int dimparse(String line) {
		String[] dims = line.split(" +");
		return dims.length;
	}
	
	// network vertex parsing
	public static void nwvertexparse(String line) {
		String[] verteice = line.split(" +");
		NWVertexResource r = new NWVertexResource(Double.valueOf(verteice[3]));
		NWVertex v = new NWVertex(verteice[1], r);
		VSTable.put(v.name(), v);
		VITable.put(Integer.valueOf(v.id()), v);
	}
	
	// network link parsing
	public static void nwlinkparse(String line) {
		String[] links = line.split(" +");
		
		NWLinkResource r = new NWLinkResource(Double.valueOf(links[3]),
				Double.valueOf(links[5]));
		LITable.put(Integer.valueOf(r.id()), r);
		
		int startid = VSTable.get(links[1]).id();
		int endid = VSTable.get(links[7]).id();
		
		// Share resources
		NWLink l1 = new NWLink(startid, endid, r);
		NWElement e1 = new NWElement(l1);
		network.set(startid, endid, e1);
		
		NWLink l2 = new NWLink(endid, startid, r);
		NWElement e2 = new NWElement(l2);
		network.set(endid, startid, e2);
	}
	
	// operator dim parsing
	public int queryparse(String line) {
		String[] ops = line.split(" +");
		return ops.length-2;		
	}
	
	// operator parsing
	public static void opparse(String line) {
		String[] ops = line.split(" +");
		//System.out.println(ops[1] + ops[3]);
		Operator op = new Operator(ops[1], Double.valueOf(ops[3]));
		OPNode<Operator> opnode = new OPNode<Operator>(op);
		OPTable.put(ops[1], opnode);
	}
	
	// edge parsing
	public static void edgeparse(String line) {
		String[] edges = line.split(" +");
		OPNode<Operator> child = OPTable.get(edges[1]);
		OPNode<Operator> parent = OPTable.get(edges[5]);
		child.setparent(parent);
		child.op.setbw(Double.valueOf(edges[3]));
		parent.addchild(child);
	}
	
	// pin parsing
	public static void pinparse(String line) {
		String[] pins = line.split(" +");
		OPNode<Operator> op = OPTable.get(pins[1]);
		
		List<NWVertex> list = new ArrayList<NWVertex>();
		for(int i=2; i<pins.length; i++) {
			list.add(VSTable.get(pins[i]));
		}
		
		PINTable.put(op, list);
	}
}
