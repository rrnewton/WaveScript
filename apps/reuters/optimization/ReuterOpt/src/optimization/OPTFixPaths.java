package optimization;

import reuters.*;

import java.io.IOException;
import java.io.OutputStream;
import java.util.*;

public class OPTFixPaths {
	
	public static void main(String args[]) {
		playoptimal();
	}
	
	public static void playoptimal() {
		ParseInput.getinstance().parseinput();	
		
		// the case when there is only one root
		OPNode<Operator> root = ParseInput.roots.get(0);
		OPTree<Operator> tree = new OPTree<Operator>(root);
		List<OPNode<Operator>> treelist = tree.toList();
			
		// for each operator bottom up
		for(OPNode<Operator> opn : treelist) {
			boolean nodepos = false;
			// put this operator to each possible network node
			List<CostRecord> costlist = new LinkedList<CostRecord>();
			CostRecord.SUBTREECOST.put(opn, costlist);
			
			Iterator<NWVertex> nwnodelist;
			if(ParseInput.PINTable.get(opn) != null) { 
				nwnodelist = ParseInput.PINTable.get(opn).iterator(); 
			} else {
				nwnodelist = ParseInput.VITable.values().iterator();				
			}
			
			
			while(nwnodelist.hasNext()) {
				NWVertex node = nwnodelist.next();
				CostRecord cr = CostFunction.optcostestimation(opn, node);
					if (cr != null)  { costlist.add(cr); nodepos = true; }
			}
			if(!nodepos) {
				System.err.println("unable to find a node pos for the operator ");
				opn.pirnt(System.err);
			}
		}
		
		// select the root position with the least total cost
		CostRecord rcr = null; 
		double totalcost = Double.MAX_VALUE;		
		List<CostRecord> rootcostlist = CostRecord.SUBTREECOST.get(root);
		for(CostRecord cr : rootcostlist) {
			if (totalcost > cr.resource.totalcost) {
				totalcost = cr.resource.totalcost;
				rcr = cr;
			}
		}
		
		print(System.out, rcr);
	}
	
	// print the optimal operator tree allocation
	public static void print(OutputStream ops, CostRecord rcr) {
		try {
			ops.write(("The total cost of the optimal arrangement is:\t" +
					String.valueOf(rcr.resource.totalcost) + 
					"\n The arrangement is as following (from root to child):\n").getBytes());			
		} catch(IOException e) { e.printStackTrace(); }
		
		subprint(ops, rcr);
	}
	
	public static void subprint(OutputStream ops, CostRecord rcr) {
		try {
			ops.write(("Operator\t" + rcr.opn.resource().name() + 
					"is put on Node\t" + rcr.nwn.name() + "\n").getBytes());
			if(rcr.children == null) return;
			for(CostRecord cr : rcr.children) {
				subprint(ops, cr);
			}
		} catch(IOException e) { e.printStackTrace(); }
	}
}
