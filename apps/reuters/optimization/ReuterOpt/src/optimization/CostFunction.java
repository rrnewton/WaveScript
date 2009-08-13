package optimization;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import reuters.*;

public class CostFunction {
	
	/* the mini-cost & resource of the subtree when put the operator opn on node */
	public static CostRecord optcostestimation(OPNode<Operator> opn, NWVertex nwn) {
		CostRecord costrecord = new CostRecord(opn, nwn);
		if(costrecord.optcostestimation()) return costrecord;
		else return null;
	}
	
	/*
	 * Cost is divided into two parts, cpu cost and bw cost
	 * Later, different node may have different cpu price, different edge may have different bw price
	 */
	
	public static double calccost(CostRecord cr, NWPath nwp) {
		return calccost(cr.opn.resource().bw(), nwp.latency()) + cr.resource.totalcost;
	}
	
	public static double calccost(double bw, double latency) { return bw*latency; }	
	public static double calccost(double cpu) { return 0; }
	
	
	/*
	 * children's position selection is dependent with each other,
	 * so need to record 5 highest rank
	
	private static void internodecostestimation(OPNode<Operator> opn, NWVertex nwn) {
		ArrayList<CostRecord> mincostchildrec = new ArrayList<CostRecord>();
		int childNO = opn.children().size();
		double[] mincostchild = new double[childNO];
		
		// initialization
		for(int i=0; i<childNO; i++) 
			mincostchildrec.add(null);
		
		loopchild = 0; cost = Double.MAX_VALUE;
		loopchildren(opn, nwn, mincostchildrec, mincostchild, childNO);
		
		
	}
	
	/*
	 * recursive function, used to loop through all children's possible
	 * position
	 * para: mincostchildrec, children's selected nw node position
	 * 		 mincostchild, corresponding cost
	 
	private static int loopchild = 0;
	private static double cost = Double.MAX_VALUE;	
	private static void loopchildren(OPNode<Operator> opn, NWVertex node, 
			ArrayList<CostRecord> mincostchildrec, double[] mincostchild, int childNO) {
		
		if(loopchild == childNO-1) { 
			// last child, calculate cost
			OPNode<Operator> childop = opn.children().get(loopchild);
			for(CostRecord costrec : CostRecord.SUBTREECOST.get(childop)) {
				
			}			
		}
		else {
			// not the last child
		}	
	}
	*/
	
}
